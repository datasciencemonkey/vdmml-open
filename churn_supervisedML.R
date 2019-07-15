# Load necessary packages
library('swat')
library('ggplot2')
library('reshape2')
options(cas.print.messages = TRUE)

# Data name
indata <- 'TCHURN'

source("C:/Users/sagang/Google Drive/work/swatR/password.R")

# Hostname, port, username, password
conn <- CAS('dl-viya-cluster-1.dlviyacluster.sashq-r.openstack.sas.com',
            8777,username = usr, password = passwd, protocol = "http")


## Set Caslib to Public
cas.sessionProp.setSessOpt(conn,caslib='Public')
print("Changed the current CASLIB to Public")

# Read in the dataset
fpath <- "C://Users//sagang//OneDrive\ -\ SAS//Documents\\tchurn.csv"
castbl <- cas.read.csv(conn, fpath)



## Check if the caslib changed to Public
#cas.table.caslibInfo(conn)

head(castbl)

#Generate Summary stats
cas.simple.summary(castbl)



## Visualize all numeric data
# Bring data locally
df <- to.casDataFrame(castbl, obs = nrow(castbl))

# Use reshape2's melt to help with data formatting
d <- melt(df[sapply(df, is.numeric)], id.vars=NULL)
ggplot(d, aes(x = value)) + 
    facet_wrap(~variable,scales = 'free_x') + 
    geom_histogram(fill = 'blue', bins = 25) 



## Visualize Missingness
tbl <- cas.simple.distinct(castbl)$Distinct[,c('Column', 'NMiss')]
tbl

tbl$PctMiss <- tbl$NMiss/nrow(castbl)
ggplot(tbl, aes(Column, PctMiss)) +
  geom_col(fill = 'blue') +
  ggtitle('Pct Missing Values') +
  theme(plot.title = element_text(hjust = 0.5))


## Impute Missing Values
cas.dataPreprocess.impute(castbl,
                          methodContinuous = 'MEDIAN',
                          methodNominal    = 'MODE',
                          inputs           = colnames(castbl)[-1],
                          copyAllVars      = TRUE,
                          casOut           = list(name = indata, replace = TRUE)
)


## Split the Data into Training and Validation

loadActionSet(conn, 'sampling')

# Partition the data
cas.sampling.srs(conn,
                 table   = indata,
                 samppct = 30,
                 partind = TRUE,
                 output  = list(casOut = list(name = indata, replace = T), copyVars = 'ALL')
)


## Verify Partition
# Load the fedsql actionset
loadActionSet(conn, 'fedsql')
print("actionset fedSQL loaded")


sql_query <- paste0("SELECT CASE WHEN _PartInd_ = 0 THEN 'Training' ELSE 'Validation' END AS name,_PartInd_, COUNT(*) AS obs 
            FROM Public.",indata, " GROUP BY 1,2;")
# Make sure the partition worked correctly using SQL
cas.fedSql.execDirect(conn, query = sql_query)$`Result Set`



## Set up variable shortcuts
# Get variable info and types
colinfo <- head(cas.table.columnInfo(conn, table = indata)$ColumnInfo, -1)

# My target variable is the first column
target <- colinfo$Column[1]

# For models that can inherently handle missing values (ex: Decision Tree)
inputs <- colinfo$Column[-1]
nominals <- c(target, subset(colinfo, Type == 'varchar')$Column)

# For models that cannot handle missing values (ex: Neural Network)
imp.inputs <- grep('IMP_', inputs, value = T)
imp.nominals <- c(target, grep('IMP_', nominals, value = T))



## MODEL BUILDING

## Decision Tree
# Load the decsion tree actionset
loadActionSet(conn, 'decisionTree')
# Train the decision tree model
cas.decisionTree.dtreeTrain(conn,
    table    = list(name = indata, where = '_PartInd_ = 0'),
    target   = target, 
    inputs   = inputs, 
    nominals = nominals,
    varImp   = TRUE,
    casOut   = list(name = 'dt_model', replace = TRUE)
)

## Random Forest
cas.decisionTree.forestTrain(conn,
    table    = list(name = indata, where = '_PartInd_ = 0'),
    target   = target, 
    inputs   = inputs, 
    nominals = nominals,
    varImp = TRUE,
    casOut   = list(name = 'rf_model', replace = TRUE)
)


## Gradient Boosting
# Train the gradient boosting model
cas.decisionTree.gbtreeTrain(conn,
    table    = list(name = indata, where = '_PartInd_ = 0'),
    target   = target, 
    inputs   = inputs, 
    nominals = nominals,
    varImp = TRUE,
    casOut   = list(name = 'gbt_model', replace = TRUE)
)


## Neural Network
# Load the neuralNet actionset
loadActionSet(conn, 'neuralNet')

# Build a neural network model
cas.neuralNet.annTrain(conn,
    table    = list(name = indata, where = '_PartInd_ = 0'),
    target   = target, 
    inputs   = imp.inputs, 
    nominals = imp.nominals,
    casOut   = list(name = 'nn_model', replace = TRUE)
)


## Score the models
models <- c('dt','rf','gbt','nn')
scores <- c(cas.decisionTree.dtreeScore, cas.decisionTree.forestScore, 
            cas.decisionTree.gbtreeScore, cas.neuralNet.annScore)
names(scores) <- models

# Function to help automate prediction process on new data
score.params <- function(model){return(list(
    object       = defCasTable(conn, indata),
    modelTable   = list(name = paste0(model, '_model')),
    copyVars     = list(target, '_PartInd_'),
    assessonerow = TRUE,
    casOut       = list(name = paste0(model, '_scored'), replace = T)
))}
lapply(models, function(x) {do.call(scores[[x]], score.params(x))})




## Compare Confusion Matrix ##
# Load the percentile actionset for scoring
loadActionSet(conn, 'percentile')

# Useful function for model assessment
assess.model <- function(model){
    cas.percentile.assess(conn,
        table    = list(name = paste0(model,'_scored'), 
                        where = '_PartInd_ = 1'),
        inputs   = paste0('_', model, '_P_           1'),
        response = target,
        event    = '1')
}

model.names <- c('Decision Tree', 'Random Forest', 
                 'Gradient Boosting', 'Neural Network')
roc.df <- data.frame()
for (i in 1:length(models)){
    tmp <- (assess.model(models[i]))$ROCInfo
    tmp$Model <- model.names[i] 
    roc.df <- rbind(roc.df, tmp)
}

# Manipulate the dataframe
compare <- subset(roc.df, CutOff == 0.5)
rownames(compare) <- NULL
compare[,c('Model','TP','FP','FN','TN')]




## Compare Misclassification ##
# Build a dataframe to compare the misclassification rates
compare$Misclassification <- 1 - compare$ACC
miss <- compare[order(compare$Misclassification), c('Model','Misclassification')]
rownames(miss) <- NULL
miss




## ROC Curve Comparison ##
# Add a new column to be used as the ROC curve label
roc.df$Models <- paste(roc.df$Model, round(roc.df$C, 3), sep = ' - ')

# Create the ROC curve
ggplot(data = roc.df[c('FPR', 'Sensitivity', 'Models')], 
       aes(x = as.numeric(FPR), y = as.numeric(Sensitivity), colour = Models)) + 
       geom_line() +
       labs(x = 'False Positive Rate', y = 'True Positive Rate')





## Save the champion model for later use
cas.table.save(conn, table = list(name = 'gbt_model'), name = 'gbt_model', replace = T)


##End Session
cas.session.endSession(conn)

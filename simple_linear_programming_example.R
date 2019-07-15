library('swat')
# library('ggplot2')
# library('reshape2')
options(cas.print.messages = TRUE)

conn <- CAS('dl-viya-cluster-1.dlviyacluster.sashq-r.openstack.sas.com',
            8777,username = usr, password = passwd, protocol = "http")

loadActionSet(conn, 'optimization')

## Set Caslib to Public
cas.sessionProp.setSessOpt(conn,caslib='Public')
print("Changed the current CASLIB to Public")

# Read in the dataset
fpath <- "C://Users//sagang//Desktop//optlp.csv"
castbl <- cas.read.csv(conn, fpath, casOut = list(name="opt_example", replace=TRUE))


# proc cas;
#    loadactionset "optimization";
# action optimization.solveLp result=r status=s /
# data      = {name="example"}
# primalOut = {name="expout" replace=true}
# dualOut   = {name="exdout" replace=true}
# objSense  = "min"
# algorithm = "primal"
# logFreq   = 1;
# run;
# print r.SolutionSummary; run;
# action table.fetch / table = "expout"; run;
# quit;


cas.optimization.solveLp(conn,
                         algorithm="primal",
                         objSense ="min",
                         logFreq = 1,
                         data = list(name="opt_example"),
                         primalOut = list(name="expout", replace = TRUE),
                         dualOut = list(name="exdout", replace=TRUE))
cas.table.fetch(conn, table = list(name = 'expout'))

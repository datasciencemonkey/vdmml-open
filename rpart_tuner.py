def recursive_partitioning_tuner(conn,input_table, inputs, nominals, target, autotune_factory=[]):
    """
    Inputs:
    conn - connection object for CAS
    input_table - CASTable that contains the input data
    inputs - inputs to the model - all inputs
    nominals - variables that are deemed nominal
    target - the variable  that is to be modelled
    autotune_factory - list of recursive partitioning techniques
    """
    autotune_result_summary = {}
    for model in autotune_factory :
        model_type = model.split("_")[1]
        model_options = {
              "table"   : input_table,
               "nominals":  nominals,
              "inputs"  : inputs,
              "target"  : target,
               "varimp":  True,
              "casout"  : dict(name = f"auto_{model_type}_model", replace=True),
             "savestate" :dict(name=f"auto_{model_type}_save",replace=True)
           }
        if model_type == "tree":
            tuning_result = conn.autotune.tuneDecisionTree(
                trainoptions = {i:model_options[i] for i in list(model_options.keys())[:-1]},
                tuneroptions = dict(seed=54321, maxiters=5, objective ="misc")
            )
            autotune_result_summary[model_type] = tuning_result
        
        if model_type == "forest":
            tuning_result = conn.autotune.tuneForest(
                trainoptions = model_options,
                tuneroptions = dict(seed=54321, maxiters=5, objective ="misc")
            )
            autotune_result_summary[model_type] = tuning_result
        if model_type == "gradboost":
            tuning_result = conn.autotune.tuneGradientBoostTree(
                trainoptions = model_options,
                tuneroptions = dict(seed=54321, maxiters=5, objective ="misc")
            )
            autotune_result_summary[model_type] = tuning_result
    return autotune_result_summary




def collect_scored_data(conn, model_tables,scoring_table,copy_vars):
    """
    Inputs:
    conn - connection object for CAS
    input_table - CASTable that contains the input data
    inputs - inputs to the model - all inputs
    nominals - variables that are deemed nominal
    target - the variable  that is to be modelled
    autotune_factory - list of recursive partitioning techniques
    """
    auto_score_results = {}
    for model_table in model_tables:
        table_name_prefix = model_table.split("_")
        model_type = table_name_prefix[1]
        model_score_data = '_'.join([table_name_prefix[0],table_name_prefix[1],"DATA"])
        if model_type.lower() == "gradboost":
            auto_score_results[model_type] = scoring_table.gbtreescore(modeltable=model_table,
                    casout=dict(name = model_score_data,
                                replace=True),
                    copyVars= copy_vars)

            conn.fedsql.execdirect("""create table %s  {options replace=true} as
                        (select *,
                        case when _gbt_predname_=1 then _gbt_predp_ else 1-_gbt_predp_ end as _pred_,
                        case when _gbt_predname_=0 then _gbt_predp_ else 1-_gbt_predp_ end as _pred0_
                        from  %s
                        )
                        """  %(model_score_data,model_score_data))


        if model_type.lower() == "tree":
            auto_score_results[model_type] = scoring_table.dtreescore(modeltable=model_table,
                    casout=dict(name = model_score_data,
                                replace=True),
                    copyVars= copy_vars)

            conn.fedsql.execdirect("""create table %s  {options replace=true} as
                        (select *,
                        case when _dt_predname_=1 then _dt_predp_ else 1-_dt_predp_ end as _pred_,
                        case when _dt_predname_=0 then _dt_predp_ else 1-_dt_predp_ end as _pred0_
                        from  %s
                        )
                        """  %(model_score_data,model_score_data))

            
        if model_type.lower() == "forest":
            auto_score_results[model_type] = scoring_table.forestscore(modeltable=model_table,
                    casout=dict(name = model_score_data,
                                replace=True),
                    copyVars= copy_vars)
            # Calculating stats        
            conn.fedsql.execdirect("""create table %s  {options replace=true} as
                        (select *,
                        case when _rf_predname_=1 then _rf_predp_ else 1-_rf_predp_ end as _pred_,
                        case when _rf_predname_=0 then _rf_predp_ else 1-_rf_predp_ end as _pred0_
                        from  %s
                        )
                        """  %(model_score_data,model_score_data))

    return auto_score_results


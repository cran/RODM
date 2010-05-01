`RODM_create_dt_model` <- function(
#
# This function creates an ODM Decision Tree model. 
#
   database,                     # Database ODBC channel identifier
   data_table_name,              # Database table/view containing the training dataset
   case_id_column_name = NULL,   # Row unique case identifier in data_table_name		
   target_column_name,           # Target column name in data_table_name		
   model_name = "DT_MODEL",      # ODM Model name				  
   auto_data_prep = TRUE,        # Setting to perform automatic data preparation
   gini_impurity_metric = TRUE,  # Use gini impurity metric (false for entropy)
   max_depth = NULL,             # Tree max depth
   minrec_split = NULL,          # Minimum records in a node to split the node
   minpct_split = NULL,          # Minimum % of records in a node to split the node
   minrec_node = NULL,           # Minimum records to form a node
   minpct_node = NULL,           # Minimum % of records to form a node
   retrieve_outputs_to_R = TRUE, # Flag controlling if the outpout results are moved to the R environment 
   leave_model_in_dbms = TRUE,   # Flag controlling if the model is deleted or left in RDBMS               
   sql.log.file = NULL)          # File where to append the log of all the SQL calls made by this function
{
   if (!is.null(sql.log.file)) write(paste("--- SQL calls by ODM function: RODM_create_dt_model ", 
              date(), "---"), file = sql.log.file, append = TRUE, ncolumns = 1000)

   # Store settings in the RDBMS RODM settings table
   DT.settings.table <- data.frame(matrix(c(
       "ALGO_NAME", "ALGO_DECISION_TREE"),
       nrow = 1, ncol=2, byrow=TRUE))
   names(DT.settings.table) <- c("SETTING_NAME", "SETTING_VALUE")
   if (gini_impurity_metric == FALSE) {
       SVM.settings.table <- rbind(SVM.settings.table, 
           data.frame(matrix(c("TREE_IMPURITY_METRIC", "TREE_IMPURITY_ENTROPY"), 
             nrow=1, ncol=2, byrow=TRUE,
             dimnames = list(NULL,c("SETTING_NAME", "SETTING_VALUE")))))
   }
   if (!is.null(max_depth)) {
       DT.settings.table <- rbind(DT.settings.table, 
           data.frame(matrix(c("TREE_TERM_MAX_DEPTH", max_depth),
             nrow=1, ncol=2, byrow=TRUE,
             dimnames = list(NULL,c("SETTING_NAME", "SETTING_VALUE")))))
   }
   if (!is.null(minrec_split)) {
       DT.settings.table <- rbind(DT.settings.table, 
           data.frame(matrix(c("TREE_TERM_MINREC_SPLIT", minrec_split),
             nrow=1, ncol=2, byrow=TRUE,
             dimnames = list(NULL,c("SETTING_NAME", "SETTING_VALUE")))))
   }
   if (!is.null(minpct_split)) {
       DT.settings.table <- rbind(DT.settings.table, 
           data.frame(matrix(c("TREE_TERM_MINPCT_SPLIT", minpct_split),
             nrow=1, ncol=2, byrow=TRUE,
             dimnames = list(NULL,c("SETTING_NAME", "SETTING_VALUE")))))
   }
   if (!is.null(minrec_node)) {
       DT.settings.table <- rbind(DT.settings.table, 
           data.frame(matrix(c("TREE_TERM_MINREC_NODE", minrec_node),
             nrow=1, ncol=2, byrow=TRUE,
             dimnames = list(NULL,c("SETTING_NAME", "SETTING_VALUE")))))
   }
   if (!is.null(minpct_node)) {
       DT.settings.table <- rbind(DT.settings.table, 
           data.frame(matrix(c("TREE_TERM_MINPCT_NODE", minpct_node),
             nrow=1, ncol=2, byrow=TRUE,
             dimnames = list(NULL,c("SETTING_NAME", "SETTING_VALUE")))))
   }
   RODM_store_settings(database, DT.settings.table, auto_data_prep, sql.log.file)

   # Create the ODM Decision Tree classification model, retrieving
   # basic details (settings and attributes) if desired
   dt.list <- RODM_create_model(
     database, model_name, "dbms_data_mining.classification",
     data_table_name, case_id_column_name, target_column_name, 
     retrieve_outputs_to_R, sql.log.file)

   # Retrieve DT-specific details if desired
   if (retrieve_outputs_to_R == TRUE) { 
     query.string <- paste(
            "select to_clob(dbms_data_mining.get_model_details_xml('", 
            model_name, "')) as details from dual", sep="")
     tree_xml <- sqlQuery(database, query = query.string)
     dt.list <- c(dt.list, list("dt.tree_xml" = tree_xml))
   } 

   # Clean up as requested
   if (leave_model_in_dbms == FALSE) RODM_drop_model(database, model_name, sql.log.file)
   
   return(dt.list)
} # End of RODM_create_dt_model


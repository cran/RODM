`RODM_create_nmf_model` <- function(
#
# Create an Non-Negative Matrix Factorization (NMF) model
#
   database,                     # Database ODBC channel identifier
   data_table_name,              # Database table/view containing the training dataset
   case_id_column_name = NULL,   # Name of the column of data_table_frame containing the case id        
   model_name = "NMF_MODEL",     # model name
   auto_data_prep = TRUE,        # Setting to perform automatic data preparation
   num_features = NULL,          # setting that specifies the number of features for a feature selection model.
   conv_tolerance = NULL,        # setting that specifies convergence tolerance for nmf.
   num_iterations = NULL,        # setting that specifies the number of iterations for nmf.
   random_seed = NULL,           # setting that specifies the random seed for nmf.
   retrieve_outputs_to_R = TRUE, # Flag controlling if the output results are moved to the R environment (optional)
   leave_model_in_dbms = TRUE,   # Flag controlling if the model is deleted or left in RDBMS
   sql.log.file = NULL)          # File where to append the log of all the SQL calls made by this function (optional)
{

# Fix this to produce H matrix...  scoring?
# How to handle categoricals?
# Does ADP without reversal ruin H matrix?  NMF doesn't support reversal...

   if (!is.null(sql.log.file)) write(paste("--- SQL calls by ODM function: RODM_create_nmf_model ", 
                       date(), "---"), file = sql.log.file, append = TRUE, ncolumns = 1000)

   # Store settings in the RDBMS RODM settings table
   NMF.settings.table <- data.frame(matrix(c(
       "ALGO_NAME", "ALGO_NONNEGATIVE_MATRIX_FACTOR"),
       nrow = 1, ncol=2, byrow=TRUE))
   names(NMF.settings.table) <- c("SETTING_NAME", "SETTING_VALUE")
   if (!is.null(num_features)) {
     NMF.settings.table <- rbind(NMF.settings.table, 
         data.frame(matrix(c("FEAT_NUM_FEATURES", num_features),
           nrow=1, ncol=2, byrow=TRUE,
           dimnames = list(NULL,c("SETTING_NAME", "SETTING_VALUE")))))
   }
   if (!is.null(conv_tolerance)) {
     NMF.settings.table <- rbind(NMF.settings.table, 
         data.frame(matrix(c("NMFS_CONV_TOLERANCE", conv_tolerance),
           nrow=1, ncol=2, byrow=TRUE,
           dimnames = list(NULL,c("SETTING_NAME", "SETTING_VALUE")))))
   }
   if (!is.null(num_iterations)) {
     NMF.settings.table <- rbind(NMF.settings.table, 
         data.frame(matrix(c("NMFS_NUM_ITERATIONS", num_iterations),
           nrow=1, ncol=2, byrow=TRUE,
           dimnames = list(NULL,c("SETTING_NAME", "SETTING_VALUE")))))
   }
   if (!is.null(num_iterations)) {
     NMF.settings.table <- rbind(NMF.settings.table, 
         data.frame(matrix(c("NMFS_RANDOM_SEED", num_iterations),
           nrow=1, ncol=2, byrow=TRUE,
           dimnames = list(NULL,c("SETTING_NAME", "SETTING_VALUE")))))
   }
   RODM_store_settings(database, NMF.settings.table, auto_data_prep, sql.log.file)

   # Create the ODM Nonnegative Matrix Factorization model, retrieving
   # basic details (settings and attributes) if desired
   nmf.list <- RODM_create_model(
     database, model_name, "dbms_data_mining.feature_extraction",
     data_table_name, case_id_column_name, NULL, 
     retrieve_outputs_to_R, sql.log.file)

   # Retrieve NMF-specific details if desired
   if (retrieve_outputs_to_R == TRUE) { 
     query.string <- paste("SELECT ",
            "FEATURE_ID, ",
            "MAPPED_FEATURE_ID, ",
            "ATTRIBUTE_NAME, ",
            "ATTRIBUTE_SUBNAME, ",
            "ATTRIBUTE_VALUE, ",
            "COEFFICIENT ",
            "FROM table(dbms_data_mining.get_model_details_nmf('",
            model_name, "')) t, table(t.attribute_set) s ",
            "order by 1,2,3,4,5", sep="");
     features <- sqlQuery(database, query = query.string)

   # Extract results
#   query.string <- paste("CREATE TABLE ", db.table.name, " AS  SELECT t.feature_id,  a.attribute_name, a.attribute_value, a.coefficient  FROM TABLE(dbms_data_mining.get_model_details_nmf('", model_name,
#                      "')) t, TABLE(t.attribute_set) a ORDER BY feature_id,attribute_name,attribute_value",
#                        sep="", collapse="")
#   nmf.features <- data.matrix(sqlFetch(database, db.table.name))
#   size <- length(nmf.features[,1])
#   h.rows <- max(nmf.features[, 1])
#   h.cols <- max(nmf.features[, 2])
#   H <- matrix(0, nrow=h.rows, ncol=h.cols)
#   for (i in 1:size) {
#      H[nmf.features[i, 1], nmf.features[i, 2]] <- nmf.features[i, 4]
#   }
      

     if (!is.null(sql.log.file)) write(query.string, file = sql.log.file, append = TRUE, ncolumns = 1000)
     nmf.list <- c(nmf.list, list("nmf.features" = features))
   }

   # Clean up as requested
   if (leave_model_in_dbms == FALSE) RODM_drop_model(database, model_name, sql.log.file)

   return(nmf.list)
} # end of RODM_create_nmf_model

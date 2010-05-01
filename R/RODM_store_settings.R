`RODM_store_settings` <- function(
#
# Store model settings in the ODM settings table
# This is an internal auxilliary function that should not be
# called directly by the end-user.
#
  database,
  rodmset, 
  auto_data_prep,
  sql.log.file = NULL
)
{
   query.string <- "TRUNCATE TABLE RODM_SETTINGS_TABLE"
   if (!is.null(sql.log.file)) write(query.string, file = sql.log.file, append = TRUE, ncolumns = 1000)
   sqlQuery(database, query = query.string)

   if (auto_data_prep == TRUE) {
     if (is.null(rodmset)) {
       rodmset <- data.frame(matrix(c(
         "PREP_AUTO", "ON"),
       nrow = 1, ncol=2, byrow=TRUE))
       names(rodmset) <- c("SETTING_NAME", "SETTING_VALUE")
     } else {
       D <- data.frame(matrix(c("PREP_AUTO", "ON"), nrow=1, ncol=2, byrow=TRUE,
                              dimnames = list(NULL,c("SETTING_NAME", "SETTING_VALUE"))))
       rodmset <- rbind(rodmset, D)
     }
   } else {
     if (is.null(rodmset)) return()
   }

   # Insert by hand (instead of using sqlSave) for performance reasons
   for (i in 1:length(rodmset[,1])) {
     query.string <- paste(
         "INSERT INTO RODM_SETTINGS_TABLE (SETTING_NAME, SETTING_VALUE) ",
         "VALUES ('", rodmset[i,1],
         "','", rodmset[i,2], "')", sep="")
     if (!is.null(sql.log.file)) write(query.string, file = sql.log.file, append = TRUE, ncolumns = 1000)   
     sqlQuery(database, query = query.string)
   }

   return()
} # end of RODM_store_settings

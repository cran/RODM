\name{RODM_create_ai_model}

\alias{RODM_create_ai_model}

\title{Create an Attribute Importance (AI) model
}

\description{
This function creates an Oracle Data Mining Attribute Importance (AI) model. 
}

\usage{
RODM_create_ai_model(database, 
                     data_table_name, 
                     case_id_column_name = NULL, 
                     target_column_name,
                     model_name = "AI_MODEL",
                     auto_data_prep = TRUE,
                     retrieve_outputs_to_R = TRUE,
                     leave_model_in_dbms = TRUE, 
                     sql.log.file = NULL)
}

\arguments{
  \item{database}{Database ODBC channel identifier returned from a call to RODM_open_dbms_connection}
  \item{data_table_name}{Database table/view containing the training dataset.}
  \item{case_id_column_name}{Row unique case identifier in data_table_name.}
  \item{target_column_name}{Target column name in data_table_name.}					
  \item{model_name}{ODM Model name.}
  \item{auto_data_prep}{Setting that specifies whether or not ODM should perform automatic data preparation.}
  \item{retrieve_outputs_to_R}{Flag controlling if the output results are moved to the R environment.}
  \item{leave_model_in_dbms}{Flag controlling if the model is dropped or left in RDBMS.}
  \item{sql.log.file}{File where to append the log of all the SQL calls made by this function.}    
}

\details{Attribute Importance (AI) uses a Minimum Description Length
  (MDL) based algorithm that ranks the relative importance of attributes
  in their ability to contribute to the prediction of a specified target
  attribute. This algorithm can provide insight into the attributes relevance to a
  specified target attribute and can help reduce the number of
  attributes for model building to increase performance and model
  accuracy.

  For more details on the algotithm implementation, parameters settings and 
  characteristics of the ODM function itself consult the following Oracle documents: ODM Concepts, 
  ODM Application Developer's Guide, and Oracle PL/SQL Packages: Data Mining,
  listed in the references below. 
}

\value{
 If retrieve_outputs_to_R is TRUE, returns a list with the following elements:
    \item{model.model_settings}{Table of settings used to build the model.}
    \item{model.model_attributes}{Table of attributes used to build the model.}
    \item{ai.importance}{Table of features along with their importance.}
}

\author{
Pablo Tamayo \email{pablo.tamayo@oracle.com}

Ari Mozes \email{ari.mozes@oracle.com}
}

\references{
Oracle Data Mining Concepts 11g Release 1 (11.1)
\url{http://download.oracle.com/docs/cd/B28359_01/datamine.111/b28129/toc.htm}

Oracle Data Mining Application Developer's Guide 11g Release 1 (11.1)
\url{http://download.oracle.com/docs/cd/B28359_01/datamine.111/b28131/toc.htm}

Oracle Data Mining Administrator's Guide 11g Release 1 (11.1) 
\url{http://download.oracle.com/docs/cd/B28359_01/datamine.111/b28130/toc.htm}

Oracle Database PL/SQL Packages and Types Reference 11g Release 1 (11.1)
\url{http://download.oracle.com/docs/cd/B28359_01/appdev.111/b28419/d_datmin.htm#ARPLS192}
}

\examples{
# Determine attribute importance for survival in the sinking of the Titanic 
# based on pasenger's sex, age, class, etc.

\dontrun{
DB <- RODM_open_dbms_connection(dsn="orcl11g", uid="rodm", pwd="rodm")

data(titanic3, package="PASWR")
db_titanic <- titanic3[,c("pclass", "survived", "sex", "age", "fare", "embarked")]
db_titanic[,"survived"] <- ifelse(db_titanic[,"survived"] == 1, "Yes", "No")
RODM_create_dbms_table(DB, "db_titanic")   # Push the table to the database

# Create the Oracle Data Mining Attribute Importance model
ai <- RODM_create_ai_model(
   database = DB,                      # Database ODBC connection
   data_table_name = "db_titanic",     # Database table containing the input dataset
   target_column_name = "survived",    # Target column name in data_table_name
   model_name = "TITANIC_AI_MODEL")    # Oracle Data Mining model name to create

attribute.importance <- ai$ai.importance
ai.vals <- as.vector(attribute.importance[,3])
names(ai.vals) <- as.vector(attribute.importance[,1])

#windows(height=8, width=12)
barplot(ai.vals, main="Relative survival importance of Titanic dataset attributes",
        ylab = "Relative Importance", xlab = "Attribute", cex.names=0.7)

RODM_drop_model(DB, "TITANIC_AI_MODEL")    # Drop the model
RODM_drop_dbms_table(DB, "db_titanic")     # Drop the database table

RODM_close_dbms_connection(DB)
}
}

\seealso{\code{\link{RODM_drop_model}}}

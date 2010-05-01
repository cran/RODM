\name{RODM_drop_model}

\alias{RODM_drop_model}

\title{Drop an Oracle Data Mining model}

\description{This function drops an Oracle Data Mining model in the Oracle database.}

\usage{
RODM_drop_model(database, 
                model_name,
                sql.log.file = NULL)
}

\arguments{
  \item{database}{Database ODBC channel identifier returned from a call to RODM_open_dbms_connection}
  \item{model_name}{ODM Model name to be dropped.}
  \item{sql.log.file}{File to append the log of all the SQL calls made by this function.}
}

\details{This function drops an Oracle Data Mining model in the Oracle database.}

\value{
  None.
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

\seealso{
\code{\link{RODM_create_svm_model}}, 
\code{\link{RODM_create_kmeans_model}}, 
\code{\link{RODM_create_oc_model}}, 
\code{\link{RODM_create_nmf_model}}, 
\code{\link{RODM_create_ai_model}}, 
\code{\link{RODM_create_nb_model}}, 
\code{\link{RODM_create_glm_model}}, 
\code{\link{RODM_create_assoc_model}}, 
\code{\link{RODM_create_dt_model}}}

\examples{
\dontrun{
DB <- RODM_open_dbms_connection(dsn="orcl11g", uid="rodm", pwd="rodm")
data(titanic3, package="PASWR")
db_titanic <- titanic3[,c("pclass", "survived", "sex", "age", "fare", "embarked")]
db_titanic[,"survived"] <- ifelse(db_titanic[,"survived"] == 1, "Yes", "No")
RODM_create_dbms_table(DB, "db_titanic")   # Push the table to the database
ai <- RODM_create_ai_model(
   database = DB,                    # Database ODBC connection
   data_table_name = "db_titanic",   # Database table containing the input dataset
   target_column_name = "survived",  # Target column name in data_table_name
   model_name = "TITANIC_AI_MODEL")  # Oracle Data Mining model name to create

# Drop the model that was just created
RODM_drop_model(DB, "TITANIC_AI_MODEL")

RODM_drop_dbms_table(DB, "db_titanic")     # Drop the database table
RODM_close_dbms_connection(DB)
}
}



\name{RODM_list_dbms_models}

\alias{RODM_list_dbms_models}

\title{List Oracle Data Mining models}

\description{This function list all of the Oracle Data Mining models in the
user's schema in the Oracle database.}

\usage{
RODM_list_dbms_models(database) 
}

\arguments{
  \item{database}{Database ODBC channel identifier returned from a call to RODM_open_dbms_connection}
}

\details{This function list all of the Oracle Data Mining models in the
user's schema in the database.  For each model, this function
returns the model name, mining function (type of operation),
algorithm, date the model was created, time it took to build the model (in
seconds), size of the model (in megabytes), and comments associated with the
model (if any).
}

\value{
List of the following information:
  \item{MODEL_NAME}{Name of the model.}
  \item{MINING_FUNCTION}{Mining function used when building the model.}
  \item{ALGORITHM}{Algorithm used when building the model.}
  \item{CREATION_DATE}{Date the model was created.}
  \item{BUILD_DURATION}{Duration to build the model in seconds.}
  \item{MODEL_SIZE}{Size of the model in MB.}
  \item{COMMENTS}{Comments associated with the model, if any.}
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
\dontrun{
DB <- RODM_open_dbms_connection(dsn="orcl11g", uid= "rodm", pwd = "rodm")

data(titanic3, package="PASWR")              # Load survival data from Titanic

# Select subset of attributes

ds <- titanic3[,c("pclass", "survived", "sex", "age", "fare", "embarked")]  
ds[,"survived"] <- ifelse(ds[,"survived"] == 1, "Yes", "No") # Rename target values
n.rows <- length(ds[,1])                                     # Number of rows
set.seed(seed=6218945)
random_sample <- sample(1:n.rows, ceiling(n.rows/2))  # Split in train/test subsets
titanic_train <- ds[random_sample,]                         # Training set
titanic_test <-  ds[setdiff(1:n.rows, random_sample),]      # Test set
RODM_create_dbms_table(DB, "titanic_train")  # Push the training table to the RDBMS
RODM_create_dbms_table(DB, "titanic_test")   # Push the testing table to the RDBMS

# Create an ODM Naive Bayes model
nb <- RODM_create_nb_model(
   database = DB,                     # Database ODBC channel identifier
   model_name = "titanic_nb_model",   # ODM model name
   data_table_name = "titanic_train", # (in quotes) Data frame or database table 
                                      # containing the input dataset
   target_column_name = "survived")   # Target column name in data_table_name

# Create an ODM Attribute Importance model
ai <- RODM_create_ai_model(
   database = DB,                     # Database ODBC channel identifier
   model_name = "titanic_ai_model",   # ODM model name
   data_table_name = "titanic_train", # (in quotes) Data frame or database table 
                                      # containing the input dataset
   target_column_name = "survived")   # Target column name in data_table_name
# List the models
mlist <- RODM_list_dbms_models(DB)

RODM_drop_model(DB, "titanic_nb_model")
RODM_drop_model(DB, "titanic_ai_model")
RODM_drop_dbms_table(DB, "titanic_train")
RODM_drop_dbms_table(DB, "titanic_test")

RODM_close_dbms_connection(DB)
}
}



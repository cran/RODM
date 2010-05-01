\name{RODM_create_nb_model}

\alias{RODM_create_nb_model}

\title{Create a Naive Bayes model}

\description{
This function creates an Oracle Data Mining Naive Bayes model.
}

\usage{
RODM_create_nb_model(database, 
                     data_table_name, 
                     case_id_column_name = NULL, 
                     target_column_name, 
                     model_name = "NB_MODEL", 
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
  \item{auto_data_prep}{Whether or not ODM should invoke automatic data preparation for the build.}
  \item{retrieve_outputs_to_R}{Flag controlling if the output results are moved to the R environment.}
  \item{leave_model_in_dbms}{Flag controlling if the model is deleted or left in RDBMS.}
  \item{sql.log.file}{File where to append the log of all the SQL calls made by this function.} 
}

\details{Naive Bayes (NB) for classification makes predictions using Bayes'
  Theorem assuming that each attribute is conditionally independent of
  the others given a particular value of the target (Duda, Hart and
  Stork 2000). NB provides a very flexible general classifier for fast
  model building and scoring that can be used for both binary and
  multi-class classification problems. 

  For more details on the algotithm implementation, parameters settings and 
  characteristics of the ODM function itself consult the following Oracle documents: ODM Concepts, 
  ODM Application Developer's Guide, Oracle SQL Packages: Data Mining, and Oracle Database SQL Language 
  Reference (Data Mining functions), listed in the references below. 
}

\value{
 If retrieve_outputs_to_R is TRUE, returns a list with the following elements:
    \item{model.model_settings}{Table of settings used to build the model.}
    \item{model.model_attributes}{Table of attributes used to build the model.}
    \item{nb.conditionals}{Table of conditional probabilities.}
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

Oracle Database SQL Language Reference (Data Mining functions) 11g Release 1 (11.1)
\url{http://download.oracle.com/docs/cd/B28359_01/server.111/b28286/functions001.htm#SQLRF20030}
}

\examples{
# Predicting survival in the sinking of the Titanic based on pasenger's sex, age, 
# class, etc.
\dontrun{
DB <- RODM_open_dbms_connection(dsn="orcl11g", uid= "rodm", pwd = "rodm")

data(titanic3, package="PASWR")             # Load survival data from Titanic
# Select subset of attributes
ds <- titanic3[,c("pclass", "survived", "sex", "age", "fare", "embarked")]  
ds[,"survived"] <- ifelse(ds[,"survived"] == 1, "Yes", "No") # Rename target values
n.rows <- length(ds[,1])                                     # Number of rows
set.seed(seed=6218945)
random_sample <- sample(1:n.rows, ceiling(n.rows/2))  # Split in train/test subsets
titanic_train <- ds[random_sample,]                         # Training set
titanic_test <-  ds[setdiff(1:n.rows, random_sample),]      # Test set

RODM_create_dbms_table(DB, "titanic_train") # Push the training table to the RDBMS
RODM_create_dbms_table(DB, "titanic_test")  # Push the testing table to the RDBMS

# Create an ODM Naive Bayes model
nb <- RODM_create_nb_model(
   database = DB,                     # Database ODBC channel identifier
   model_name = "titanic_nb_model",   # ODM model name
   data_table_name = "titanic_train", # (in quotes) Data frame or database 
                                      # table containing the input dataset
   target_column_name = "survived")   # Target column name in data_table_name

# Predict test data using the Naive Bayes model
nb2 <- RODM_apply_model(
   database = DB,                    # Database ODBC channel identifier
   data_table_name = "titanic_test", # Database table containing the input dataset
   model_name = "titanic_nb_model",  # ODM model name
   supplemental_cols = "survived")   # Carry the target column to the output 

# Compute contingency matrix, performance statistics and ROC curve
print(nb2$model.apply.results[1:10,])        # Print example of prediction results
actual <- nb2$model.apply.results[, "SURVIVED"]                
predicted <- nb2$model.apply.results[, "PREDICTION"]                
probs <- as.real(as.character(nb2$model.apply.results[, "'Yes'"]))       
table(actual, predicted, dnn = c("Actual", "Predicted")) # Confusion matrix

library(verification)
perf.auc <- roc.area(ifelse(actual == "Yes", 1, 0), probs) # Compute ROC and plot
auc.roc <- signif(perf.auc$A, digits=3)
auc.roc.p <- signif(perf.auc$p.value, digits=3)
roc.plot(ifelse(actual == "Yes", 1, 0), probs, binormal=T, plot="both", 
         xlab="False Positive Rate", 
         ylab="True Postive Rate", main= "Titanic survival ODM NB model ROC Curve")
text(0.7, 0.4, labels= paste("AUC ROC:", signif(perf.auc$A, digits=3)))
text(0.7, 0.3, labels= paste("p-value:", signif(perf.auc$p.value, digits=3)))

RODM_drop_model(DB, "titanic_nb_model")     # Drop the model
RODM_drop_dbms_table(DB, "titanic_train") # Drop the training table in the RDBMS
RODM_drop_dbms_table(DB, "titanic_test")  # Drop the testing table in the RDBMS

RODM_close_dbms_connection(DB)
}
}

\seealso{
\code{\link{RODM_apply_model}},
\code{\link{RODM_drop_model}}}

\keyword{classif}

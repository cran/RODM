\name{RODM_create_dt_model}

\alias{RODM_create_dt_model}

\title{Create a Decision Tree (DT) model}

\description{
This function creates a Decision tree (DT). 
}

\usage{
RODM_create_dt_model(database, 
                     data_table_name, 
                     case_id_column_name = NULL, 
                     target_column_name,
                     model_name = "DT_MODEL", 
                     auto_data_prep = TRUE,
                     cost_matrix = NULL,
                     gini_impurity_metric = TRUE,
                     max_depth = NULL, 
                     minrec_split = NULL,
                     minpct_split = NULL,
                     minrec_node = NULL,
                     minpct_node = NULL,
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
  \item{cost_matrix}{User-specified cost matrix for the target classes.}
  \item{gini_impurity_metric}{Tree impurity metric: "IMPURITY_GINI" (default) or "IMPURITY_ENTROPY"}
  \item{max_depth}{Specifies the maximum depth of the tree, from root to leaf inclusive. 
                             The default is 7.}
  \item{minrec_split}{Specifies the minimum number of cases required in a node in order 
                                for a further split to be possible. Default is 20.}
  \item{minpct_split}{Specifies the minimum number of cases required in a node in order for 
                                a further split to be possible. Expressed as a percentage of all the rows 
                                in the training data. The default is 1 (1 per cent).}
  \item{minrec_node}{Specifies the minimum number of cases required in a child node. 
                               Default is 10.}
  \item{minpct_node}{Specifies the minimum number of cases required in a child node, expressed 
                               as a percentage of the rows in the training data. The default is 0.05 (.05 per cent).}
  \item{retrieve_outputs_to_R}{Flag controlling if the output results are moved to the R environment.}
  \item{leave_model_in_dbms}{Flag controlling if the model is deleted or left in RDBMS.}
  \item{sql.log.file}{File where to append the log of all the SQL calls made by this function.} 
}

\details{

   The Decision Tree algorithm produces accurate and interpretable models with relatively little user 
   intervention and can be used for both binary and multiclass classification problems. The algorithm 
   is fast, both at build time and apply time. The build process for Decision Tree is parallelized. 
   Decision tree scoring is especially fast. The tree structure, created in the model build, is used 
   for a series of simple tests. Each test is based on a single predictor. It is a membership test: 
   either IN or NOT IN a list of values (categorical predictor); or LESS THAN or EQUAL TO some value 
   (numeric predictor). The algorithm supports two homogeneity metrics, gini and entropy, for 
   calculating the splits. 

  For more details on the algotithm implementation, parameters settings and 
  characteristics of the ODM function itself consult the following Oracle documents: ODM Concepts, 
  ODM Developer's Guide, Oracle SQL Packages: Data Mining, and Oracle Database SQL Language 
  Reference (Data Mining functions), listed in the references below. 

}

\value{
 If retrieve_outputs_to_R is TRUE, returns a list with the following elements:
    \item{model.model_settings}{Table of settings used to build the model.}
    \item{model.model_attributes}{Table of attributes used to build the model.}
    \item{dt.distributions}{Target class disctributions at each tree node.}
    \item{dt.nodes}{Node summary information.}
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

\seealso{
\code{\link{RODM_apply_model}},
\code{\link{RODM_drop_model}}}

\examples{
\dontrun{
DB <- RODM_open_dbms_connection(dsn="orcl11g", uid= "rodm", pwd = "rodm")

# Predicting survival in the sinking of the Titanic based on pasenger's sex, age, class, etc.
data(titanic3, package="PASWR")                                             # Load survival data from Titanic
ds <- titanic3[,c("pclass", "survived", "sex", "age", "fare", "embarked")]  # Select subset of attributes
ds[,"survived"] <- ifelse(ds[,"survived"] == 1, "Yes", "No")                # Rename target values
n.rows <- length(ds[,1])                                                    # Number of rows
random_sample <- sample(1:n.rows, ceiling(n.rows/2))   # Split dataset randomly in train/test subsets
titanic_train <- ds[random_sample,]                         # Training set
titanic_test <-  ds[setdiff(1:n.rows, random_sample),]      # Test set
RODM_create_dbms_table(DB, "titanic_train")   # Push the training table to the database
RODM_create_dbms_table(DB, "titanic_test")    # Push the testing table to the database

dt <- RODM_create_dt_model(database = DB,    # Create ODM DT classification model
                             data_table_name = "titanic_train", 
                             target_column_name = "survived", 
                             model_name = "DT_MODEL")

dt2 <- RODM_apply_model(database = DB,    # Predict test data
                         data_table_name = "titanic_test", 
                         model_name = "DT_MODEL",
                         supplemental_cols = "survived")

print(dt2$model.apply.results[1:10,])                                  # Print example of prediction results
actual <- dt2$model.apply.results[, "SURVIVED"]                
predicted <- dt2$model.apply.results[, "PREDICTION"]                
probs <- as.real(as.character(dt2$model.apply.results[, "'Yes'"]))       
table(actual, predicted, dnn = c("Actual", "Predicted"))              # Confusion matrix
library(verification)
perf.auc <- roc.area(ifelse(actual == "Yes", 1, 0), probs)            # Compute ROC and plot
auc.roc <- signif(perf.auc$A, digits=3)
auc.roc.p <- signif(perf.auc$p.value, digits=3)
roc.plot(ifelse(actual == "Yes", 1, 0), probs, binormal=T, plot="both", xlab="False Positive Rate", 
         ylab="True Postive Rate", main= "Titanic survival ODM DT model ROC Curve")
text(0.7, 0.4, labels= paste("AUC ROC:", signif(perf.auc$A, digits=3)))
text(0.7, 0.3, labels= paste("p-value:", signif(perf.auc$p.value, digits=3)))

dt        # look at the model details

RODM_drop_model(DB, "DT_MODEL")             # Drop the model
RODM_drop_dbms_table(DB, "titanic_train")   # Drop the database table
RODM_drop_dbms_table(DB, "titanic_test")    # Drop the database table

RODM_close_dbms_connection(DB)
}
}

\keyword{classif}

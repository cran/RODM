\name{RODM_create_glm_model}

\alias{RODM_create_glm_model}

\title{Create an ODM Generalized Linear Model}

\description{
This function creates an ODM generalized linear model.
}

\usage{
RODM_create_glm_model(database, 
                      data_table_name, 
                      case_id_column_name = NULL, 
                      target_column_name,
                      model_name = "GLM_MODEL", 
                      mining_function = "classification",
                      auto_data_prep = TRUE,
                      class_weights = NULL,
                      weight_column_name = NULL,
                      conf_level = NULL, 
                      reference_class_name = NULL,
                      missing_value_treatment = NULL,
                      ridge_regression = NULL,
                      ridge_value = NULL, 
                      vif_for_ridge = NULL,
                      diagnostics_table_name = NULL,
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
  \item{mining_function}{Type of mining function for GLM  model: "classification" (default) or "regression".}
  \item{auto_data_prep}{Whether or not ODM should invoke automatic data preparation for the build.}
  \item{class_weights}{User-specified weights for the target classes.}
  \item{weight_column_name}{Name of a column in data_table_name that contains a weighting factor for the rows.
        Row weights can be used as a compact representation of repeated rows, and can also be used to emphasize 
        certain rows during model construction.}
  \item{conf_level}{The confidence level for coefficient confidence intervals.}
  \item{reference_class_name}{The target value to be used as the reference value in a logistic 
        regression model. Probabilities will be produced for the other (non-reference) class. 
        By default, the algorithm chooses the value with the highest prevalence (the most cases) 
        for the reference class.}
  \item{missing_value_treatment}{How to handle missing values.  Either replace
        by the mean or mode by setting ODMS_MISSING_VALUE_MEAN_MODE, or delete
        the entire row when a missing value is present by setting ODMS_MISSING_VALUE_DELETE_ROW.}
  \item{ridge_regression}{Whether or not ridge regression will be enabled. By default, 
        the algorithm determines whether or not to use ridge. You can explicitly enable ridge by 
        setting GLMS_RIDGE_REGRESSION to GLMS_RIDGE_REG_ENABLE. Ridge applies to both regression and 
        classification mining functions. When ridge is enabled, no prediction bounds are produced 
        by the PREDICTION_BOUNDS SQL operator.}
  \item{ridge_value}{The value for the ridge parameter used by the algorithm. This setting is 
        only used when you explicitly enable ridge regression by setting GLMS_RIDGE_REGRESSION 
        to GLMS_RIDGE_REG_ENABLE. If ridge regression is enabled internally by the algorithm, 
        the ridge parameter is determined by the algorithm.}
  \item{vif_for_ridge}{(Linear regression only) Whether or not to produce Variance Inflation 
        Factor (VIF) statistics when ridge is being used. By default, VIF is not produced when ridge 
        is enabled.When you explicitly enable ridge regression by setting GLMS_RIDGE_REGRESSION to 
        GLMS_RIDGE_REG_ENABLE, you can request VIF statistics by setting GLMS_VIF_FOR_RIDGE to 
        GLMS_VIF_RIDGE_ENABLE; the algorithm will produce VIF if enough system resources are available.}
  \item{diagnostics_table_name}{Non-existing database table to hold per-row diagnostic
        information.  Requires a case_id_column_name to be specified.  The table will
        remain in the database and must be dropped explicitly when desired.};
  \item{retrieve_outputs_to_R}{Flag controlling if the output results are moved to the R environment.}
  \item{leave_model_in_dbms}{Flag controlling if the model is deleted or left in RDBMS.}
  \item{sql.log.file}{File where to append the log of all the SQL calls made by this function.} 
}

\details{

    Generalized linear models (GLM) implements logistic regression for classification of binary 
    targets and linear regression for continuous targets. GLM classification supports confidence 
    bounds for prediction probabilities. GLM regression supports confidence bounds for predictions
    and supports linear and logistic regression with the logit link and binomial variance functions.
    Ridge regression is a technique that compensates for multicollinearity. Oracle Data Mining 
    supports ridge regression for both regression and classification mining functions. The 
    algorithm automatically uses ridge if it detects singularity (exact multicollinearity) in the data.


  For more details on the algotithm implementation, parameters settings and 
  characteristics of the ODM function itself consult the following Oracle documents: ODM Concepts, 
  ODM Developer's Guide, Oracle SQL Packages: Data Mining, and Oracle Database SQL Language 
  Reference (Data Mining functions), listed in the references below. 
 

}

\value{
 If retrieve_outputs_to_R is TRUE, returns a list with the following elements:
    \item{model.model_settings}{Table of settings used to build the model.}
    \item{model.model_attributes}{Table of attributes used to build the model.}
    \item{glm.globals}{Global details for the GLM model.}
    \item{glm.coefficients}{The coefficients of the GLM model, along with more
          per-attribute information.}
}

\author{
Pablo Tamayo \email{pablo.tamayo@oracle.com}

Ari Mozes \email{ari.mozes@oracle.com}
}

\references{
Dobson, Annette J. and Barnett, Adrian G. (2008) An Introduction to Generalized Linear Models, Third Edition. Texts in Statistical Science ,77 . Chapman & Hall/CRC Press, Boca Raton, FL.

B. L. Milenova, J. S. Yarmus, and M. M. Campos. SVM in oracle database 10g: removing the barriers to widespread adoption of support vector machines. In Proceedings of the ''31st international Conference on Very Large Data Bases'' (Trondheim, Norway, August 30 - September 02, 2005). pp1152-1163, ISBN:1-59593-154-6.

Milenova, B.L. Campos, M.M.,  Mining high-dimensional data for information fusion: a database-centric approach
8th International Conference on Information Fusion, 2005. Publication Date: 25-28 July 2005. ISBN: 0-7803-9286-8.
John Shawe-Taylor & Nello Cristianini. Support Vector Machines and other kernel-based learning methods. Cambridge University Press, 2000.

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

### GLM Classification

# Predicting survival in the sinking of the Titanic based on pasenger's sex, age, class, etc.

data(titanic3, package="PASWR")                                             # Load survival data from Titanic
ds <- titanic3[,c("pclass", "survived", "sex", "age", "fare", "embarked")]  # Select subset of attributes
ds[,"survived"] <- ifelse(ds[,"survived"] == 1, "Yes", "No")                # Rename target values
n.rows <- length(ds[,1])                                                    # Number of rows
random_sample <- sample(1:n.rows, ceiling(n.rows/2))   # Split dataset randomly in train/test subsets
titanic_train <- ds[random_sample,]                         # Training set
train.rows <- length(titanic_train[,1])                                                    # Number of rows
row.id <- matrix(seq(1, train.rows), nrow=train.rows, ncol=1, dimnames= list(NULL, c("ROW_ID"))) # Row id
titanic_train <- cbind(row.id, titanic_train)                                                     # Add row id to dataset 
titanic_test <-  ds[setdiff(1:n.rows, random_sample),]      # Test set
RODM_create_dbms_table(DB, "titanic_train")   # Push the training table to the database
RODM_create_dbms_table(DB, "titanic_test")    # Push the testing table to the database

# Weight one class more heavily than the other
weights <- data.frame(
            target_value = c("Yes", "No"),
            class_weight = c(1, 10))

glm <- RODM_create_glm_model(database = DB,    # Create ODM GLM classification model
                             data_table_name = "titanic_train", 
                             case_id_column_name = "ROW_ID",
                             target_column_name = "survived", 
                             model_name = "GLM_MODEL",
                             class_weights = weights,
                             diagnostics_table_name = "GLM_DIAG",
                             mining_function = "classification")

glm2 <- RODM_apply_model(database = DB,    # Predict test data
                         data_table_name = "titanic_test", 
                         model_name = "GLM_MODEL",
                         supplemental_cols = "survived")

print(glm2$model.apply.results[1:10,])                                  # Print example of prediction results
actual <- glm2$model.apply.results[, "SURVIVED"]                
predicted <- glm2$model.apply.results[, "PREDICTION"]                
probs <- as.real(as.character(glm2$model.apply.results[, "'Yes'"]))       
table(actual, predicted, dnn = c("Actual", "Predicted"))              # Confusion matrix
library(verification)
perf.auc <- roc.area(ifelse(actual == "Yes", 1, 0), probs)            # Compute ROC and plot
auc.roc <- signif(perf.auc$A, digits=3)
auc.roc.p <- signif(perf.auc$p.value, digits=3)
roc.plot(ifelse(actual == "Yes", 1, 0), probs, binormal=T, plot="both", xlab="False Positive Rate", 
         ylab="True Postive Rate", main= "Titanic survival ODM GLM model ROC Curve")
text(0.7, 0.4, labels= paste("AUC ROC:", signif(perf.auc$A, digits=3)))
text(0.7, 0.3, labels= paste("p-value:", signif(perf.auc$p.value, digits=3)))

glm       # look at the model details

# access and look at the per-row diagnostics from model training
diaginfo <- sqlQuery(DB, query = "SELECT * FROM GLM_DIAG")
diaginfo

RODM_drop_model(DB, "GLM_MODEL")            # Drop the model
RODM_drop_dbms_table(DB, "GLM_DIAG")        # Drop the diagnostics table
RODM_drop_dbms_table(DB, "titanic_train")   # Drop the database table
RODM_drop_dbms_table(DB, "titanic_test")    # Drop the database table
}

### GLM Regression
\dontrun{
x1 <- 2 * runif(200) 
noise <- 3 * runif(200) - 1.5
y1 <- 2 + 2*x1 + x1*x1 + noise
dataset <- data.frame(x1, y1)
names(dataset) <- c("X1", "Y1")
RODM_create_dbms_table(DB, "dataset")   # Push the training table to the database

glm <- RODM_create_glm_model(database = DB,    # Create ODM GLM model
                             data_table_name = "dataset", 
                             target_column_name = "Y1",
                             mining_function = "regression")

glm2 <- RODM_apply_model(database = DB,    # Predict training data
                             data_table_name = "dataset",
                             model_name = "GLM_MODEL",
                             supplemental_cols = "X1")
windows(height=8, width=12)
plot(x1, y1, pch=20, col="blue")
points(x=glm2$model.apply.results[, "X1"], 
       glm2$model.apply.results[, "PREDICTION"], pch=20, col="red")
legend(0.5, 9, legend = c("actual", "GLM regression"), pch = c(20, 20), 
                col = c("blue", "red"),
                pt.bg =  c("blue", "red"), cex = 1.20, pt.cex=1.5, bty="n")

RODM_drop_model(DB, "GLM_MODEL")            # Drop the model
RODM_drop_dbms_table(DB, "dataset")   # Drop the database table
RODM_close_dbms_connection(DB)
}
}
\keyword{classif}

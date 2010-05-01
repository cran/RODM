\name{RODM_create_svm_model}

\alias{RODM_create_svm_model}

\title{Create an ODM Support Vector Machine model}

\description{
This function creates an ODM Support Vector Machine model.  
}

\usage{
RODM_create_svm_model(database,                        
                      data_table_name, 
                      case_id_column_name = NULL, 
                      target_column_name = NULL, 
                      model_name = "SVM_MODEL", 
                      mining_function = "classification",
                      auto_data_prep = TRUE,
                      active_learning = TRUE,
                      complexity_factor = NULL, 
                      conv_tolerance = NULL, 
                      epsilon = NULL, 
                      kernel_cache_size = NULL, 
                      kernel_function = NULL, 
                      outlier_rate = NULL, 
                      std_dev = NULL, 
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
  \item{mining_function}{Type of mining function for SVM model: "classification" (default), "regression" or "anomaly_detection".}
  \item{auto_data_prep}{Whether or not ODM should invoke automatic data preparation for the build.}
  \item{active_learning}{Whether or not ODM should use active learning.}
  \item{complexity_factor}{Setting that specifies the complexity factor for SVM. The default is NULL.}
  \item{conv_tolerance}{Setting that specifies tolerance for SVM. The default is 0.001.}  
  \item{epsilon}{Regularization setting for regression, similar to complexity factor. Epsilon 
                      specifies the allowable residuals, or noise, in the data. The default is NULL.}
  \item{kernel_cache_size}{Setting that specifiefs the Gaussian kernel cache size (bytes) for SVM. 
                                The default is 5e+07.}
  \item{kernel_function}{Setting for specifying the kernel function for SVM (Gaussian or Linear).  The default is to let ODM decide based on the data.}
  \item{outlier_rate}{A setting specifying the desired rate of outliers in the training data for 
                           anomaly detection one-class SVM. The default is NULL.}
  \item{std_dev}{A setting that specifies the standard deviation for the SVM Gaussian kernel. The default
                      is NULL (algorithm generated).}
  \item{retrieve_outputs_to_R}{Flag controlling if the output results are moved to the R environment.}
  \item{leave_model_in_dbms}{Flag controlling if the model is deleted or left in RDBMS.}
  \item{sql.log.file}{File where to append the log of all the SQL calls made by this function.} 
}



\details{Support Vector Machines (SVMs) for classification belong to a
  class of algorithms known as "kernel" methods (Cristianini and
  Shawe-Taylor 2000). Kernel methods rely on applying predefined
  functions (kernels) to the input data. The boundary is a function of
  the predictor values. The key concept behind SVMs is that the points
  lying closest to the boundary, i.e., the support vectors, can be used
  to define the boundary. The goal of the SVM algorithm is to identify
  the support vectors and assign them weights that produce an optimal,
  largest margin, class-separating boundary.  

  This function enables to call Oracle Data Mining's SVM implementation
  (for details see Milenova et al 2005) that supports classification,
  regression and anomaly detection (one-class classification) with 
  linear or Gaussian kernels and an automatic and efficient estimation 
  of the complexity factor (C) and standard deviation (sigma). It also 
  supports sparse data, which makes it very efficient for problems such 
  as text mining.  Support Vector Machines (SVMs) for regression utilizes 
  an epsilon-insensitive loss function and works particularly well for 
  high-dimensional noisy data. The scalability and usability of this function 
  are particularly useful when deploying predictive models in a production 
  database data mining system. The implementation also supports Active learning 
  which forces the SVM algorithm to restrict learning to the most informative 
  training examples and not to attempt to use the entire body of data. In most 
  cases, the resulting models have predictive accuracy comparable to that of a 
  standard (exact) SVM model. Active learning provides a significant improvement 
  in both linear and Gaussian SVM models, whether for classification, regression, 
  or anomaly detection. However, active learning is especially advantageous when 
  using the Gaussian kernel, because nonlinear models can otherwise grow to be 
  very large and can place considerable demands on memory and other system resources.

  The SVM algorithm operates natively on numeric attributes. The function 
  automatically "explodes" categorical data into a set of binary attributes, 
  one per category value. For example, a character column for marital status 
  with values married or single would be transformed to two numeric attributes: 
  married and single. The new attributes could have the value 1 (true) or 0 (false).
  When there are missing values in columns with simple data types (not nested), 
  SVM interprets them as missing at random. The algorithm automatically replaces 
  missing categorical values with the mode and missing numerical values with the mean.
  SVM requires the normalization of numeric input. Normalization places the values of 
  numeric attributes on the same scale and prevents attributes with a large original 
  scale from biasing the solution. Normalization also minimizes the likelihood of
  overflows and underflows. Furthermore, normalization brings the numerical attributes 
  to the same scale (0,1) as the exploded categorical data. The SVM algorithm 
  automatically handles missing value treatment and the transformation of categorical 
  data, but normalization and outlier detection must be handled manually. 

  For more details on the algotithm implementation see Milenova et al 2005. For 
  details on the parameters and characteristics of the ODM function itself consult 
  the ODM Concepts, the ODM Developer's Guide and the Oracle SQL Packages: Data 
  Mining documents in the references below. 
}

\value{
 If retrieve_outputs_to_R is TRUE, returns a list with the following elements:
    \item{model.model_settings}{Table of settings used to build the model.}
    \item{model.model_attributes}{Table of attributes used to build the model.}
 If the model that was built uses a linear kernel, then the following is additionally returned:
    \item{svm.coefficients}{The coefficients of the SVM model, one for each
input attribute.  If auto_data_prep, then these coefficients will be in the 
transformed space (after automatic outlier-aware normalization is applied).}
}

\examples{
\dontrun{
DB <- RODM_open_dbms_connection(dsn="orcl11g", uid= "rodm", pwd = "rodm")

# Separating three Gaussian classes in 2D

X1 <- c(rnorm(200, mean = 2, sd = 1), rnorm(300, mean = 8, sd = 2), 
        rnorm(300, mean = 5, sd = 0.6))
Y1 <- c(rnorm(200, mean = 1, sd = 2), rnorm(300, mean = 4, sd = 1.5), 
        rnorm(300, mean = 6, sd = 0.5))
target <- c(rep(1, 200), rep(2, 300), rep(3, 300)) 
ds <- data.frame(cbind(X1, Y1, target)) 
n.rows <- length(ds[,1])                              # Number of rows
set.seed(seed=6218945)
random_sample <- sample(1:n.rows, ceiling(n.rows/2))   # Split in train/test subsets
ds_train <- ds[random_sample,]                         # Training set
ds_test <-  ds[setdiff(1:n.rows, random_sample),]      # Test set
RODM_create_dbms_table(DB, "ds_train")   # Push the training table to the database
RODM_create_dbms_table(DB, "ds_test")    # Push the testing table to the database

svm <- RODM_create_svm_model(database = DB,    # Create ODM SVM  model
                             data_table_name = "ds_train", 
                             target_column_name = "target")

svm2 <- RODM_apply_model(database = DB,    # Predict test data
                         data_table_name = "ds_test", 
                         model_name = "SVM_MODEL",
                         supplemental_cols = c("X1","Y1","TARGET"))

color.map <- c("blue", "green", "red")
color <- color.map[svm2$model.apply.results[, "TARGET"]]
plot(as.numeric(as.character(svm2$model.apply.results[, "X1"])),
     as.numeric(as.character(svm2$model.apply.results[, "Y1"])),
     pch=20, col=color, ylim=c(-2,10), xlab="X1", ylab="Y1", 
     main="Test Set")


actual <- svm2$model.apply.results[, "TARGET"]
predicted <- svm2$model.apply.results[, "PREDICTION"]
for (i in 1:length(ds_test[,1])) {
   if (actual[i] != predicted[i]) 
    points(x=svm2$model.apply.results[i, "X1"],
           y=svm2$model.apply.results[i, "Y1"],
           col = "black", pch=20)
}
legend(6, 1.5, legend=c("Class 1 (correct)", "Class 2 (correct)", 
       "Class 3 (correct)", "Error"), 
       pch = rep(20, 4), col = c(color.map, "black"), pt.bg = c(color.map, 
       "black"), cex = 1.20, pt.cex=1.5, bty="n")

RODM_drop_model(DB, "SVM_MODEL")       # Drop the model
RODM_drop_dbms_table(DB, "ds_train")   # Drop the database table
RODM_drop_dbms_table(DB, "ds_test")    # Drop the database table
}

### SVM Classification

# Predicting survival in the sinking of the Titanic based on pasenger's sex, age,
# class, etc.
\dontrun{
data(titanic3, package="PASWR")            # Load survival data from Titanic

# Select subset of attributes

ds <- titanic3[,c("pclass", "survived", "sex", "age", "fare", "embarked")]  
ds[,"survived"] <- ifelse(ds[,"survived"] == 1, "Yes", "No") # Rename target values
n.rows <- length(ds[,1])                                     # Number of rows
random_sample <- sample(1:n.rows, ceiling(n.rows/2)) # Split in train/test subsets
titanic_train <- ds[random_sample,]                         # Training set
titanic_test <-  ds[setdiff(1:n.rows, random_sample),]      # Test set
RODM_create_dbms_table(DB, "titanic_train") # Push the training table to the RDBMS
RODM_create_dbms_table(DB, "titanic_test")  # Push the testing table to the RDBMS

svm <- RODM_create_svm_model(database = DB,    # Create ODM SVM model
                             data_table_name = "titanic_train", 
                             target_column_name = "survived", 
                             model_name = "SVM_MODEL",
                             mining_function = "classification")

svm2 <- RODM_apply_model(database = DB,    # Predict test data
                         data_table_name = "titanic_test", 
                         model_name = "SVM_MODEL",
                         supplemental_cols = "survived")

print(svm2$model.apply.results[1:10,])       # Print example of prediction results
actual <- svm2$model.apply.results[, "SURVIVED"]                
predicted <- svm2$model.apply.results[, "PREDICTION"]                
probs <- as.real(as.character(svm2$model.apply.results[, "'Yes'"]))       
table(actual, predicted, dnn = c("Actual", "Predicted"))  # Confusion matrix
library(verification)
perf.auc <- roc.area(ifelse(actual == "Yes", 1, 0), probs) # Compute ROC and plot
auc.roc <- signif(perf.auc$A, digits=3)
auc.roc.p <- signif(perf.auc$p.value, digits=3)
roc.plot(ifelse(actual == "Yes", 1, 0), probs, binormal=T, plot="both", 
         xlab="False Positive Rate", 
         ylab="True Postive Rate", 
         main= "Titanic survival ODM SVM model ROC Curve")
text(0.7, 0.4, labels= paste("AUC ROC:", signif(perf.auc$A, digits=3)))
text(0.7, 0.3, labels= paste("p-value:", signif(perf.auc$p.value, digits=3)))

RODM_drop_model(DB, "SVM_MODEL")            # Drop the model
RODM_drop_dbms_table(DB, "titanic_train")   # Drop the database table
RODM_drop_dbms_table(DB, "titanic_test")    # Drop the database table
}

### SVM Regression

# Aproximating a one-dimensional non-linear function
\dontrun{
X1 <- 10 * runif(500) - 5 
Y1 <- X1*cos(X1) + 2*runif(500) 
ds <- data.frame(cbind(X1, Y1)) 
RODM_create_dbms_table(DB, "ds")   # Push the training table to the database

svm <- RODM_create_svm_model(database = DB,    # Create ODM SVM regression model
                             data_table_name = "ds",
                             target_column_name = "Y1",
                             mining_function = "regression")

svm2 <- RODM_apply_model(database = DB,    # Predict training data
                         data_table_name = "ds", 
                         model_name = "SVM_MODEL",
                         supplemental_cols = "X1")

plot(X1, Y1, pch=20, col="blue")
points(x=svm2$model.apply.results[, "X1"], 
       svm2$model.apply.results[, "PREDICTION"], pch=20, col="red")
legend(-4, -1.5, legend = c("actual", "SVM regression"), pch = c(20, 20), 
                col = c("blue", "red"),
                pt.bg =  c("blue", "red"), cex = 1.20, pt.cex=1.5, bty="n")

RODM_drop_model(DB, "SVM_MODEL")       # Drop the model
RODM_drop_dbms_table(DB, "ds")         # Drop the database table
}

### Anomaly detection
# Finding outliers in a 2D-dimensional discrete distribution of points
\dontrun{
X1 <- c(rnorm(200, mean = 2, sd = 1), rnorm(300, mean = 8, sd = 2))
Y1 <- c(rnorm(200, mean = 2, sd = 1.5), rnorm(300, mean = 8, sd = 1.5))
ds <- data.frame(cbind(X1, Y1)) 
RODM_create_dbms_table(DB, "ds")   # Push the table to the database

svm <- RODM_create_svm_model(database = DB,    # Create ODM SVM anomaly model
                             data_table_name = "ds", 
                             target_column_name = NULL, 
                             model_name = "SVM_MODEL",
                             mining_function = "anomaly_detection")

svm2 <- RODM_apply_model(database = DB,    # Predict training data
                         data_table_name = "ds", 
                         model_name = "SVM_MODEL",
                         supplemental_cols = c("X1","Y1"))

plot(X1, Y1, pch=20, col="white")
col <- ifelse(svm2$model.apply.results[, "PREDICTION"] == 1, "green", "red")
for (i in 1:500) points(x=svm2$model.apply.results[i, "X1"], 
                        y=svm2$model.apply.results[i, "Y1"], 
                        col = col[i], pch=20)
legend(8, 2, legend = c("typical", "anomaly"), pch = c(20, 20), col = c("green", 
                "red"), pt.bg =  c("green", "red"), cex = 1.20, pt.cex=1.5, 
                bty="n")

RODM_drop_model(DB, "SVM_MODEL")       # Drop the model
RODM_drop_dbms_table(DB, "ds")         # Drop the database table

RODM_close_dbms_connection(DB)
}
}

\author{
Pablo Tamayo \email{pablo.tamayo@oracle.com}

Ari Mozes \email{ari.mozes@oracle.com}
}

\references{
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

\keyword{classif}


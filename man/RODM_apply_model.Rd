\name{RODM_apply_model}

\alias{RODM_apply_model}

\title{Apply an Oracle Data Mining model}

\description{
This function applies a previously created ODM model to score new data.
}

\usage{
RODM_apply_model(database, 
                 data_table_name, 
                 model_name,
                 supplemental_cols,
                 sql.log.file = NULL)
}

\arguments{
  \item{database}{Database ODBC channel identifier returned from a call to RODM_open_dbms_connection}
  \item{data_table_name}{Database table/view containing the training dataset.}
  \item{model_name}{ODM Model name}
  \item{supplemental_cols}{Columns to carry over into the output result.}
  \item{sql.log.file}{File to append the log of all the SQL calls made by this function.}
}

\details{
This function applies a previously created ODM model to score new data.
The supplemental_cols parameter should be assigned in such a way as to retain
the connection between the scores and the original cases.  The simplest way to
do this is to include a unique case identifier in the list, which provides the
ability to identify the original row information for a score.  If only some
of the information from the original data is needed (for example, only the
actual target value is needed when computing a measure of accuracy), then it
is only this information which should be identified by the supplemental columns.
}

\value{A list with the following components:

  \item{model.apply_results}{A data frame table containing:
    For classification:
         class 1 probability  	numeric/double
         ...   	 		...
         class N probability	numeric/double
         supplemental column 1
         ...  
         supplemental column M
         prediction   		

    For regression:
         supplemental column 1
         ...  
         supplemental column M
         prediction   		numeric/double

    For anomaly detection (e.g. one-class SVM):
         class 1 probability	numeric/integer (class 1 is the typical class)  
	 class 0 probability	numeric/integer (class 0 is the outlier class)  
         supplemental column 1
         ...  
         supplemental column M
         prediction             integer: 0 or 1

    For clustering:
         leaf cluster 1 probability  	numeric/double
         ...                            ...
         leaf cluster N probability	numeric/double
         supplemental column 1
         ...  
         supplemental column M
         cluster_id   		
}
}

\author{
Pablo Tamayo \email{pablo.tamayo@oracle.com}

Ari Mozes \email{ari.mozes@oracle.com}
}

\examples{

\dontrun{
DB <- RODM_open_dbms_connection(dsn="orcl11g", uid= "rodm", pwd = "rodm")

### Classification

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
svm <- RODM_create_svm_model(database = DB,    # Create ODM SVM classification model
                             data_table_name = "titanic_train", 
                             target_column_name = "survived", 
                             model_name = "SVM_MODEL",
                             mining_function = "classification")

# Apply the SVM classification model to test data.
svm2 <- RODM_apply_model(database = DB,    # Predict test data
                         data_table_name = "titanic_test", 
                         model_name = "SVM_MODEL",
                         supplemental_cols = "survived")

print(svm2$model.apply.results[1:10,])                                # Print example of prediction results
actual <- svm2$model.apply.results[, "SURVIVED"]                
predicted <- svm2$model.apply.results[, "PREDICTION"]                
probs <- as.real(as.character(svm2$model.apply.results[, "'Yes'"]))       
table(actual, predicted, dnn = c("Actual", "Predicted"))              # Confusion matrix
library(verification)
perf.auc <- roc.area(ifelse(actual == "Yes", 1, 0), probs)            # Compute ROC and plot
auc.roc <- signif(perf.auc$A, digits=3)
auc.roc.p <- signif(perf.auc$p.value, digits=3)
roc.plot(ifelse(actual == "Yes", 1, 0), probs, binormal=T, plot="both", xlab="False Positive Rate", 
         ylab="True Postive Rate", main= "Titanic survival ODM SVM model ROC Curve")
text(0.7, 0.4, labels= paste("AUC ROC:", signif(perf.auc$A, digits=3)))
text(0.7, 0.3, labels= paste("p-value:", signif(perf.auc$p.value, digits=3)))

RODM_drop_model(DB, "SVM_MODEL")            # Drop the model
RODM_drop_dbms_table(DB, "titanic_train")   # Drop the training table in the database
RODM_drop_dbms_table(DB, "titanic_test")    # Drop the testing table in the database
}

### Regression

# Aproximating a one-dimensional non-linear function

\dontrun{
X1 <- 10 * runif(500) - 5 
Y1 <- X1*cos(X1) + 2*runif(500) 
ds <- data.frame(cbind(X1, Y1)) 
RODM_create_dbms_table(DB, "ds")   # Push the table to the database
svm <- RODM_create_svm_model(database = DB,    # Create ODM SVM regression model
                             data_table_name = "ds", 
                             target_column_name = "Y1", 
                             model_name = "SVM_MODEL",
                             mining_function = "regression")

# Apply the SVM regression model to test data.
svm2 <- RODM_apply_model(database = DB,    # Predict training data
                         data_table_name = "ds", 
                         model_name = "SVM_MODEL",
                         supplemental_cols = "X1")

plot(X1, Y1, pch=20, col="blue")
points(x=svm2$model.apply.results[, "X1"], svm2$model.apply.results[, "PREDICTION"], pch=20, col="red")
legend(-4, -1.5, legend = c("actual", "SVM regression"), pch = c(20, 20), col = c("blue", "red"),
                pt.bg =  c("blue", "red"), cex = 1.20, pt.cex=1.5, bty="n")

RODM_drop_model(DB, "SVM_MODEL")            # Drop the model
RODM_drop_dbms_table(DB, "ds")              # Drop the database table
}

### Anomaly detection

# Finding outliers in a 2D-dimensional discrete distribution of points

\dontrun{
X1 <- c(rnorm(200, mean = 2, sd = 1), rnorm(300, mean = 8, sd = 2))
Y1 <- c(rnorm(200, mean = 2, sd = 1.5), rnorm(300, mean = 8, sd = 1.5))
ds <- data.frame(cbind(X1, Y1)) 
RODM_create_dbms_table(DB, "ds")   # Push the table to the database
svm <- RODM_create_svm_model(database = DB,    # Create ODM SVM anomaly detection model
                             data_table_name = "ds", 
                             target_column_name = NULL, 
                             model_name = "SVM_MODEL",
                             mining_function = "anomaly_detection")

# Apply the SVM anomaly detection model to data.
svm2 <- RODM_apply_model(database = DB,    # Predict training data
                         data_table_name = "ds", 
                         model_name = "SVM_MODEL",
                         supplemental_cols = c("X1","Y1"))

plot(X1, Y1, pch=20, col="white")
col <- ifelse(svm2$model.apply.results[, "PREDICTION"] == 1, "green", "red")
for (i in 1:500) points(x=svm2$model.apply.results[i, "X1"], 
                        y=svm2$model.apply.results[i, "Y1"], 
                        col = col[i], pch=20)
legend(8, 2, legend = c("typical", "anomaly"), pch = c(20, 20), col = c("green", "red"),
                pt.bg =  c("green", "red"), cex = 1.20, pt.cex=1.5, bty="n")

RODM_drop_model(DB, "SVM_MODEL")            # Drop the model
RODM_drop_dbms_table(DB, "ds")    # Drop the database table
}

### Clustering 

# Clustering a 2D multi-Gaussian distribution of points into clusters

\dontrun{
set.seed(seed=6218945)
X1 <- c(rnorm(100, mean = 2, sd = 1), rnorm(100, mean = 8, sd = 2), rnorm(100, mean = 5, sd = 0.6),
        rnorm(100, mean = 4, sd = 1), rnorm(100, mean = 10, sd = 1)) # Create and merge 5 Gaussian distributions
Y1 <- c(rnorm(100, mean = 1, sd = 2), rnorm(100, mean = 4, sd = 1.5), rnorm(100, mean = 6, sd = 0.5),
        rnorm(100, mean = 3, sd = 0.2), rnorm(100, mean = 2, sd = 1))
ds <- data.frame(cbind(X1, Y1)) 
n.rows <- length(ds[,1])                                                    # Number of rows
row.id <- matrix(seq(1, n.rows), nrow=n.rows, ncol=1, dimnames= list(NULL, c("ROW_ID"))) # Row id
ds <- cbind(row.id, ds)                                                     # Add row id to dataset 
RODM_create_dbms_table(DB, "ds")   
km <- RODM_create_kmeans_model(
   database = DB,                  # database ODBC channel identifier
   data_table_name = "ds",         # data frame containing the input dataset
   case_id_column_name = "ROW_ID", # case id to enable assignments during build
   num_clusters = 5)

# Apply the K-Means clustering model to data.
km2 <- RODM_apply_model(
   database = DB,                  # database ODBC channel identifier
   data_table_name = "ds",         # data frame containing the input dataset
   model_name = "KM_MODEL",
   supplemental_cols = c("X1","Y1"))

x1a <- km2$model.apply.results[, "X1"]
y1a <- km2$model.apply.results[, "Y1"]
clu <- km2$model.apply.results[, "CLUSTER_ID"]
c.numbers <- unique(as.numeric(clu))
c.assign <- match(clu, c.numbers)
color.map <- c("blue", "green", "red", "orange", "purple")
color <- color.map[c.assign]
nf <- layout(matrix(c(1, 2), 1, 2, byrow=T), widths = c(1, 1), heights = 1, respect = FALSE)
plot(x1a, y1a, pch=20, col=1, xlab="X1", ylab="Y1", main="Original Data Points")
plot(x1a, y1a, pch=20, type = "n", xlab="X1", ylab="Y1", main="After kmeans clustering")
for (i in 1:n.rows) {
   points(x1a[i], y1a[i], col= color[i], pch=20)
}   
legend(5, -0.5, legend=c("Cluster 1", "Cluster 2", "Cluster 3", "Cluster 4", "Cluster 5"), pch = rep(20, 5), 
       col = color.map, pt.bg = color.map, cex = 0.8, pt.cex=1, bty="n")

RODM_drop_model(DB, "KM_MODEL")         # Drop the model
RODM_drop_dbms_table(DB, "ds")          # Drop the database table


RODM_close_dbms_connection(DB)
}
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
\code{\link{RODM_create_svm_model}}, 
\code{\link{RODM_create_kmeans_model}}, 
\code{\link{RODM_create_oc_model}}, 
\code{\link{RODM_create_nb_model}}, 
\code{\link{RODM_create_glm_model}}, 
\code{\link{RODM_create_dt_model}}}

\keyword{classif}
\keyword{cluster}


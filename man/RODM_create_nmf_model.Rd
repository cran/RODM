\name{RODM_create_nmf_model}

\alias{RODM_create_nmf_model}

\title{Create a Non-Negative Matrix Factorization (NMF) model }

\description{
This function creates a Non-Negative Matrix Factorization (NMF) model. 
}

\usage{
RODM_create_nmf_model(database, 
                      data_table_name, 
                      case_id_column_name = NULL, 
                      model_name = "NMF_MODEL",
                      auto_data_prep = TRUE,
                      num_features = NULL, 
                      conv_tolerance = NULL,
                      num_iterations = NULL,
                      random_seed = NULL, 
                      retrieve_outputs_to_R = TRUE,
                      leave_model_in_dbms = TRUE, 
                      sql.log.file = NULL)
}

\arguments{
  \item{database}{Database ODBC channel identifier returned from a call to RODM_open_dbms_connection}
  \item{data_table_name}{Database table/view containing the training dataset.}
  \item{case_id_column_name}{Row unique case identifier in data_table_name.}
  \item{model_name}{ODM Model name.}
  \item{auto_data_prep}{Whether or not ODM should invoke automatic data preparation for the build.}
  \item{num_features}{Number of NMF factors.}
  \item{conv_tolerance}{Setting that specifies convergence tolerance for nmf.}
  \item{num_iterations}{Setting that specifies the number of iterations for nmf.}
  \item{random_seed}{Setting that specifies the random seed for nmf.}
  \item{retrieve_outputs_to_R}{Flag controlling if the output results are moved to the R environment.}
  \item{leave_model_in_dbms}{Flag controlling if the model is deleted or left in RDBMS.}
  \item{sql.log.file}{File where to append the log of all the SQL calls made by this function.}    
}

\details{Non-Negative Matrix Factorization (NMF) is a feature extraction
  and dimensionality reduction algorithm that creates a new set of
  features by decomposing the original data matrix V into the product of
  two lower rank matrices W and H so that V ~ W x H (Lee and Seung
  1999). NMF uses an iterative procedure to modify the initial values of
  W and H so that the product becomes increasingly closer and closer to
  V. Each feature is a linear combination of the original attributes;
  the coefficients of these linear combinations are sparse and
  non-negative. Some applications of feature extraction are image
  decomposition and latent semantic analysis (Lee and Seung 1999), data
  compression, and molecular pattern recognition (Brunet et al
  2004). The ODM implementation uses a default to adjust for the number
  of features and different filtering parameters for returning
  coefficients and stopping criteria.

  For more details on the algotithm implementation, parameters settings and 
  characteristics of the ODM function itself consult the following Oracle documents: ODM Concepts, 
  ODM Developer's Guide, Oracle SQL Packages: Data Mining, and Oracle Database SQL Language 
  Reference (Data Mining functions), listed in the references below. 

}

\value{
 If retrieve_outputs_to_R is TRUE, returns a list with the following elements:
    \item{model.model_settings}{Table of settings used to build the model.}
    \item{model.model_attributes}{Table of attributes used to build the model.}
    \item{nmf.features}{The NMF features}
    \item{nmf.H}{The H matrix of the converged NMF model}
}

\author{
Pablo Tamayo \email{pablo.tamayo@oracle.com}

Ari Mozes \email{ari.mozes@oracle.com}
}

\references{
Lee and Seung 1999. D. D. Lee and H. S. Seung Learning the Parts of Objects by Non-Negative Matrix Factorization, Nature  401, pages 788-7910, 1999.

Lee, D. D. & Seung, H. S. (2001) Adv. Neural Info. Proc. Syst. 13, 556-562.

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

# NMF projection of 101 animals into 3 summary features (factors F1-3)
# based on 16 attributes

data(Zoo, package="mlbench")
Zoo <- data.matrix(Zoo[, -1]) + 0.00001
Zoo2 <- t(Zoo) 
nrow <- length(Zoo2[,1])
dataset <- data.frame(seq(1, nrow), Zoo2)
names(dataset)[1] <- "ROW_ID"
RODM_create_dbms_table(DB, "dataset")   

nmf <- RODM_create_nmf_model(
   database = DB,                  # Database ODBC channel identifier
   data_table_name = "dataset",    # (in quotes) Data frame or database table 
                                   # containing the input dataset
   case_id_column_name ="ROW_ID",  # Name of the column of data_table_frame 
                                   # containing the case id (optional)  
   num_features = 3)               # setting that specifies the number of features 

nmf2 <- RODM_apply_model(
   database = DB,                  # database ODBC channel identifier
   data_table_name = "dataset",    # data frame containing the input dataset
   model_name = "NMF_MODEL",
   supplemental_cols = "ROW_ID")

H <- matrix(0, nrow = 3, ncol=length(colnames(Zoo2)), dimnames = list(c("F1", "F2", "F3"), colnames(Zoo2)))
for (i in 1:length(nmf$nmf.features[, 1])) {
   H[nmf$nmf.features[i,"FEATURE_ID"], nmf$nmf.features[i,"ATTRIBUTE_NAME"]] <- nmf$nmf.features[i,"COEFFICIENT"] 
}
library(scatterplot3d)
windows(height=8, width=12)
x <- scatterplot3d(H[1,], H[2,], H[3,], xlab ="F1", ylab = "F2", 
             zlab = "F3", angle = 45, pch=20, main="NMF projection of animals", 
             cex.symbols=0.8)
for (i in sample(length(H[1,]), 20)) {
   point.3D <- x$xyz.convert(H[1, i], H[2, i], H[3, i])
   text (point.3D$x, point.3D$y, labels = colnames(H)[i], 
             adj = NULL, pos = (i %% 4) + 1, offset = 0.5, vfont = NULL,
              cex = 0.8, col = "blue")
}

RODM_drop_model(DB, "NMF_MODEL")            # Drop the model
RODM_drop_dbms_table(DB, "dataset")         # Drop the database table

RODM_close_dbms_connection(DB)
}
}

\keyword{cluster}

\name{RODM_create_kmeans_model}

\alias{RODM_create_kmeans_model}

\title{Create a Hierarchical k-means model}

\description{
This function creates a Hierarchical k-means model. 
}

\usage{
RODM_create_kmeans_model(database, 
                         data_table_name, 
                         case_id_column_name = NULL, 
                         model_name = "KM_MODEL",
                         auto_data_prep = TRUE,
                         num_clusters = NULL, 
                         block_growth = NULL, 
                         conv_tolerance = NULL,
                         euclidean_distance = TRUE,
                         iterations = NULL, 
                         min_pct_attr_support = NULL,
                         num_bins = NULL, 
                         variance_split = TRUE,
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
  \item{num_clusters}{Setting that specifies the number of clusters for a clustering model.}
  \item{block_growth}{Setting that specifies the growth factor for memory to hold cluster data for k-Means.}
  \item{conv_tolerance}{Setting that specifies the convergence tolerance for k-Means.}
  \item{euclidean_distance}{Distance function (cosine, euclidean or fast_cosine).}
  \item{iterations}{Setting that specifies the number of iterations for k-Means.}
  \item{min_pct_attr_support}{Setting that specifies the minimum percent required for attributes in rules.}
  \item{num_bins}{Setting that specifies the number of histogram bins k-Means.}
  \item{variance_split}{Setting that specifies the split criterion for k-Means.}
  \item{retrieve_outputs_to_R}{Flag controlling if the output results are moved to the R environment.}
  \item{leave_model_in_dbms}{Flag controlling if the model is deleted or left in RDBMS.}
  \item{sql.log.file}{File where to append the log of all the SQL calls made by this function.}    
}

\details{The algorithm k-means (kmeans) uses a distance-based similarity measure
  and tessellates the data space creating hierarchies. It handles large
  data volumes via summarization and supports sparse data. It is
  especially useful when the dataset has a moderate number of numerical
  attributes and one has a predetermined number of clusters. The main
  parameters settings correspond to the choice of distance function
  (e.g., Euclidean or cosine), number of iterations, convergence
  tolerance and split criterion.

  For more details on the algotithm implementation, parameters settings and 
  characteristics of the ODM function itself consult the following Oracle documents: ODM Concepts, 
  ODM Developer's Guide and Oracle SQL Packages: Data Mining, and Oracle Database SQL Language 
  Reference (Data Mining functions), listed in the references below. 

}

\value{
 If retrieve_outputs_to_R is TRUE, returns a list with the following elements:
    \item{model.model_settings}{Table of settings used to build the model.}
    \item{model.model_attributes}{Table of attributes used to build the model.}
    \item{km.clusters}{General per-cluster information.}
    \item{km.taxonomy}{Parent-child cluster relationship.}
    \item{km.centroid}{Per cluster-attribute centroid information.}
    \item{km.histogram}{Per cluster-attribute hitogram information.}
    \item{km.rule}{Cluster rules.}
    \item{km.leaf_cluster_count}{Leaf clusters with support.}
    \item{km.assignment}{Assignment of training data to clusters (with probability).}
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

### Clustering a 2D multi-Gaussian distribution of points into clusters

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

km        # look at the model details and cluster assignments

RODM_drop_model(DB, "KM_MODEL")   # Drop the model
RODM_drop_dbms_table(DB, "ds")    # Drop the database table

RODM_close_dbms_connection(DB)
}
}
\keyword{cluster}


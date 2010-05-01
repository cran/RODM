\name{RODM_create_oc_model}

\alias{RODM_create_oc_model}

\title{Create an O-cluster model}

\description{
This function creates a O-cluster model. 
}

\usage{
RODM_create_oc_model(database, 
                         data_table_name, 
                         case_id_column_name,
                         model_name = "OC_MODEL",
                         auto_data_prep = TRUE,
                         num_clusters = NULL, 
                         max_buffer = NULL,
                         sensitivity = NULL,
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
  \item{num_clusters}{Setting that specifies the number of clusters for the clustering model.}
  \item{max_buffer}{Buffer size for O-Cluster. Default is 50,000.}
  \item{sensitivity}{A fraction that specifies the peak density required for separating a new 
        cluster. The fraction is related to the global uniform density. Default is 0.5.}
  \item{retrieve_outputs_to_R}{Flag controlling if the output results are moved to the R environment.}
  \item{leave_model_in_dbms}{Flag controlling if the model is deleted or left in RDBMS.}
  \item{sql.log.file}{File where to append the log of all the SQL calls made by this function.}    
}

\details{
     The O-Cluster algorithm creates a hierarchical grid-based clustering model, that is, 
   it creates axis-parallel (orthogonal) partitions in the input attribute space. The algorithm 
   operates recursively. The resulting hierarchical structure represents an irregular grid that 
   tessellates the attribute space into clusters. The resulting clusters define dense areas in the 
   attribute space.

   The clusters are described by intervals along the attribute axes and the corresponding centroids 
   and histograms. A parameter called sensitivity defines a baseline density level. Only areas with 
   peak density above this baseline level can be identified as clusters.

   The k-means algorithm tessellates the space even when natural clusters may not exist. For example, 
   if there is a region of uniform density, k-Means tessellates it into n clusters (where n is specified 
   by the user). O-Cluster separates areas of high density by placing cutting planes through areas of 
   low density. O-Cluster needs multi-modal histograms (peaks and valleys). If an area has projections 
   with uniform or monotonically changing density, O-Cluster does not partition it.

   The clusters discovered by O-Cluster are used to generate a Bayesian probability model that is then 
   used during scoring (model apply) for assigning data points to clusters. The generated probability 
   model is a mixture model where the mixture components are represented by a product of independent 
   normal distributions for numerical attributes and multinomial distributions for categorical attributes.

   Keep the following in mind if you choose to prepare the data for O-Cluster: 1. O-Cluster does not 
   necessarily use all the input data when it builds a model. It reads the data in batches (the default 
   batch size is 50000). It will only read another batch if it believes, based on statistical tests, 
   that there may still exist clusters that it has not yet uncovered. 2. Because O-Cluster may stop the 
   model build before it reads all of the data, it is highly recommended that the data be randomized.
   3. Binary attributes should be declared as categorical. O-Cluster maps categorical data to numerical 
   values. 4. The use of Oracle Data Mining's equi-width binning transformation with automated estimation 
   of the required number of bins is highly recommended. 5. The presence of outliers can significantly 
   impact clustering algorithms. Use a clipping transformation before binning or normalizing. Outliers 
   with equi-width binning can prevent O-Cluster from detecting clusters. As a result, the whole 
   population appears to falls within a single cluster.

  For more details on the algotithm implementation, parameters settings and 
  characteristics of the ODM function itself consult the following Oracle documents: ODM Concepts, 
  ODM Developer's Guide and Oracle SQL Packages: Data Mining, and Oracle Database SQL Language 
  Reference (Data Mining functions), listed in the references below. 

}

\value{
 If retrieve_outputs_to_R is TRUE, returns a list with the following elements:
    \item{model.model_settings}{Table of settings used to build the model.}
    \item{model.model_attributes}{Table of attributes used to build the model.}
    \item{oc.clusters}{General per-cluster information.}
    \item{oc.split_predicate}{Cluster split predicates.}
    \item{oc.taxonomy}{Parent-child cluster relationship.}
    \item{oc.centroid}{Per cluster-attribute centroid information.}
    \item{oc.histogram}{Per cluster-attribute hitogram information.}
    \item{oc.rule}{Cluster rules.}
    \item{oc.leaf_cluster_count}{Leaf clusters with support.}
    \item{oc.assignment}{Assignment of training data to clusters (with probability).}
}

\author{
Pablo Tamayo \email{pablo.tamayo@oracle.com}

Ari Mozes \email{ari.mozes@oracle.com}
}

\references{
B.L. Milenova and M.M. Campos, Clustering Large Databases with Numeric and Nominal Values Using 
Orthogonal Projection, Proceeding of the 29th VLDB Conference, Berlin, Germany (2003). 

Oracle9i O-Cluster: Scalable Clustering of Large High Dimensional Data Sets
\url{http://www.oracle.com/technology/products/bi/odm/pdf/o_cluster_algorithm.pdf}

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
# Create and merge 5 Gaussian distributions
X1 <- c(rnorm(100, mean = 2, sd = 1), rnorm(100, mean = 8, sd = 2), 
        rnorm(100, mean = 5, sd = 0.6),
        rnorm(100, mean = 4, sd = 1), rnorm(100, mean = 10, sd = 1)) 
Y1 <- c(rnorm(100, mean = 1, sd = 2), rnorm(100, mean = 4, sd = 1.5), 
        rnorm(100, mean = 6, sd = 0.5),
        rnorm(100, mean = 3, sd = 0.2), rnorm(100, mean = 2, sd = 1))
ds <- data.frame(cbind(X1, Y1)) 
n.rows <- length(ds[,1])                          # Number of rows
row.id <- matrix(seq(1, n.rows), nrow=n.rows, ncol=1, dimnames= 
                 list(NULL, c("ROW_ID"))) # Row id
ds <- cbind(row.id, ds)                           # Add row id to dataset 
RODM_create_dbms_table(DB, "ds")   

oc <- RODM_create_oc_model(
   database = DB,                  # database ODBC channel identifier
   data_table_name = "ds",         # data frame containing the input dataset
   case_id_column_name = "ROW_ID", # case id to enable assignments during build
   num_clusters = 5)

oc2 <- RODM_apply_model(
   database = DB,                  # database ODBC channel identifier
   data_table_name = "ds",         # data frame containing the input dataset
   model_name = "OC_MODEL",
   supplemental_cols = c("X1","Y1"))

x1a <- oc2$model.apply.results[, "X1"]
y1a <- oc2$model.apply.results[, "Y1"]
clu <- oc2$model.apply.results[, "CLUSTER_ID"]
c.numbers <- unique(as.numeric(clu))
c.assign <- match(clu, c.numbers)
color.map <- c("blue", "green", "red")
color <- color.map[c.assign]
nf <- layout(matrix(c(1, 2), 1, 2, byrow=T), widths = c(1, 1), heights = 1,
             respect = FALSE)
plot(x1a, y1a, pch=20, col=1, xlab="X1", ylab="Y1", main="Original Data Points")
plot(x1a, y1a, pch=20, type = "n", xlab="X1", ylab="Y1", main="After OC clustering")
for (i in 1:n.rows) {
   points(x1a[i], y1a[i], col= color[i], pch=20)
}   
legend(5, -0.5, legend=c("Cluster 1", "Cluster 2", "Cluster 3"), pch = rep(20, 3), 
       col = color.map, pt.bg = color.map, cex = 0.8, pt.cex=1, bty="n")

RODM_drop_model(DB, "OC_MODEL")   # Drop the database table
RODM_drop_dbms_table(DB, "ds")    # Drop the database table

RODM_close_dbms_connection(DB)
}
}

\keyword{cluster}

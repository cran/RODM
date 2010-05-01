\name{RODM_create_assoc_model}

\alias{RODM_create_assoc_model}

\title{Create an Association Rules model }

\description{
This function creates an Association Rules model. 
}

\usage{
RODM_create_assoc_model(database, 
                        data_table_name, 
                        case_id_column_name,
                        model_name = "AR_MODEL",
                        min_support = NULL,
                        min_confidence = NULL,
                        max_rule_length = NULL,
                        retrieve_outputs_to_R = TRUE,
                        leave_model_in_dbms = TRUE, 
                        sql.log.file = NULL)
}

\arguments{
  \item{database}{Database ODBC channel identifier returned from a call to RODM_open_dbms_connection}
  \item{data_table_name}{Database table/view containing the training dataset.}
  \item{case_id_column_name}{Row unique case identifier in data_table_name.}
  \item{model_name}{ODM Model name.}
  \item{min_support}{Setting that specifies the minimum support for assoc.}
  \item{min_confidence}{Setting that specifies the minimum confidence for assoc.}
  \item{max_rule_length}{Setting that specifies the maximum rule length for assoc.}
  \item{retrieve_outputs_to_R}{Flag controlling if the output results are moved to the R environment.}
  \item{leave_model_in_dbms}{Flag controlling if the model is deleted or left in RDBMS.}
  \item{sql.log.file}{File where to append the log of all the SQL calls made by this function.}    
}

\details{
  
   This function implements the apriori algorithm (Agrawal and Srikant 1994) to find frequent 
  itemsets and generate Association Models (AM). It finds the co-occurrence of items in large 
  volumes of "transactional" data such as in the case of market basket analysis. The rule 
  is an implication where the appearance of a set of items in a transactional record implies 
  another set of items. The groups of items used to form rules must pass a minimum threshold 
  according to how frequently they occur (support) and how often the consequent follows the 
  antecedent (confidence). Association models generate all rules that have support and confidence 
  greater than user-specified thresholds. The AM algorithm is efficient, and scales well with 
  respect to the number of transactions, number of items, and number of itemsets and rules produced.

  For more details on the algotithm implementation, parameters settings and 
  characteristics of the ODM function itself consult the following Oracle documents: ODM Concepts, 
  ODM Developer's Guide, Oracle SQL Packages: Data Mining, and Oracle Database SQL Language 
  Reference (Data Mining functions), listed in the references below. 

}

\value{
 If retrieve_outputs_to_R is TRUE, returns a list with the following elements:
    \item{model.model_settings}{Table of settings used to build the model.}
    \item{model.model_attributes}{Table of attributes used to build the model.}
    \item{ar.rules}{List of the association rules.}
    \item{ar.itemsets}{List of the frequent itemsets.}
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
\code{\link{RODM_drop_model}}}

\examples{

\dontrun{
DB <- RODM_open_dbms_connection(dsn="orcl11g", uid= "rodm", pwd = "rodm")

data(satfruit, package="PASWR")
ards <- satfruit[,c("WH", "BA", "NAR", "COR", "SF", "VI", "PS", "ES", "AF", 
                    "CO", "AR", "AL", "OL")]  # Select subset of attributes
ards[,] <- ifelse(ards[,] == 0, NA, "YES")   # make it sparse, as required by ODM
n.rows <- length(ards[,1])           # Number of rows
row.id <- matrix(seq(1, n.rows), nrow=n.rows, ncol=1, 
                 dimnames= list(NULL, c("ROW_ID"))) # Row id
ards <- cbind(row.id, ards)          # Add row id to dataset 
RODM_create_dbms_table(DB, "ards")   # Push the training table to the database

# Build the association rules model
ar <- RODM_create_assoc_model(
 database = DB, 
 data_table_name = "ards", 
 case_id_column_name = "ROW_ID")

# Inspect the contents of ar to find the rules and itemsets

RODM_drop_model(DB, "AR_MODEL") 
RODM_drop_dbms_table(DB, "ards") 

RODM_close_dbms_connection(DB)
}
}

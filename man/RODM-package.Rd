\name{RODM-package}
\alias{RODM-package}
\alias{RODM}
\docType{package}
\title{
RODM: An Interface to Oracle Data Mining
}
\description{
  Oracle Data Mining (ODM) is an option of Oracle's Relational Database 
Management System (RDBMS) Enterprise Edition (EE). It contains several data 
mining and data analysis algorithms for classification, prediction, regression, 
clustering, associations, feature selection, anomaly detection, feature 
extraction, and specialized analytics. It provides means for the creation, 
management and operational deployment of data mining models inside the 
database environment. 

    RODM is an interface that provides a powerful environment for prototyping 
data analysis and data mining methodologies. It facilitates the prototyping of 
vertical applications and makes ODM and the RDBMS environment easily accessible 
to statisticians and data analysts familiar with R but not experts in SQL. 
In this way it provides an ideal environment for demos and proof of concept 
studies. It also facilitates the benchmarking and testing of functionality 
including statistics and graphics of performance metrics and enables the 
scripting and control of production data mining methodologies from a high-level 
environment.
}
\details{
\tabular{ll}{
Package: \tab RODM\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2010-05-01\cr
License: \tab LGPL\cr
}

RODM is a package that provides access to Oracle's in-database data mining
functionality. 

Requirements\cr
RODM requires the use of an Oracle release 11g database. If you don't have an 
installed Oracle database in place and need to install one from scratch we 
stronly recommend you follow the guidelines in the Oracle Data Mining 
Administrator's Guide. RODM requires R release > 2.10.1.

Connecting to an Oracle database:\cr
RODM_open_dbms_connection\cr
RODM_close_dbms_connection\cr

The above routines are used to establish a connection to an Oracle 11g
database using ODBC.  RODM uses the RODBC package as a means to manage the
database connection.  A data source name must be provided, as well as a
username and password.  The user that is connecting needs sufficient
privileges for performing mining operations in the database. We have tested 
RODM using the Oracle ODBC driver that comes with the Oracle RDBMS. We recommend 
the use of this ODBC driver instead of others.

Pushing data to the database:\cr
RODM_create_dbms_table\cr
RODM_drop_dbms_table\cr

Once a valid database connection has been established, in-database mining can
begin.  If the data to be mined exists within R (e.g., in a data frame), it
first needs to be pushed to the database and placed in a table.  The above
routines leverage the RODBC package to push data to a database table, which
can then be accessed for mining by ODM.


Auxilliary functions (for internal use):\cr
RODM_store_settings\cr
RODM_create_model\cr

The above routines are used under-the-covers when building ODM models.  They
do not need to be invoked directly.  They are present merely to improve
maintainability and modularity.


Building ODM models:\cr
RODM_create_ai_model\cr
RODM_create_assoc_model\cr
RODM_create_dt_model\cr
RODM_create_glm_model\cr
RODM_create_kmeans_model\cr
RODM_create_nb_model\cr
RODM_create_nmf_model\cr
RODM_create_oc_model\cr
RODM_create_svm_model\cr

The above nine routines are used to build ODM models in the database.  They
share many of the same arguments.  All of these routines require a database
connection (as retrieved via RODM_open_dbms_connection) and a table/view in the
database (either pre-existing or created via RODM_create_dbms_table) which
will provide the training data.
All routines accept a case identifier column name.  This is the name of a
column which can be used to uniquely identify a training record.  Most
routines do not need a case identifier, but some may provide extra information
if one is present (e.g., cluster assignments).
All supervised algorithms require a target column name.
A model name can be specified (or defaults to an algorithm-specific model
name).  When created, the model will exist in Oracle as a database schema object.
Most algorithms accept a parameter to direct ODM to enable automatic data
preparation (default TRUE).  This feature will request that ODM prepare data
as befitting individual algorithm needs (e.g., outlier treatment, binning,
normalization, missing value imputation).
Many algorithms accept a number of expert settings.  These expert settings
will differ from algorithm to algorithm, and ODM is designed to identify
values for these settings without user input, hence they do not need to be
specified by the user in most situations.
When the models are created in the database, information regarding the
models can be retrieved and returned to the R environment.  The
retrieve_outputs_to_R parameter tells RODM whether or not this information
should be pulled back into R for further analysis in R.
As these models are database schema objects, they can be left in the database
for future use.  They can be applied to new data as desired.  The default
behavior is to leave the models in the database, but they can be automatically
cleaned up by changing the leave_model_in_dbms parameter.  If a model with the
same name already exists in the database schema when another is being created,
the previous model will be automatically dropped.
Finally, the RODM package is envisioned as a quick proof of concept mechanism,
with the potential of deploying the resulting methodology wholly within
Oracle.  As such, it is necessary to capture the SQL that would be used in the
database.  The sql.log.file parameter can be used to have RODM produce a file
with the relevant SQL statements that comprise the work being performed.


Further operations involving ODM models:\cr
RODM_list_dbms_models\cr
RODM_apply_model\cr
RODM_drop_model\cr

Finally, there are a few routines involving ODM models once they are built.
The list of accessible ODM models can be retrieved, and individual models can
be dropped.
Models (other than those used for Attribute Importance and Associations) can
be applied to new data in the database.  Regression models will produce the
expected value given the input variables.  NMF models will produce the full
set of feature values for each input case (as well as a column which contains
the identity of the feature with the highest value for each case).
Classification models will produce the probability distribution across target
classes for each case, as well as a column indicating the winning class.
Clustering models will produce the probability distribution across clusters
for each case, as well as a column indicating the most likely cluster
assignment.
In all cases, additional columns from the test/apply dataset can be included
in the output via the supplemental_cols parameter.  It is necessary to provide
some information here if there is a desire to link the results back to the
original data.  Either a case identifier should be provided, or the list of
columns which will yield sufficient information for future analysis.

}

\author{
Pablo Tamayo \email{pablo.tamayo@oracle.com}

Ari Mozes \email{ari.mozes@oracle.com}

Maintainer: Pablo Tamayo \email{pablo.tamayo@oracle.com}
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

Richard O. Duda, Peter E. Hart, David G. Stork, Pattern Classification (2nd Edition). John Wiley & Sons 2001.

Wikipedia entry for Oracle Data Mining. 
\url{http://en.wikipedia.org/wiki/Oracle_Data_Mining}

P. Tamayo, C. Berger, M. M. Campos, J. S. Yarmus, B. L.Milenova, A. Mozes, M. Taft, M. Hornick, R. Krishnan, S.Thomas, M. Kelly, D. Mukhin, R. Haberstroh, S. Stephens and J. Myczkowski. Oracle Data Mining - Data Mining in the Database Environment. In Part VII of Data Mining and Knowledge Discovery Handbook, Maimon, O.; Rokach, L. (Eds.) 2005, p315-1329, ISBN-10: 0-387-24435-2.

Oracle Data Mining: Mining Gold from Your Warehouse, (Oracle In-Focus series), by Dr. Carolyn Hamm.
}

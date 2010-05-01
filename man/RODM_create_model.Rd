\name{RODM_create_model}

\alias{RODM_create_model}

\title{Create an ODM model }

\description{
Helper function to create an Oracle Data Mining model
}

\usage{
RODM_create_model(database, 
                  model_name,          
                  mining_function_type,              
                  data_table_name,                   
                  case_id_column_name = "",          
                  target_column_name = "",           
                  retrieve_outputs_to_R = TRUE,      
                  sql.log.file = NULL)               
}

\arguments{
  \item{database}{Database ODBC channel identifier returned from a call to RODM_open_dbms_connection}
  \item{model_name}{ODM Model name.}
  \item{mining_function_type}{Mining function to use, e.g., "dbms_data_mining.classification"}
  \item{data_table_name}{Database table/view containing the training dataset.}
  \item{case_id_column_name}{Row unique case identifier in data_table_name.}
  \item{target_column_name}{Name of the target column (for supervised models).}
  \item{retrieve_outputs_to_R}{Flag controlling if the output results are moved to the R environment.}
  \item{sql.log.file}{File to append the log of all the SQL calls made by this function.}
}

\details{
This is the generic function for creating ODM models.

It is not intended to be called directly by the user, but rather via the
RODM_create_XX_model functions.
}

\value{
 If retrieve_outputs_to_R is TRUE, returns a list with the following elements:
    \item{model.model_settings}{Table of settings used to build the model.}
    \item{model.model_attributes}{Table of attributes used to build the model.}
}

\author{
Pablo Tamayo \email{pablo.tamayo@oracle.com}

Ari Mozes \email{ari.mozes@oracle.com}
}

\references{
Oracle Data Mining Concepts 11g Release 1 (11.1)
\url{http://download.oracle.com/docs/cd/B28359_01/datamine.111/b28129/toc.htm}

Oracle Database PL/SQL Packages and Types Reference 11g Release 1 (11.1)
\url{http://download.oracle.com/docs/cd/B28359_01/appdev.111/b28419/d_datmin.htm#ARPLS192}
}

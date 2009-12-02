\name{RODM_store_settings}

\alias{RODM_store_settings}

\title{Store Mining Model settings}

\description{
This function stores model build settings in the the database table RODM_SETTINGS_TABLE.
}

\usage{
RODM_store_settings(database, 
                    rodmset, 
                    auto_data_prep, 
                    sql.log.file = NULL,
                    bias_frame = NULL,
                    bias_weights = FALSE)
}

\arguments{
  \item{database}{Database ODBC channel identifier returned from a call to RODM_open_dbms_connection}
  \item{rodmset}{Data frame containing the settings to be used for building a model.}
  \item{auto_data_prep}{Whether or not ODM should invoke automatic data preparation for the build.}
  \item{sql.log.file}{File to append the log of all the SQL calls made by this function.}
  \item{bias_frame}{Data frame containint bias information (priors/costs/weights).}
  \item{bias_weights}{Priors or Weights for 2-column bias frame.}
}

\details{
This is a generic function for persisting settings that drive model build.

It is not intended to be called directly by the user, but rather via the
RODM_create_XX_model functions.
}

\value{
  None.
}

\author{
Pablo Tamayo \email{pablo.tamayo@oracle.com}

Ari Mozes \email{ari.mozes@oracle.com}
}

\references{
Oracle Database PL/SQL Packages and Types Reference 11g Release 1 (11.1)
\url{http://download.oracle.com/docs/cd/B28359_01/appdev.111/b28419/d_datmin.htm#ARPLS192}
}

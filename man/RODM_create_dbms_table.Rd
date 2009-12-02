\name{RODM_create_dbms_table}

\alias{RODM_create_dbms_table}

\title{Create a table in the Oracle Database}

\description{Create and populate a table in the Oracle Database using a dataframe.}

\usage{
RODM_create_dbms_table(database, 
                       data_table_name)
}

\arguments{
  \item{database}{Database ODBC channel identifier returned from a call to RODM_open_dbms_connection}
  \item{data_table_name}{Data frame containing the input dataset.  Also the name of the table to be created in the database.}
}

\details{
This functions moves a data frame to a relational table in the
Oracle database using RODBC's sqlSave function.
A table with the same name as the data frame will be created in the RDBMS to
hold the data in the data frame.
If the table already exists, it will first be dropped.

The data frame can contain attributes of numeric: double or integer, logical, character or factor type. 
The conversions performed by this function when transfering data to the RDBMS are 
as follows:

\preformatted{
  R data frame column type:  RDMS type:
  ----------------------------------------
    numeric/double           FLOAT(126)
    numeric/integer          NUMBER(38)
    logical                  VARCHAR2(255)
    character                VARCHAR2(255)
    factor                   VARCHAR2(255)
}

}

\value{
  None.
}

\author{
Pablo Tamayo \email{pablo.tamayo@oracle.com}

Ari Mozes \email{ari.mozes@oracle.com}
}

\references{
Oracle Data Mining Administrator's Guide 11g Release 1 (11.1) 
\url{http://download.oracle.com/docs/cd/B28359_01/datamine.111/b28130/toc.htm}
}

\seealso{\code{\link{RODM_drop_dbms_table}}}

\examples{
\dontrun{
data(titanic3, package="PASWR")
DB <- RODM_open_dbms_connection(dsn="orcl11g", uid="rodm", pwd="rodm")

# Move the titanic3 data frame to a new table in the Oracle database
RODM_create_dbms_table(DB, "titanic3")

# Now drop the table (just to clean up from this example)
RODM_drop_dbms_table(DB, "titanic3")

RODM_close_dbms_connection(DB)
}
}


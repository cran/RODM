\name{RODM_drop_dbms_table}

\alias{RODM_drop_dbms_table}

\title{Drops a table in the Oracle Database}

\description{This function drops a table in the Oracle Database.}

\usage{
RODM_drop_dbms_table(database, data_table_name) 
}

\arguments{
  \item{database}{Database ODBC channel identifier returned from a call to RODM_open_dbms_connection}
  \item{data_table_name}{Oracle database table to be dropped.}
}

\details{This function drops a table in the Oracle Database.}

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

\seealso{\code{\link{RODM_create_dbms_table}}}

\examples{
\dontrun{
data(titanic3, package="PASWR")
DB <- RODM_open_dbms_connection(dsn="orcl11g", uid="rodm", pwd="rodm")
RODM_create_dbms_table(DB, "titanic3")

# Drop the titanic3 database table
RODM_drop_dbms_table(DB, "titanic3")

RODM_close_dbms_connection(DB)
}
}


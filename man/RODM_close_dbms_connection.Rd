\name{RODM_close_dbms_connection}

\alias{RODM_close_dbms_connection}

\title{Close a connection to an Oracle Database}

\description{This function closes a connection to an Oracle Database.}

\usage{
RODM_close_dbms_connection(
    database)
}

\arguments{
  \item{database}{Database ODBC channel identifier returned from a call to \code{\link{RODM_open_dbms_connection}}}
}

\details{This functions closes an Oracle Database ODBC channel by calling
  the RODBC function: odbcClose.
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

\seealso{\code{\link{RODM_open_dbms_connection}}}

\examples{
# Given a database user rodm, establish a connection to the orcl11g
# database.
# The database user would need privileges as described above, and could
# have been created in a fashion similar to:
# grant create session, create table, create view, create mining model, 
#       unlimited tablespace to rodm identified by rodm;
\dontrun{
DB <- RODM_open_dbms_connection(dsn="orcl11g", uid="rodm", pwd="rodm")

# Close the connection to the database.
RODM_close_dbms_connection(DB)
}
}



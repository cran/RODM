\name{RODM_open_dbms_connection}

\alias{RODM_open_dbms_connection}

\title{Open a connection to an Oracle Database}

\description{
This function opens a connection to an Oracle Database.
}

\usage{
RODM_open_dbms_connection(
   dsn,  
   uid = "",
   pwd = "")
}

\arguments{
  \item{dsn}{ODBC Data Source Name.}
  \item{uid}{Database user id.}
  \item{pwd}{Password for the database user.}
}

\details{
This functions opens an ODBC channel to an Oracle database by calling
the RODBC function: odbcConnect(dsn=dsn, uid=uid, pwd=pwd, case="oracle").

It validates that the database is Oracle version 11, that Oracle Data
Mining is installed, and that the connecting user has appropriate privileges for
mining in the database.

The necessary privileges for connecting and mining are as follows: CREATE SESSION, CREATE TABLE, CREATE VIEW, and CREATE MINING MODEL.
The connecting user will also need quota in a tablespace.

The first time RODM_open_dbms_connection is invoked for a given database user,
this function will also create a temporary table to hold settings
that are used to build ODM models (table name RODM_SETTINGS_TABLE).
}

\value{
  An Oracle database ODBC channel.
}


\author{
Pablo Tamayo \email{pablo.tamayo@oracle.com}

Ari Mozes \email{ari.mozes@oracle.com}
}

\references{
Oracle Data Mining Administrator's Guide 11g Release 1 (11.1) 
\url{http://download.oracle.com/docs/cd/B28359_01/datamine.111/b28130/toc.htm}
}

\seealso{\code{\link{RODM_close_dbms_connection}}}

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



usethis::edit_r_environ()
Sys.getenv("userid")
Sys.getenv("pwd")

keyring::key_set("pwd")
keyring::key_get("pwd")

rm(list = ls())
cat("\014")


library(here)
library(RODBC)
library(tidyverse)
library(dtplyr)

cn <- odbcDriverConnect(connection="DSN=dsnNAME;trusted_connection=yes;")
  tblList <- sqlTables(cn)
odbcClose(cn)


write.csv(tblList, here("jpodTables.csv"),row.names=FALSE, na="")

library(DBI)
jpod <- dbConnect(odbc::odbc(), "dsnNAME",uid=Sys.getenv("USERNAME"),pwd=Sys.getenv("pwd"))
  # listTBLS <- dbListTables(jpod, table_name = "IDX%")
  
  result <- dbSendQuery(jpod, "SELECT * FROM UJ.QM_QUEUE ORDER BY QUEUE_ID")
  
  # Retrieve the first 100 results
  first_100 <- dbFetch(result, n = 100)
  
  # Retrieve the rest of the results
  rest <- dbFetch(result)
  
dbDisconnect(jpod)


# listTBLS

#include access
testdb <- file.path(c(paste0(here(),"/TestDB.accdb")))

channel <- odbcConnectAccess2007(testdb)
qryOut <- sqlFetch(channel,"qry_SIGassoc")
odbcClose(channel)



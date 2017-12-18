# install SQLTable branch of odbc dev for selecting schema
devtools::install_github("rstats-db/odbc@SQLTable")
# https://github.com/r-dbi/odbc/issues/91

# load data from previous steps (not run)
load("data/data-prep.Rda")
load("data/impute-missing.Rda")
load("data/calculate-eui.Rda")

# connect to MS SQL Server database
library(tidyverse)
library(odbc)
library(DBI)
con <- dbConnect(odbc(), Driver = "SQL Server", Server = getOption("ms_sql_server"), Database = "NYEMEO88")

# write relevant tables to db
dbWriteTable(con, dbId(con, "building_metadata", "EO88"), bldg_meta, row.names = FALSE)
dbWriteTable(con, dbId(con, "building_filingdata", "EO88"), bldg_sfy, row.names = FALSE)
dbWriteTable(con, dbId(con, "noaa_regions", "EO88"), regions, row.names = FALSE)
dbWriteTable(con, dbId(con, "weather_monthly", "EO88"), weather, row.names = FALSE)
dbWriteTable(con, dbId(con, "consumption_filingdata_asis", "EO88"), eo88, row.names = FALSE)
dbWriteTable(con, dbId(con, "consumption_filingdata_asis_summed", "EO88"), select(eo88_summed, -Demand), row.names = FALSE)
dbWriteTable(con, dbId(con, "consumption_filingdata_imputed", "EO88"), eo88_imp, row.names = FALSE)
dbWriteTable(con, dbId(con, "consumption_filingdata_imputed_summed", "EO88"), select(eo88_final, -Demand), row.names = FALSE)
dbWriteTable(con, dbId(con, "consumption_filingdata_final", "EO88"), select(eo88_final, -Demand, -Cost), row.names = FALSE)

# disconnect from db
dbDisconnect(con)

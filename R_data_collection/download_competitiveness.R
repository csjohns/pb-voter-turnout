
library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)
library(RPostgreSQL)

source("credentials_comp.R") # loads the access credentials



drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv, user = username, password = password, dbname = db.name, host = hostname, port = port) #establish connection to DB

dbDisconnect(con) #closes the connection to the database


require(RMySQL)

source("credentials.R") # loads the access credentials
source("dbDownload.R")

ed <- dbDownload(table = "electiondistricts", username = username, password = password, dbname = db.name, host = hostname, port = port)
data <- dbDownload(table = "pb", username = username, password = password, dbname = db.name, host = hostname, port = port)
rm(password, username, hostname, db.name, port) # if you want to remove the credentials from your environment 


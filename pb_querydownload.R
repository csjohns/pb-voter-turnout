library(RMySQL)

source("credentials.R") # loads the access credentials

con <- dbConnect(MySQL(), user=username, password=password, dbname=db.name, host=hostname, port = port) #establish connection to DB
rs <- dbSendQuery(con, "SELECT * FROM pb;") #defines the query; this SQL here is the very basic query to pull the whole table, you can send any SQL that you like
data <- dbFetch(rs, n = -1) # -1 indicates to fetch all rows returned by query; can limit to only the first n rows (default appears to be 500)
status_completed <- dbHasCompleted(rs)  # check whether query has completed with all results returned
dbClearResult(rs)  #clears the resources used by the query - results saved to dataframe stay
dbDisconnect(con) #closes the connection to the database

rm(password, username, hostname, db.name, port) # if you want to remove the credentials from your environment 
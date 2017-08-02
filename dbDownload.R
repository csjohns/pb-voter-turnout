dbDownload <- function(table,...){
  require(RMySQL)
  con <- dbConnect(MySQL(),...) #establish connection to DB
  query <- paste("SELECT * FROM ", table, ";",sep = "")
  rs <- dbSendQuery(con, query) #defines the query; this SQL here is the very basic query to pull the whole table, you can send any SQL that you like
  db <- dbFetch(rs, n = -1) # -1 indicates to fetch all rows returned by query; can limit to only the first n rows (default appears to be 500)
  status_completed <- dbHasCompleted(rs)  # check whether query has completed with all results returned
  dbClearResult(rs)  #clears the resources used by the query - results saved to dataframe stay
  dbDisconnect(con) #closes the connection to the database
  
  return(db)
}
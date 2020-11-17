### pushing voter file up to sonya
library(data.table)
library(RMySQL)

source("credentials.R") # loads the access credentials
con <- dbConnect(MySQL(), username = username, password = password, dbname = db.name, host = hostname, port = port) #establish connection to DB


voterfile <- fread("PersonFile20180426-11056504994/PersonFile20180426-11056504994.txt")
gc()

rm(voterfiledb)
gc()
voterfiledb <- as.data.frame(voterfile[7000001:nrow(voterfile)]) #0-1, 1-2, 2-3, 3-4, 4-5, 5-6, 6-7, 7+
gc()
dbWriteTable(con, name = "voterfile52018", value = voterfiledb, overwrite = FALSE, append = TRUE, row.names = FALSE)
# dbDisconnect(con)
# rm(password, username, hostname, db.name, port) # if you want to remove the credentials from your environment 

dbGetQuery(con, 'select count(*) from voterfile52018')
dbGetQuery(con, 'select count(distinct DWID) from voterfile52018')
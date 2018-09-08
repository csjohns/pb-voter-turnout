#########################################################################################################################################
####
#### This script adds the 2017 turnout columns to the pb table that I have regenerated from the raw data.
####   And uploads (with replacement) the cleaned table with the accurate imputed district identifiers 
####
#### *First run ExploringSonyaMaster.R and addingnew23files.R to regenerate the original table*
#### (I've confirmed it's all the same with adjust district info)
####
#########################################################################################################################################


pb_local <- pb_imputed #copy pb_imputed to modify

# then download current version from server:

source("credentials.R") # loads the access credentials
source("dbDownload.R")

### Load PB data ### -------------------------------------------------------------------------------------

pb <- dbDownload(table = "pb", username = username, password = password, dbname = db.name, host = hostname, port = port)
rm(password, username, hostname, db.name, port) # if you want to remove the credentials from your environment

# votes2017 <- pb %>% select(VANID, ends_with("_2017"))

pb_local <- pb %>%
  select(-pbdistrict) %>% 
  left_join(pb_local, .)

all.equal(sort(names(pb)), sort(names(pb_local)))

all.equal(pb$VANID, pb_local$VANID)
all.equal(pb$pb_2016, pb_local$pb_2016)
all.equal(pb$CensusTract, pb_local$CensusTract)
all.equal(pb$DoB, pb_local$DoB)

## keeping the regenerated file as original because it has the more accurate DoBs (left join pb to pb_local)

# backup the server file just in case
write.csv(pb, "backup/pb_download_9-8-18.csv", row.names = FALSE)


### Write updated data to server
library(DBI)
library(RMySQL)
source("credentials.R") # loads the access credentials

con <- dbConnect(MySQL(), username = username, password = password, dbname = db.name, host = hostname, port = port) #establish connection to DB
dbWriteTable(con, name = "pb", value = as.data.frame(pb_local), overwrite = TRUE, row.names = FALSE)
dbDisconnect(con)
rm(password, username, hostname, db.name, port) # if you want to remove the credentials from your environment

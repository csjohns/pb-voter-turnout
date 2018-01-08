#####################################################################################################################################################################
## PB Voter Turnout Analysis
## December 2017 - Data processing of new match, uploading to the server as pb_update


### Preliminaries ------------------------------------------------------------------------------------------------------------------------------------------------

library(RColorBrewer)
library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)
library(margins)
library(ggplot2)

#source("credentials.R") # loads the access credentials
#source("dbDownload.R")

stringNAs <- function(x){
  ifelse(x, "", NA)
}

pbnyc <- read.csv(file = "pbnyc_district_votes.csv", as.is = TRUE)

#### Read in PB voter tally ------------------------------------------------------------------------------------------------------------------------------------------------

### adding in our "new" voters 
new_pb <- read.delim("PBNYCvoters-Source.txt", header = TRUE, as.is = TRUE)

new_van <- read.delim("PBNYCvoters-VoterfileData.xls", header = TRUE, as.is = TRUE)

pb_new <- new_pb %>% 
  rename(pb_cycle = COL10.CYCLE,
         pb_district = COL7.PB.VOTE.DISTRICT) %>%
  select(DWID, pb_cycle, pb_district) %>%
  inner_join(new_van) %>%
  distinct()

names(pb_new) <- names(pb_new) %>% 
  str_replace("General", "g_20") %>%
  str_replace("PresidentialPrimary", "pp_20") %>%
  str_replace("Primary", "p_20")

pb <- pb_new %>%
  select(-starts_with("Special"), -ends_with("Party"))

# Renaming to match existing PB table
pb <- pb %>% 
  rename(Ethnicity = EthnicCatalistName,
         DoB = DOB,
         DoR = DateReg,
         RegStatus = RegistrationStatusName,
         County = CountyName,
         # City = CityName,
         Zip = Zip5,
         Lat = Latitude,
         Long = Longitude,
         NYCCD = CityCouncilName,
         CensusTract = CensusTractName,
         pbdistrict = pb_district,
         pbcycle = pb_cycle) %>%
  mutate(County = recode(County, Bronx = "BRONX" , Kings = "KINGS", `New York` = "NEW YORK", Queens = "QUEENS", Richmond = "RICHMOND"))


# add cycle years, not just cycle id
pb <- pbnyc %>%
  select(pbnycCycle, voteYear) %>% 
  distinct() %>%
  rename(pbcycle = pbnycCycle,
         pbyear = voteYear) %>%
  left_join(pb,.)

pb <- pb %>% mutate(pbyear = paste0("pb_", pbyear))
## now just need to spread out the pb!
pb <- pb %>%
  mutate(voted = 1) %>%
  spread(pbyear, voted, fill = 0)


### need to get year and pb/electoral votes into order here next - 


### Read in PB data from Sonya's DB ------------------------------------------------------------------------------------------------------------------------------------------------

#pb_orig <- dbDownload(table = "pb", username = username, password = password, dbname = db.name, host = hostname, port = port)
#rm(password, username, hostname, db.name, port) # if you want to remove the credentials from your environment 
#pb <- pb_orig

## Reshape a la pbvoters_regression


pb <- pb %>% 
  group_by(VANID) %>%
  mutate(DoB = mdy(DoB),
         totpb = sum(pb_2013, pb_2014, pb_2015 , pb_2016, na.rm = T)
  )

pb <- pbnyc %>%
  rename(pbdistrict = district) %>%
  group_by(pbdistrict) %>%
  summarize(start_year = min(voteYear)) %>%
  left_join(pb, .)

con <- dbConnect(MySQL(), username = username, password = password, dbname = db.name, host = hostname, port = port) #establish connection to DB
dbWriteTable(con, name = "pb_update", value = as.data.frame(pb), overwrite = TRUE)
dbDisconnect(con)

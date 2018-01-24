library(RColorBrewer)
library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)
library(margins)
library(ggplot2)
library(DBI)

source("credentials.R") # loads the access credentials
source("dbDownload.R")

findDistrict <- function(ds){
  if(length(unique(na.omit(ds))) == 1) {return(unique(na.omit(ds)))} else
  {return(NA)}
}

sonya <- read.csv("SonyaMasterPeople.csv", as.is = TRUE)

glimpse(sonya)

table(sonya$Cycle2district, sonya$Cycle3district, useNA = 'ifany')
table(sonya$Cycle4district, sonya$Cycle3district, useNA = 'ifany')
table(sonya$Cycle4district, sonya$Cycle5district, useNA = 'ifany')
table(sonya$Cycle2district, sonya$Cycle5district, useNA = 'ifany')
table(sonya$Cycle3district, sonya$Cycle5district, useNA = 'ifany')

sonya <- sonya %>% 
  gather(cycledistrict, district, starts_with("Cycle")) %>% 
  group_by(VANID) %>%
  #mutate(testdist = length(unique(na.omit(district)))) %>% View()
  mutate(pbdistrict = findDistrict(district)) %>% #glimpse()
  spread(cycledistrict, district) %>%
  select(-starts_with("Cycle"))
  
sonya <- sonya %>%
  rename(pb_2012 = PBvoter2012,
         pb_2013 = PBvoter2013,
         pb_2014 = PBvoter2014,
         pb_2015 = PBvoter2015,
         pb_2016 = PBvoter2016) %>%
  select(-PBvoter2017)

sonya_limited <- sonya %>% 
  filter(!is.na(pbdistrict))

pbnyc <- read.csv(file = "pbnyc_district_votes.csv", as.is = TRUE)

pb <- sonya

names(sonya) <- names(sonya) %>% 
  str_replace("General", "g_20") %>%
  str_replace("PresidentialPrimary", "pp_20") %>%
  str_replace("Primary", "p_20")

pb <- sonya %>%
  select(-starts_with("Special"), -ends_with("Party"))


# Renaming to match existing PB table
pb <- pb %>% 
  rename(DoB = DOB,
         DoR = DateReg,
         Zip = Zip5
         #pbcycle = pb_cycle
         ) %>%
  mutate(County = recode(County, Bronx = "BRONX" , Kings = "KINGS", `New York` = "NEW YORK", Queens = "QUEENS", Richmond = "RICHMOND"))  

# function to fill blanks with zeros
replZero <- function(x){
  if(is.na(x)) {return(0)} else if (!is.na(x)){
    return(x)}
  }
  
# replace blanks with zeros in pb voting vars
pb <- pb %>%
  mutate_at(vars(starts_with("pb_")), funs(replZero))

### Write cleaned data to server

# con <- dbConnect(MySQL(), username = username, password = password, dbname = db.name, host = hostname, port = port) #establish connection to DB
# dbWriteTable(con, name = "pb", value = as.data.frame(pb), overwrite = TRUE)
# dbDisconnect(con)
# 

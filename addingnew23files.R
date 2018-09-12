########################################################################################################################
### This is the file that processes the new voters from district 23 and adds them and/or their votes to the PB table.  
###    Should be *run after ExploringSonyaMaster.R* which processes Sonya's match output (that output was most recent
###      at the time that this additional data was brought in, and resulted in a cleaner join than pulling from server)
###  
###  The file was created in Mar 2018.  
###  
###  Sept. 2018 - refines the process for imputing  missing districts to make sure that it only includes valid pb districts
###
########################################################################################################################

# what needs to be done to get the new data in to the old
# 1) create new column pb_2013 new, and flag for new_data
# 2) select only VANID
# 3) update if  pb_2013_new == 1, then pb_2013 == 1 == 0 or NA, fill with 
# check to see how many actually are different

pb23_13 <- read.delim("new23/2013pbnycD23voters20180301-19023894851.txt", as.is = TRUE)

pb23_13 <- pb23_13 %>% 
  #select(-CityName, -DateReg) %>% 
  rename(Ethnicity = EthnicCatalistName,
         DoB = DOB,
         DoR = DateReg,
         #       RegStatus = RegistrationStatusName,
         County = CountyName,
         City = CityName,
         NYCCD = CityCouncilName,
         CensusTract = CensusTractName,
         VANID = `Voter.File.VANID`,
         ED = PrecinctName)


pb23_13 <- pb23_13  %>%
  mutate(County = recode(County, Bronx = "BRONX" , Kings = "KINGS", `New York` = "NEW YORK", Queens = "QUEENS", Richmond = "RICHMOND")) 

names(pb23_13) <- names(pb23_13) %>% 
  str_replace("General", "g_20") %>%
  str_replace("PresidentialPrimary", "pp_20") %>%
  str_replace("Primary", "p_20")

pb23_13 <- pb23_13 %>%
  select(-starts_with("Special"), -ends_with("Party")) %>%
  select(-DWID, -p_2011,
         -starts_with("Reported"))

pb23_13 <- pb23_13 %>% 
  mutate(pb_2013 = 1,
         pbdistrict = 23)


################ REPLICATING FOR THE 2016 VOTERS ######--------------------------------------------------------------------------------------
pb23_16 <- read.delim("new23/2016pbnycD23voters20180301-20797589892.txt", as.is = TRUE)

pb23_16 <- pb23_16 %>% 
  #select(-CityName, -DateReg) %>% 
  rename(Ethnicity = EthnicCatalistName,
         DoB = DOB,
         DoR = DateReg,
         #       RegStatus = RegistrationStatusName,
         County = CountyName,
         City = CityName,
         NYCCD = CityCouncilName,
         CensusTract = CensusTractName,
         VANID = `Voter.File.VANID`,
         ED = PrecinctName)

pb23_16 <- pb23_16  %>%
  mutate(County = recode(County, Bronx = "BRONX" , Kings = "KINGS", `New York` = "NEW YORK", Queens = "QUEENS", Richmond = "RICHMOND"))

names(pb23_16) <- names(pb23_16) %>% 
  str_replace("General", "g_20") %>%
  str_replace("PresidentialPrimary", "pp_20") %>%
  str_replace("Primary", "p_20")

pb23_16 <- pb23_16 %>%
  select(-starts_with("Special"), -ends_with("Party")) %>%
  select(-DWID,
         -starts_with("Reported"))

pb23_16 <- pb23_16 %>% 
  mutate(pb_2016 = 1,
         pbdistrict = 23)


### MODIFYING PB FILES, updating existing voters with new votes, adding new voters

pb <- pb %>% 
  mutate(pb_2016 = ifelse(VANID %in% pb23_16$VANID, 1, pb_2016))

pb_update <- pb23_16 %>% 
  filter(!VANID %in% pb$VANID) %>% 
  bind_rows(pb, .)

pb_update <- pb_update %>% 
  mutate(pb_2013 = ifelse(VANID %in% pb23_13$VANID, 1, pb_2013))

pb_update <- pb23_13 %>% 
  filter(!VANID %in% pb_update$VANID) %>% 
  bind_rows(pb_update, .)

pb <- pb_update %>% 
  select(-p_2017)


### Imputing pb districts for those that are NA, replacing NA with the voter's current district
###   *IF* that district was a valid PB district at the time of their first vote.
pbnyc_history <- read.csv(file = "pbnyc_district_votes.csv", as.is = TRUE) %>% 
  rename(pbdistrict = district) %>%
  group_by(pbdistrict) %>%
  summarize(start_year = min(voteYear)) %>% 
  filter(start_year < 2017)

pb <- pb %>% 
  mutate(first_pb = ifelse(pb_2012 == 1, 2012, 
                           ifelse(pb_2013 == 1, 2013,
                                  ifelse(pb_2014 == 1, 2014,
                                         ifelse(pb_2015 == 1, 2015,
                                                ifelse(pb_2016 == 1, 2016, NA))))))

## replace NA pbdistricts wtih NYCCD, then filter back to NA if start data for that district is 
pb_imputed <- pb %>% 
  mutate(NYCCD = ifelse(is.na(NYCCD), pbdistrict, NYCCD),
         pbdistrict_imputed = ifelse(is.na(pbdistrict) & NYCCD %in% pbnyc_history$pbdistrict, NYCCD, pbdistrict),
         imputed = ifelse(is.na(pbdistrict) & NYCCD %in% pbnyc_history$pbdistrict, 1, 0))
pb_imputed <- pb_imputed %>% 
  left_join(pbnyc_history, by = c("pbdistrict_imputed" = "pbdistrict")) %>% 
  mutate(pbdistrict_imputed = ifelse(imputed == 1 & start_year > first_pb, NA, pbdistrict_imputed)) %>% 
  select(-imputed, -first_pb, -start_year, -pbdistrict) %>% 
  rename(pbdistrict = pbdistrict_imputed)
    
  
### Write updated data to server
# library(DBI)
# library(RMySQL)
# source("credentials.R") # loads the access credentials
#
# con <- dbConnect(MySQL(), username = username, password = password, dbname = db.name, host = hostname, port = port) #establish connection to DB
# dbWriteTable(con, name = "pb", value = as.data.frame(pb_imputed), overwrite = TRUE, row.names = FALSE)
# dbDisconnect(con)
# rm(password, username, hostname, db.name, port) # if you want to remove the credentials from your environment 

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
  mutate(pb_2013 = 1)


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
  mutate(pb_2016 = 1)


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



load("pbdistricts.Rdata")
pbdistricts <- na.omit(pbdistricts)
pb <- pb %>% mutate(NYCCD = ifelse(is.na(NYCCD), pbdistrict, NYCCD),
                    pbdistrict = ifelse(is.na(pbdistrict) & NYCCD %in% pbdistricts, NYCCD, pbdistrict))

### Write updated data to server
# library(DBI)
# library(RMySQL)
# source("credentials.R") # loads the access credentials
#
# con <- dbConnect(MySQL(), username = username, password = password, dbname = db.name, host = hostname, port = port) #establish connection to DB
# dbWriteTable(con, name = "pb", value = as.data.frame(pb), overwrite = TRUE, row.names = FALSE)
# dbDisconnect(con)
# rm(password, username, hostname, db.name, port) # if you want to remove the credentials from your environment 

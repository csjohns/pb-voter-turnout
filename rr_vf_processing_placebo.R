##############################################################################################################################
###
### NYC PB Voters Project 
### Loading voterfile and PB data, processing and appending auxiliary information
### Carolina Johnson
### 
### 1. Download PB data from server
### 2. Load PB district info from disk
### 3. Geoprocesing PB voters and districts
### 4. Load voterfile info from disk , geoprocesses
### 5. Rename/clean voterfile 
### 6. Join PB participation data to voterfile
### ....
### 10. Create age groups
### 11. Save to RDS
###
##############################################################################################################################

# Load libraries


library(dplyr)
library(tidyr)
library(glue)
library(lubridate)
library(stringr)
library(data.table)
# 
# source("credentials.R") # loads the access credentials
# source("dbDownload.R")

stringNAs <- function(x){
  ifelse(x, "", NA)
}
# 
# conv19c <- function(s, ft = "%m/%d/%y"){
#   as.Date(format(as.Date(s,format=ft), "19%y%m%d"), "%Y%m%d")
# }
### Load PB data ### -------------------------------------------------------------------------------------

# pb <- dbDownload(table = "pb", username = username, password = password, dbname = db.name, host = hostname, port = port)

# load("pb_orig.Rdata")
#pb <- pb_orig 
#rm(pb_orig)
# pb <- pb %>% select(-DWID) %>%
#   filter(DoR != "" & !is.na(DoR)) %>% 
#   mutate_at(vars(starts_with("pb_2")), replace_na, 0) # this line is to deal with the fact taht the row appended dist 23 voters (who didn't otherwise exist)
# pb <- pb %>% mutate(DoB = mdy(DoB),
#                     pb = 1)

pb <- readRDS("data/cleaned_R_results/pb.rds")
# limit to only 23/39 and 2016 districts
pbnyc <- read.csv(file = "pbnyc_district_votes.csv", as.is = TRUE)
pb2016 <- pbnyc %>% filter(districtCycle == 1 & voteYear == 2016) 

### Load full voterfile data ### -------------------------------------------------------------------------------------
### Limit it to only non-PB districts and VANIDS

pbdistricts <- unique(pbnyc$district)

rm(pbnyc)
# 
# ## loading full voter file
# con <- dbConnect(MySQL(), username = username, password = password, dbname = db.name, host = hostname, port = port) #establish connection to DB
# voterfile <- glue_sql("SELECT * FROM voterfile52018 
#                       WHERE RegistrationStatusName NOT IN ('Applicant', 'Dropped', 'Unregistered')
#                       AND DateReg <> ''
#                       AND (CityCouncilName IS NULL 
#                       OR CityCouncilName NOT IN ({nyccds*}))",
#                       nyccds = pbdistricts,
#                       .con = con) %>% 
#   dbGetQuery(con, .)
# dbDisconnect(con)
# rm(password, username, hostname, db.name, port) # if you want to remove the credentials from your environment 
# save(voterfile, file = "voterfile_noPB.Rdata ")
# voterfile <- voterfile %>% 
#   filter(DateReg != "" & !`Voter File VANID` %in% pb$VANID) 

### Load full voterfile data ### -------------------------------------------------------------------------------------
### Limit it to only non-PB districts and VANIDS

voterfile <- fread("PersonFile20180426-11056504994/PersonFile20180426-11056504994.txt")
voterfile <- voterfile[! RegistrationStatusName %in% c('Applicant', 'Dropped', 'Unregistered') & DateReg != ""]

set.seed(92018)
source("vf_gis_nyccdmatch.R")
source("pb_cleanup_addnyccd_foranalysis.R")

## identify PB districts; remove PB voters
voterfile <- voterfile %>% 
  mutate(pbdistrict = as.numeric(CityCouncilName %in% pbdistricts)) %>% 
  filter(!`Voter File VANID` %in% pb$VANID & !is.na(CityCouncilName))

# voterfile <- fread("PersonFile20180426-11056504994/PersonFile20180426-11056504994.txt")

# ## filtering voterfile to only actual registered non-pb voters and 
# voterfile <- voterfile[!CityCouncilName %in% pbdistricts & !`Voter File VANID` %in% pb$VANID & !is.na(CityCouncilName) & RegistrationStatusName != "Applicant"]
# 
# ## Renaming/recoding to match with the pb table on the DB
# voterfile <- as.data.frame(voterfile)

# renaming columns - modified 5/15 for the 2018 voterfile column name changes
voterfile <- voterfile  %>%
  rename(Ethnicity = EthnicCatalistName,
         DoB = DOB,
         DoR = DateReg,
         RegStatus = RegistrationStatusName,
         County = CountyName,
         City = CityName,
         #Zip = Zip5,
         Lat = Latitude,
         Long = Longitude,
         NYCCD = CityCouncilName,
         CensusTract = CensusTractName,
         VANID = `Voter File VANID`,
         ED = PrecinctName)

voterfile <- voterfile  %>%
  mutate(County = recode(County, Bronx = "BRONX" , Kings = "KINGS", `New York` = "NEW YORK", Queens = "QUEENS", Richmond = "RICHMOND")) %>% 
  mutate(DoB = mdy(DoB))

names(voterfile) <- names(voterfile) %>% 
  str_replace("General", "g_20") %>%
  str_replace("PresidentialPrimary", "pp_20") %>%
  str_replace("Primary", "p_20")

voterfile <- voterfile %>%
  select(-starts_with("Special"), -ends_with("Party")) %>%
  select(-Lat, -Long, -starts_with("Street"), -starts_with("Apt"), - VHHID, -StateFileID, -DWID, 
         -starts_with("Reported"), -CounDist)

### Limit voterfile to only voters in eligible districts  -------------------------------------------------

comparedist <- c(23, 39, pb2016$district)

voterfile <-  pbnyc %>% 
  filter(districtCycle == 1) %>% 
  select(NYCCD = district, pbyear = voteYear) %>% 
  left_join(voterfile, .) %>% 
  filter(NYCCD %in% comparedist | pbdistrict == 0) %>% 
  rename(pb = pbdistrict)

voterfile <- voterfile %>% mutate(pb = replace_na(pb, 0))
### Downstream processing, adding auxiliary info - pulled out to standardize across different match_specs
source("rr_vf_aux_processing.R")

saveRDS(voterfile, "data/cleaned_R_results/voterfile_for_matching_placebo.rds")

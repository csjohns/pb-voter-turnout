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
### 7. Add census ACS data at tract level
### 8. Add competitiveness measures
### 9. Add council district level covariates.
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

### Load compare structure of the two data frames ### -------------------------------------------------------------------------------------

setdiff(names(pb), names(voterfile))
setdiff(names(voterfile), names(pb))

### Limit voterfile to only voters in eligible districts 

comparedist <- c(23, 39, pb2016$district)

voterfile <-  pbnyc %>% 
  filter(districtCycle == 1) %>% 
  select(NYCCD = district, pbyear = voteYear) %>% 
  left_join(voterfile, .) %>% 
  filter(NYCCD %in% comparedist | pbdistrict == 0) %>% 
  rename(pb = pbdistrict)


## recoding vote tallies to a binary voted/not voted indicator
convLogicVote <- function(x){as.numeric(x != "")}   #function to create binary for any case with any non-empty string

voterfile <- voterfile %>% mutate(pb = replace_na(pb, 0))
voterfile <- voterfile %>% 
  mutate_at(vars(starts_with("p_")), convLogicVote) %>% 
  mutate_at(vars(starts_with("g_")), convLogicVote) %>% 
  mutate_at(vars(starts_with("pp_")), convLogicVote) 

## Creating new variables for age (in years), and voting rates for years 2000-2007 (increasing granularity to aid matching/analysis)
voterfile <- voterfile %>% 
  rowwise() %>% 
  mutate(age = 2017 - year(DoB),
         g_early = sum(g_2000, g_2001, g_2002, g_2003, g_2004, g_2005, g_2006, g_2007, na.rm = TRUE),
         p_early = sum(p_2000, p_2001, p_2002, p_2003, p_2004, p_2005, p_2006, p_2007, na.rm = TRUE)) %>% 
  ungroup()
gc()


### Including census data ### ----------------------------------------------------------------------------------------------------------------------------
# loading extended voterfile with more complete tract info
voterfile_sf <- readRDS("data/cleaned_R_results/voters_census.rds")
voterfile_sf <- rename(voterfile_sf, 
                       CensusTractUpdate = "CensusTract",
                       countycodeupdate = "countycode")

voterfile <- left_join(voterfile, voterfile_sf, by = "VANID")
rm(voterfile_sf)

## Combine spatial pieces in order to join with census tracts
voterfile <- voterfile %>% filter(County %in% c("BRONX", "KINGS", "NEW YORK", "QUEENS", "RICHMOND")) %>% 
  mutate(countycode = recode(County, BRONX = "005", KINGS = "047", `NEW YORK` = "061", QUEENS = "081", RICHMOND = "085"),
         CensusTract = str_pad(CensusTract, 6, "left", "0"))
voterfile <- voterfile %>% 
  mutate(countycode = coalesce(countycodeupdate, countycode),
         tractcode = coalesce(CensusTractUpdate, CensusTract)) %>% 
  mutate(tract = paste0(countycode, tractcode))  


gc()

# source("censustables.R") - this script actually pulls the census data down
# here's one I made earlier (census data pull)
load("data/cleaned_R_results/census.Rdata")

voterfile <- voterfile %>% 
  left_join(educ) %>%
  left_join(inc) %>% 
  left_join(race)

## Add indicator for if voter's race matches majority race of tract
voterfile <- voterfile %>% 
  mutate(majmatch = Race == majority)

##7658 PB voters in the voter file
## 12-2-18 - now getting 7654 in voterfile?? ## 2/29-2019 - now getting 7652 in voterfile?!


### Including competitiveness ---------------------------------------------------------------------------------
vf_compet <- readRDS("data/cleaned_R_results/wide_compet.rds")
names(vf_compet) <- paste0("comp_", names(vf_compet))

voterfile <- vf_compet %>% 
  rename(VANID = "comp_VANID") %>% 
  # select(-matches("2009_general|2010_primary|2013_general|2013_primary|2017_general|2014_pp")) %>%
  left_join(voterfile, ., by = "VANID")

voterfile <- voterfile %>% 
  mutate_at(vars(starts_with("comp")), replace_na, 1) 

rm(vf_compet)

### Including district covariates ------------------------------------------------------------------------------

district_covar <- readRDS("data/cleaned_R_results/council_districts.rds")
district_covar <- district_covar %>% 
  rename(NYCCD = district,
         dist_white = white_pct, 
         dist_age18 = age18_pct,
         dist_college = college_pct,
         dist_medhhinc = medhhinc)

voterfile <- district_covar %>% 
  # select(NYCCD, dist_white, dist_college, incumbent_2017) %>% 
  left_join(voterfile, ., by = "NYCCD")

# # Note, not great common support, what to do?
# voterfile %>% 
#   select(pb,starts_with("incumbent"), starts_with("competitive")) %>% 
#   group_by(pb) %>% 
#   summarize_all(~sum(.)/n()) %>% 
#   gather("variable", "prop_true", -pb) %>% 
#   spread(key = "pb", "prop_true")
voterfile <- voterfile %>% 
  select(-starts_with("competitive"), -incumbent_2009, -incumbent_2013, -dist_medhhinc, -dist_age18) #to get back to limited controls in earlier match


voterfile <- voterfile %>% 
  mutate(agegroup = cut(age, breaks = c(0, 20, 30, 40, 50, 60,70,80,Inf)))

saveRDS(voterfile, "data/cleaned_R_results/voterfile_for_matching_placebo.rds")

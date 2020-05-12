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
### ...
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
library(purrr)

stringNAs <- function(x){
  ifelse(x, "", NA)
}
### Load PB data ### -------------------------------------------------------------------------------------
pb <- readRDS("data/cleaned_R_results/pb.rds")

### Load voterfile, limit to only active registered voters - definition from Sonya Reynolds, data source
voterfile <- fread("PersonFile20180426-11056504994/PersonFile20180426-11056504994.txt")
voterfile <- voterfile[!RegistrationStatusName %in% c('Applicant', 'Dropped', 'Unregistered') & DateReg != "" & !`Voter File VANID` %in% pb$VANID]

set.seed(1851)
## re-geocode based on latlong to assign voters missing council district to NYCCD - ED as last resort
source("vf_gis_nyccdmatch.R")

## re-geocode based on ED to assign voters missing NYCCD; then assign to pb districts based on PB logic
source("pb_cleanup_addnyccd_foranalysis.R")


# limit pb file to  only 23/39 and 2016 districts, or voters who first voted in 2012
pbnyc <- read.csv(file = "pbnyc_district_votes.csv", as.is = TRUE)
pb2016 <- pbnyc %>% filter(districtCycle == 1 & voteYear == 2016) 
pbdistricts <- unique(pbnyc$district)
# rm(pbnyc)
 
pb <- pb %>% 
  filter(pbdistrict %in% c(23, 39, pb2016$district) | pb_2012 == 1 |
           (pb_2013 == 1  & pbdistrict %in% pbnyc_history$pbdistrict[pbnyc_history$start_year==2013]) |
           (pb_2014 == 1  & pbdistrict %in% pbnyc_history$pbdistrict[pbnyc_history$start_year==2014]) |
           (pb_2015 == 1  & pbdistrict %in% pbnyc_history$pbdistrict[pbnyc_history$start_year==2015])) ## this is filtering to only districts we have full/near full data for

### Renaming/recoding voterfile to match with the pb table on the DB -----------------------------------

# renaming columns - modified 5/15/2018 for the 2018 voterfile column name changes
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
         -starts_with("Reported"), -RegStatus)

### Load compare structure of the two data frames ### -------------------------------------------------------------------------------------

setdiff(names(pb), names(voterfile))
setdiff(names(voterfile), names(pb))

# clean up pb names 
pb <- pb %>% 
  select(-State, -Zip)

### Join PB to voter file ### -------------------------------------------------------------------------------------
## this code works by basically appending the voters from the districts of interest to the non-pb voterfile
voterfile <- bind_rows(pb, voterfile)

## fill all non-pb voters in with pb = 0
voterfile <- voterfile %>% mutate(pb = replace_na(pb, 0))

### Downstream processing, adding auxiliary info - pulled out to standardize across different match_specs ---------------------
source("rr_vf_aux_processing.R")
voterfile <- voterfile %>% 
  select(-CensusTract, -CensusTractUpdate, -countycodeupdate, -countycode, -County, -tractcode)

# checking voterfile tract assignment consistency
table(voterfile$NYCCD, is.na(voterfile$high_school)) %>% prop.table(1)  ## looks like Dist 23 is an outlier - it's big, but no water, so not clear why....
voterfile %>% filter(NYCCD == 23) %>% mutate(notract = is.na(high_school)) %>% count(pb, notract)
  
### attach district matching groups --------------------
match_groups <- readRDS("data/cleaned_R_results/district_match_res.rds") 

voterfile <- voterfile %>% 
  mutate(effective_district = ifelse(pb == 1, pbdistrict, NYCCD)) %>% 
  left_join(match_groups, by = c(effective_district = "NYCCD") ) %>% 
  mutate(match_group = replace_na(match_group, 4)) #create a group for the excluded control districts

### save clean file to disk ----------------------
saveRDS(voterfile, "data/cleaned_R_results/voterfile_full_clean.rds")

### create files for each of the primary matching comparisons ---------------------------

# load processed voterfile
# voterfile <- readRDS("data/cleaned_R_results/voterfile_full_clean.rds")

# filter to appropriate population
compare_districts <- c(23, 39, 30,35,36,40)
pbdistricts <- readRDS("data/cleaned_R_results/pbdistricts.rds")

### Base model - pb to non-pb ---------
suffix <- ""
voterfile %>% 
  filter(pb == 1 | !NYCCD %in% pbdistricts) %>% 
  saveRDS(file = paste0("data/cleaned_R_results/voterfile_for_matching", suffix, ".rds"))


### within_district control model pb to non-pb in district ----------
suffix <-  "_within_dist"
voterfile %>% 
  filter(effective_district %in% compare_districts) %>% 
  mutate(NYCCD = effective_district) %>% 
  saveRDS(file = paste0("data/cleaned_R_results/voterfile_for_matching", suffix, ".rds"))

### Placebo model - non-pb to non-pb voters, comparing districts --------
suffix <- "_placebo"
# calculate PB district voter proprotions
prop_pb <- voterfile %>% 
  filter(pb == 1) %>% 
  mutate(total = n()) %>% 
  group_by(effective_district) %>% 
  summarize(prop_pb = n()/unique(total)) %>% 
  ungroup()
# filter voterfile to only non-PB voters, in comparable districts
voterfile <- voterfile %>% 
  filter(((pb == 0 & NYCCD %in% compare_districts) | !effective_district %in% pbdistricts) & !is.na(NYCCD)) %>% 
  mutate(pb = as.numeric(NYCCD %in% compare_districts)) 

# set downsizing factor
tot_pb_n <- sum(voterfile$pb) *.075
# calculate sample sizes, nest df, sample list columsn return to dataframe
vf2 <- voterfile %>% 
  group_by(NYCCD) %>% 
  nest() %>% 
  left_join(prop_pb, by = c(NYCCD = "effective_district")) %>% 
  mutate(n_sample = round(tot_pb_n*prop_pb, 0) + 20) %>% 
  # group_by(NYCCD) %>% 
  mutate(n_row = map_dbl(data, ~nrow(.)),
         n_sample = replace_na(n_sample, n_row)) %>%
  # ungroup() %>% 
  mutate(samp = map2(data, n_sample, sample_n)) %>% 
  select(NYCCD, samp) %>% 
  unnest(samp)

vf2 <- pbnyc %>% 
  filter(districtCycle == 1) %>% 
  select(NYCCD = district, pbyear = voteYear) %>% 
  left_join(vf2, .) %>% 
  ungroup()
  
  
saveRDS(vf2, paste0("data/cleaned_R_results/voterfile_for_matching", suffix, ".rds"))

##############################################################################################################################
###
### NYC PB Voters Project 
### Voterfile processing and appending auxiliary information
### ** NOT STAND-ALONE - MUST BE RUN W/IN VF_PROCESSING SCRIPTS
### Carolina Johnson
### 
### 7. Add census ACS data at tract level
### 8. Add competitiveness measures
### 9. Add council district level covariates.
### 10. Create age groups
###
##############################################################################################################################


## recoding vote tallies to a binary voted/not voted indicator
convLogicVote <- function(x){as.numeric(x != "")}   #function to create binary for any case with any non-empty string

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

### Including competitiveness ---------------------------------------------------------------------------------
source("rr_compet_cleanup.R")
vf_compet <- readRDS("data/cleaned_R_results/wide_compet.rds")
names(vf_compet) <- paste0("comp_", names(vf_compet))

voterfile <- vf_compet %>% 
  rename(VANID = "comp_VANID") %>% 
  select(-matches("2009_general|2010_primary|2013_general|2013_primary|2017_general|2014_pp")) %>% # droping years with very little variation
  left_join(voterfile, ., by = "VANID")

voterfile <- voterfile %>% 
  comp_structural_na_1() # replaces missing values in voters not missing for other elections with 1 - to handle non-elections differently from missing data

compvars <- voterfile %>% select(starts_with("comp_")) %>% names()

for (v in compvars) { # replace missing data with the modal competitiveness for that voter's ED
  print(v)
  voterfile <- voterfile %>% 
    replace_na_compet(v)
}

# voterfile <- voterfile %>% 
#   mutate_at(vars(starts_with("comp")), replace_na, 1) # not necessary with the new NA replace functions

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

# voterfile <- voterfile %>% 
#   select(-starts_with("competitive"), -incumbent_2009, -incumbent_2013, -dist_medhhinc, -dist_age18) #to get back to limited controls in earlier match

voterfile <- voterfile %>% 
  mutate(agegroup = cut(age, breaks = c(0, 20, 30, 40, 50, 60,70,80,Inf)))

### BOE - CREATE RECORDS FOR MISSING VANIDS - IMPUTING VALUE FROM THE MODAL VALUE FOR THAT ed
##############################################################################################################################
###
### NYC PB Voters Project 
### Create clean wide_compet.rds file - Imputing missing and setting column names 
### Carolina Johnson
### 05/03/2020
###
### This file is run once and results saved.
###
### Initial competitiveness calculation and assignment resulted in missing data for ~5% of voters, most likely because of failures in the many cycles of spatial joins.
### There is nearly always at least one competitiveness value (and never more than three) for each election district, so I developed a method to identify the modal
### competitiveness for any given election-ED district combination, and use this value to fill int he missing values. This resolves missingness to <0.05% now missing
###
### joining wide_compet back to the whole voterfile, flagging structurally missing data with -99 (voters only missing for election cycles they
### in which only some distrcits had elections), and imputing other missing competitiveness scores with the modal value for that ED (based on the 95% complete)
###
##############################################################################################################################

# Load libraries
library(dplyr)
library(tidyr)
library(glue)
library(lubridate)
library(stringr)
library(data.table)

stringNAs <- function(x){
  ifelse(x, "", NA)
}

### Load full voterfile data ### -------------------------------------------------------------------------------------

voterfile <- fread("PersonFile20180426-11056504994/PersonFile20180426-11056504994.txt")
# voterfile <- voterfile[! RegistrationStatusName %in% c('Applicant', 'Dropped', 'Unregistered') & DateReg != ""]
voterfile <- voterfile[ , c("Voter File VANID", "PrecinctName")]

### standard voterfile column name processing etc -------------------------------
voterfile <- voterfile  %>%
  rename(VANID = `Voter File VANID`,
         ED = PrecinctName)

### Including competitiveness ---------------------------------------------------------------------------------
source("rr_compet_cleanup.R")
vf_compet <- readRDS("data/cleaned_R_results/wide_compet.rds")
names(vf_compet) <- paste0("comp_", names(vf_compet))

voterfile <- vf_compet %>% 
  rename(VANID = "comp_VANID") %>% 
  # select(-matches("2009_general|2010_primary|2013_general|2013_primary|2017_general|2014_pp")) %>% # droping years with very little variation
  left_join(voterfile, ., by = "VANID")

voterfile <- voterfile %>% 
  comp_structural_na_1() # replaces missing values in voters not missing for other elections with 1 - to handle non-elections differently from missing data
summary(voterfile) #confirming the replaces work - constant NA value and only some cols w/-99

compvars <- voterfile %>% select(starts_with("comp_")) %>% names()

for (v in compvars) { # replace missing data with the modal competitiveness for that voter's ED
  voterfile <- voterfile %>% 
    replace_na_compet(v)
}
summary(voterfile) #confirming the replaces work - constant NA value and only some cols w/-99
# voterfile <- voterfile %>% 
#   mutate_at(vars(starts_with("comp")), replace_na, 1) # not necessary with the new NA replace functions

rm(vf_compet)

voterfile %>% 
  select(-ED) %>% 
  distinct() %>% 
  # glimpse() 
  saveRDS("data/cleaned_R_results/wide_compet_clean.rds")

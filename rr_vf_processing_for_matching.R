##############################################################################################################################
###
### NYC PB Voters Project 
### Loading full processed voterfile and creating appropriate subsets for matching
### Carolina Johnson
### 
### 1. Load full voterfile
### 2. Split into appropriate sampling frames for main model comparisons (suffix = "") and save .rds
### 3. Split into within PB district comparison groups (suffix = "_within_dist") and save .rds
### 4. Split into placebo comparison groups, and sample down to only 7.5% of the population for future computation scale
###    (suffix = "_placebo") and save .rds
###
##############################################################################################################################

library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)


### load processed voterfile ---------------------------------------------------------------------------------------------------
# source(rr_vf_processing_full.R) # - OR JUST LOAD PREPPED DATA:
voterfile <- readRDS("data/cleaned_R_results/voterfile_full_clean_deid.rds")

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
set.seed(4282020)
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

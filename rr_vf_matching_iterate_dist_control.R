##### 
### Matching PB to non-PB voters - implementing district-level controls

##############################################################################################################################
###
### NYC PB Voters Project 
### Matching pb/non-pb voters
### Carolina Johnson
### 
### 1. Create exact match subset to limit size
### 2. Create matching dataframe
### 3. Define CEM match thresholds
### 4. Run CEM
### 5. Create analysis dataframe from 1:1 match
### 6. Save matched dataframe to RDS
### 7. Run match balance checking
### 8. Clean up voterfile for space (for subsequent analysis)
###
##############################################################################################################################

# Load libraries
library(dplyr)
library(tidyr)
library(MatchIt)
library(cem)

# source("rr_vf_processing.R")

suffix <- ""
voterfile <- readRDS(paste0("data/cleaned_R_results/voterfile_for_matching", suffix, ".rds"))

#### Implementing Matching, starting with exact ###-----------------------------------------------------------------------------------------------------------------------------------------------  # 
  
## Exact matching to narrow field of possibility
# source("rr_exact_match.R")
# rm(exact_df)

### Creating matching dataframe based on the potential matches from m.exact --------------------------------------------------------------------------------
matchable_vans <- readRDS(paste0("data/cleaned_R_results/matchablevans", suffix, ".rds"))

matching_df <- voterfile %>%
  filter(VANID %in% matchable_vans) %>% 
  mutate_at(vars(starts_with("comp")), replace_na, NA) %>% 
  mutate(agegroup = cut(age, breaks = c(0, 18.5, 25.5, 39.5, 49.5, 59.5, 69.5, 79.5, Inf))) 

# attach district groups
match_groups <- readRDS("data/cleaned_R_results/district_match_res.rds") 

matching_df <-  matching_df %>% 
  mutate(effective_district = ifelse(pb == 1, pbdistrict, NYCCD)) %>% 
  left_join(match_groups, by = c(effective_district = "NYCCD") ) %>% 
  select(-effective_district) %>% 
  mutate(match_group = replace_na(match_group, 4)) #create a group for the excluded control districts
### create match model parameters
source("rr_matching_levels_dist_revised.R")
source("rr_matching_functions.R")

# testout <- custom_cem(df = matching_df,
#                       fields = matching_models$matching_fields[[11]],
#                       cutpoints = matching_models$cutpoints[[11]],
#                       grouping = matching_models$grouping[[11]])

allout <- matching_models %>% 
  mutate(outdf = pmap(.l = list(fields = matching_fields, 
                           cutpoints = cutpoints, 
                           grouping = grouping), 
                      .f = custom_cem, 
                      df = matching_df))



## save matching results to disc
suffix <- "district"
saveRDS(allout, file = paste0("data/cleaned_R_results/matching_res", suffix, ".RDS"))

### creating analysis DF by joining voterfile to the CEM match output - effectively returns voterfile info filtered to matched dataset -----
# vf_analysis <- make_analysis_vf(allout$outdf[1], voterfile)

### Match balance checking ------------------------------------------------------------------------------------------------
# select preferred match result




# run balance checks for publication
# source("pub_balance_checking.R")




    
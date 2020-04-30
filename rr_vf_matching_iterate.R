##### 
### Matching PB to non-PB voters

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

suffix <- "_placebo"
voterfile <- readRDS(paste0("data/cleaned_R_results/voterfile_for_matching", suffix, ".rds"))

#### Implementing Matching, starting with exact ###-----------------------------------------------------------------------------------------------------------------------------------------------  # 
  
## Exact matching to narrow field of possibility
# source("rr_exact_match.R")

### Creating matching dataframe based on the potential matches from m.exact --------------------------------------------------------------------------------
matchable_vans <- readRDS("data/cleaned_R_results/matchablevans.rds")

matching_df <- voterfile %>%
  filter(VANID %in% matchable_vans) %>% 
  mutate_at(vars(starts_with("comp")), replace_na, 1) %>% 
  mutate(agegroup = cut(age, breaks = c(0, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60,65, 70,75, 80,85, 90, Inf))) 

### create match model parameters
source("rr_matching_levels.R")
source("rr_matching_functions.R")

testout <- custom_cem(df = matching_df,
                      fields = matching_models$matching_fields[[1]],
                      cutpoints = matching_models$cutpoints[[1]],
                      grouping = matching_models$grouping[[1]])

allout <- matching_models %>% 
  mutate(outdf = pmap(.l = list(fields = matching_fields, 
                           cutpoints = cutpoints, 
                           grouping = grouping), 
                      .f = custom_cem, 
                      df = matching_df))



## save matching results to disc
saveRDS(allout, file = paste0("data/cleaned_R_results/matching_res", suffix, ".RDS"))

### creating analysis DF by joining voterfile to the CEM match output - effectively returns voterfile info filtered to matched dataset -----
# vf_analysis <- make_analysis_vf(allout$outdf[1], voterfile)

### Match balance checking ------------------------------------------------------------------------------------------------
# select preferred match result




# run balance checks for publication
# source("pub_balance_checking.R")




    
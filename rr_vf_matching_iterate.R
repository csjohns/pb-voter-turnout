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
  ungroup() %>% 
  filter(VANID %in% matchable_vans) %>% 
  select(-matches("2009_general|2010_primary|2013_general|2013_primary|2017_general|2014_pp|2008_general|2008_pp|comp_2012_general|2016_general|2016_pp")) %>% # droping years with very little variation
  mutate_at(vars(starts_with("comp")), replace_na, NA) %>% 
  mutate(agegroup = cut(age, breaks = c(0, 18.5, 25.5, 39.5, 49.5, 59.5, 69.5, 79.5, Inf))) %>% 
  mutate_at(vars(matches("^p_|^g_|^pp_|Race|Sex|incumbent|jenks")), as.factor)

### create match model parameters
source(paste0("rr_matching_levels", suffix, ".R"))
source("rr_matching_functions.R")

# testout <- custom_cem(df = matching_df,
#                       fields = matching_models$matching_fields[[4]],
#                       cutpoints = matching_models$cutpoints[[4]],
#                       grouping = matching_models$grouping[[4]])

allout <- matching_models %>% 
  mutate(outdf = pmap(.l = list(fields = matching_fields, 
                           cutpoints = cutpoints, 
                           grouping = grouping), 
                      .f = custom_cem, 
                      df = matching_df))



## save matching results to disc
saveRDS(allout, file = paste0("data/cleaned_R_results/matching_res", suffix, ".rds"))

allout %>% 
  mutate(n_treat = map_dbl(outdf, ~sum(.$pb))) %>% 
  select(match_type, n_treat) %>% 
  arrange(desc(n_treat))

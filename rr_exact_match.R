##############################################################################################################################
###
### NYC PB Voters Project 
### Matching pb/non-pb voters
### Carolina Johnson
### 
### 1. Create exact match subset to limit size
### 
### Input: Requires a loaded, processed voterfile
### Output: Saves a list of matchable VANS
###
##############################################################################################################################

# Load libraries
library(dplyr)
library(tidyr)
library(MatchIt)

stopifnot(exists("suffix"))
# summary(voterfile)

exact_df <- voterfile %>% 
  select(VANID, pb,  Race, agegroup, Sex, g_early, g_2008, g_2009, g_2010, 
         g_2011, p_early, p_2008, p_2009, p_2010, pp_2008) %>% 
  na.omit()

# votehash: tried and rejected as super hit to performance

m.exact <- matchit(pb ~ agegroup + Sex + g_early + g_2008 + g_2009 + g_2010 + 
                     g_2011 + p_early + p_2008 + p_2009 + p_2010 + pp_2004 + pp_2008, data = exact_df, method = "exact")

treat_sub <- unique(m.exact$subclass[m.exact$treat == TRUE])
table(m.exact$treat, is.na(m.exact$subclass))
## filter to rows with classes
matchable_vans <- exact_df$VANID[!is.na(m.exact$subclass)]
saveRDS(matchable_vans, paste0("data/cleaned_R_results/matchablevans", suffix, ".rds"))
rm(m.exact)
gc()

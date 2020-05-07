### non-exact (propensity score) matching districts to account for 
# Load libraries
library(dplyr)
library(tidyr)
library(MatchIt)
# library(cem)

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

district_covar <- readRDS("data/cleaned_R_results/council_districts_wmargin.rds")
district_covar <- district_covar %>% 
  rename(NYCCD = district,
         dist_white = white_pct, 
         dist_age18 = age18_pct,
         dist_college = college_pct,
         dist_medhhinc = medhhinc)

district_covar %>% 
  arrange(dist_white) %>% 
  mutate_at(vars(starts_with("dist_")), ~ cut(., quantile(., probs = c(0, .2, .4, .6, .8, 1)))) %>% select(-NYCCD) %>% n_distinct()
  mutate_at(vars(starts_with("dist_")), as.numeric) %>% as.data.frame()


  pbdistricts <-  read.csv(file = "pbnyc_district_votes.csv", as.is = TRUE) %>% .$district %>% unique()
  pbinsample <- matching_df$pbdistrict %>% na.omit() %>% unique()
  notpbdistricts <- setdiff(unique(matching_df$NYCCD), pbdistricts)
  
district_covar <- district_covar %>% 
  filter(NYCCD %in% c(pbinsample, notpbdistricts)) %>% 
  mutate_at(vars(starts_with("dist")), ~replace_na(., mean(., na.rm = T))) %>% 
  mutate(pbdistrict = as.numeric(NYCCD %in% pbinsample))

m.out <- MatchIt::matchit(formula = pbdistrict ~ incumbent_2013+ incumbent_2017 + incumbent_2009 + vote_margin_pct_next_2009 + vote_margin_pct_next_2013 + vote_margin_pct_next_2017 +
                            dist_age18 + dist_white + dist_college + dist_medhhinc, data = district_covar, method = "nearest", subclass = 3)

summary(m.out)
match.data(m.out)
m.out$subclass
district_covar$match_group <- m.out$subclass

# library(ggplot2)
# district_covar %>% drop_na() %>% ggplot(aes(x = incumbent_2013, y = vote_margin_pct_next_2009, color = as.factor(match_group), shape = as.factor(match_group))) + geom_point()

table(m.out$treat, is.na(m.out$subclass))
pairs <- m.out$match.matrix %>% 
  as.data.frame() %>% 
  tibble::rownames_to_column("treat_index") %>% 
  mutate(treat_index = as.numeric(treat_index),
         control_index = as.numeric(as.character(`1`))) %>% 
  mutate(treat_district = district_covar$NYCCD[treat_index],
         control_district = district_covar$NYCCD[control_index]) %>% 
  mutate(district_group = row_number()) 
  
district_covar %>% select(NYCCD, match_group) %>% drop_na() %>% 
  saveRDS(file = "data/cleaned_R_results/district_match_res.rds")



###   NEED TO IMPUTE PB DISTRICT FOR 2012 VOTERS MISSING DISTRICT



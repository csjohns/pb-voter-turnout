#################################################################################################################################################
### 
### PB and voter turnout: Match-spec robustness model checkign
### Depends on rr_vf_matching_iterate and associated scripts
###
### Created by: Carolina Johnson
### Created date: 3/29/2020
###
#################################################################################################################################################

library(dplyr)
library(tidyr)
library(purrr)
library(broom)
library(ggplot2)
library(lubridate)
library(stringr)
library(lme4)
library(margins)

source("create_pb_long.R")
make_analysis_vf <- function(match_res, voterfile = voterfile) {
  match_res %>% 
    ungroup() %>% 
    select(VANID, cem_group) %>% 
    distinct() %>% 
    left_join(voterfile)
}


### Creating/loading matched datasets
# source("pub_vf_matching.R")
allout <- readRDS("data/cleaned_R_results/matching_res.RDS")
voterfile <- readRDS("data/cleaned_R_results/voterfile_for_matching.rds")

fit_lmer_model <- function(match_res) {
  ## process analysis df to pb_long df for analysis (creating wide pb table along the way)
  df <- make_analysis_vf(match_res, voterfile)
  df <- create_pb_long(df)
  
  #### Set reference levels for factor variables 
  df <- df %>%
    group_by() %>% 
    mutate(Race = relevel(as.factor(Race), ref = "W"),
           election_type = relevel(as.factor(election_type), ref = "g"))
  
  ### run model
  lme_final <- glmer(turned_out ~ pb + after_pb + Race + Female + as.factor(year) + election_type + age + I(age^2) + I(age_at_vote < 18) + medhhinc_10k + college_pct + majmatch + (1 | VANID) + (1|NYCCD),
                     data = df, family = binomial(), nAGQ = 0) 
  lme_final
}

allout <- allout %>% 
  mutate(result = map(outdf, fit_lmer_model))
glimpse(allout)
allout <- allout %>% 
  mutate(tidyresult = map(result, tidy))

lmers <- allout %>% 
  select(match_type, tidyresult) %>% 
  unnest()

lmers %>% 
  filter(term == "after_pb") %>% 
  mutate(min = estimate - (1.96*std.error),
         max = estimate+ 1.96*std.error) %>% 
ggplot(aes(x = match_type, y = estimate, color = p.value<= 0.05) ) + 
  geom_pointrange(aes(ymin = estimate-(1.96*std.error), ymax = estimate+(1.96*std.error))) +
  # ylim(c(-10, 10)) +
  coord_flip()



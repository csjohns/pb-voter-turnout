
##############################################################################################################################
###
### NYC PB Voters Project 
### Iterating regressions from repeated match list
### Carolina Johnson
### 
### 1. Attach full set of competitiveness data for control in regression
### 2. Attach VF to processed data
### 3. Create model frames before running
### 4. Purrr through models
### 5. Tidy
### 6. Plot coefs w/small multiples
###
##############################################################################################################################

# Load libraries
library(dplyr)
library(tidyr)
library(purrr)
library(broom)
library(ggplot2)
library(lubridate)
library(stringr)
library(lme4)
library(margins)
library(simcf)

# functions
source("create_pb_long.R")

limit_outdf <- function(df) {
  df %>% 
    ungroup() %>% 
    select(VANID, cem_group)
}
make_analysis_vf <- function(match_res, voterfile = voterfile) {
  match_res %>% 
    ungroup() %>% 
    select(VANID, cem_group) %>% 
    distinct() %>% 
    left_join(voterfile)
}


attach_competition <- function(df) {# note requires vf_compet to be loaded
  df %>% 
    left_join(vf_compet)
}

create_model_data <- function(df, model_form) {
  df <- df %>% 
    mutate(Race = relevel(as.factor(Race), ref = "W"),
           election_type = relevel(as.factor(election_type), ref = "g")) %>% 
    simcf::extractdata(model_form, ., na.rm = TRUE)
  df
}

preprocess_lmer <- function(match_res, model_form) {
  ## process analysis df to pb_long df for analysis (creating wide pb table along the way)
  df <- make_analysis_vf(match_res, voterfile)
  df <- create_pb_long(df) %>% 
    group_by() %>% 
    attach_competition()
  df <- create_model_data(df, model_form)
  df
}


fit_lmer_model <- function(df, model_form) {
  ### run model
  res <- glmer(model_form, data = df, family = binomial(), nAGQ = 0) 
  progbar$tick()$print()
  res
}
calc_margin_effect <- function(data, model_res){
  margins::dydx(data, model_res, "after_pb", change = c(0,1))[[1]] %>% mean() 
}



### Creating/loading matched datasets
# source("pub_vf_matching.R")
suffix <- ""
allout <- readRDS(paste0("data/cleaned_R_results/matching_res", suffix, ".RDS"))
allout <- allout %>% 
  select(match_type, outdf)

allout <- allout %>% 
  mutate(outdf = map(outdf, limit_outdf))


uniquevanids <- sapply(allout$outdf, function(x){unique(x$VANID)}) %>% unlist() %>% unique() 


# Load and process voterfile - attaching full competitiveness measures
voterfile <- readRDS(paste0("data/cleaned_R_results/voterfile_for_matching", suffix, ".rds"))
# remove previously attached partial competitiveness records
voterfile <- voterfile %>% 
  filter(VANID %in% uniquevanids) %>% 
  select(-starts_with("comp_"), -pp_2016)


### Load competitiveness ---------------------------------------------------------------------------------
vf_compet <- readRDS("data/cleaned_R_results/wide_compet.rds") %>% 
  filter(VANID %in% uniquevanids)

# load presidential results
source("BOE_pres_process.R")

## attach pres, make long w/election & year
vf_compet <- vf_compet %>% 
  attach_pres(pres_wide) 

names(vf_compet) <- str_replace(names(vf_compet), "general", "g")
names(vf_compet) <- str_replace(names(vf_compet), "primary", "p")
  
vf_compet <- vf_compet %>% 
  gather(key = election, value = compet, -VANID) %>% 
  separate(election, c("year", "election_type")) %>% 
  mutate(year = as.numeric(year)) %>% 
  group_by(year, election_type) %>% 
  mutate(compet = replace_na(compet, mean(compet))) %>% 
  ungroup()
  

### Define model formulas
formula_df <- tibble(model_name = c("logit_minimal_form",
                                    "logit_demog_form",
                                    "logit_tract_form",
                                    "lme_final_form",
                                    "lme_comp"),
                     model_formula = list(turned_out ~ pb + after_pb + as.factor(year) + election_type + (1| VANID) + (1|NYCCD),
                                          turned_out ~ pb + after_pb  + as.factor(year) + election_type + Race + Female + age + I(age^2)  + (1| VANID) + (1|NYCCD),
                                          turned_out ~ pb + after_pb  + as.factor(year) + election_type +  Race + Female + age + I(age^2)+ I(age_at_vote < 18) + college_pct + medhhinc_10k +(1| VANID) + (1|NYCCD),
                                          turned_out ~ pb + after_pb + Race + Female + as.factor(year) + election_type + age + I(age^2) + I(age_at_vote < 18) + medhhinc_10k + college_pct + majmatch + (1 | VANID) + (1|NYCCD),
                                          turned_out ~ pb + after_pb + Race + Female + as.factor(year) + election_type + compet + age + I(age^2) + I(age_at_vote < 18) + medhhinc_10k + college_pct + majmatch + (1 | VANID) + (1|NYCCD)
                     )) 

allout <- expand_grid(allout, formula_df)
### Preprocess in list-columns --------------------------------------------------------------------------------------------


allout <- allout %>% 
  # slice(1:5) %>% 
  mutate(pblong = pmap(.l = list(match_res = outdf, 
                                 model_form = model_formula),
                       .f = preprocess_lmer)) 

progbar <- progress_estimated(nrow(allout))
allout <- allout %>% 
  mutate(result = pmap(.l = list(df = pblong,
                                 model_form = model_formula),
                       .f = fit_lmer_model))

allout  %>% mutate(AIC = map_dbl(result, AIC), BIC = map_dbl(result, BIC)) %>% select(match_type, model_name, AIC, BIC) %>%  arrange(match_type, AIC) %>% View()
glimpse(allout)
allout <- allout %>% 
  mutate(tidyresult = map(result, broom::tidy, conf.int = TRUE),
         AIC = map_dbl(result, AIC),
         BIC = map_dbl(result, BIC))
allout %>% select(match_type, model_name, pblong, result) %>% 
  saveRDS(paste0("data/cleaned_R_results/iter_regress_check", suffix, ".rds"))

allout <- readRDS(paste0("data/cleaned_R_results/iter_regress_check", suffix, ".rds"))
lmers <- allout %>%  
  select(match_type, model_name, tidyresult) %>% 
  unnest(cols = tidyresult)

robust <- lmers %>% 
  filter(group == "fixed" & term != "I(age_at_vote < 18)TRUE" & !str_detect(term, "year") & term != "(Intercept)") %>% 
  ggplot(aes(x = term, y = estimate, ymin = conf.low, ymax = conf.high, color = match_type)) +
    geom_pointrange() +
    geom_point() +
    geom_hline(aes(yintercept = 0)) +
    facet_wrap(~model_name) +
    # ylim(-5, 5) +
    coord_flip()
robust
library(plotly)

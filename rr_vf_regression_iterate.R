
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
library(furrr)
library(broom)
library(ggplot2)
library(lubridate)
library(stringr)
library(lme4)
library(margins)
library(simcf)

# functions
source("create_pb_long.R")
source("rr_regression_functions.R")

### Creating/loading matched datasets
# source("pub_vf_matching.R")
# suffix <- ""
if (suffix == "") {set.seed(4152019)} 
if (suffix == "_within_dist") {set.seed(5152019)}
allout <- readRDS(paste0("data/cleaned_R_results/matching_res", suffix, ".RDS"))
allout <- allout %>% 
  select(match_type, outdf)

allout <- allout %>% 
  mutate(outdf = map(outdf, limit_outdf))


uniquevanids <- sapply(allout$outdf, function(x){unique(x$VANID)}) %>% unlist() %>% unique() 


# Load and process voterfile - attaching full competitiveness measures
voterfile <- readRDS(paste0("data/cleaned_R_results/voterfile_for_matching", suffix, ".rds")) 
# keep matched
voterfile <- voterfile %>% 
  filter(VANID %in% uniquevanids) 

# pull out previously attached partial competitiveness records to reshape 
vf_compet <- voterfile %>% 
  select(VANID, starts_with("comp_"))

voterfile <- voterfile %>% 
  select(-starts_with("comp_"))


### Load competitiveness ---------------------------------------------------------------------------------
vf_compet <- reshape_vf_compet(vf_compet)

## create pb_longs
allout <- allout %>% 
  mutate(pblong = map(outdf, preprocess_lmer))

### Define model formulas
formula_df <- tibble(model_name = c("logit_minimal_form",
                                    # "logit_demog_form",
                                    # "logit_tract_form",
                                    # "lme_final_form",
                                    "lme_comp"),
                     model_formula = list(turned_out ~ pb + after_pb + as.factor(year) + election_type + (1| VANID) + (1|NYCCD),
                                          # turned_out ~ pb + after_pb  + as.factor(year) + election_type + Race + Female + age + I(age^2)  + (1| VANID) + (1|NYCCD),
                                          # turned_out ~ pb + after_pb  + as.factor(year) + election_type +  Race + Female + age + I(age^2)+ I(age_at_vote < 18) + college_pct + medhhinc_10k +(1| VANID) + (1|NYCCD),
                                          # turned_out ~ pb + after_pb + Race + Female + as.factor(year) + election_type + age + I(age^2) + I(age_at_vote < 18) + medhhinc_10k + college_pct + majmatch + (1 | VANID) + (1|NYCCD),
                                          turned_out ~ pb + after_pb + Race + Female + as.factor(year) + election_type + compet + age + I(age^2) + I(age_at_vote < 18) + medhhinc_10k + college_pct + majmatch + (1 | VANID) + (1|NYCCD)
                     )) 

allout <- expand_grid(allout, formula_df)
### Preprocess in list-columns --------------------------------------------------------------------------------------------
plan(multiprocess)

allout <- allout %>% 
  # slice(1:5) %>%
  mutate(pblong = pmap(.l = list(df = pblong, 
                                 model_form = model_formula),
                       .f = create_model_data) )

# progbar <- progress_estimated(nrow(allout))
allout <- allout %>% 
  # slice(1:2) %>%
  mutate(result = future_pmap(.l = list(df = pblong,
                                 model_form = model_formula),
                       .f = fit_lmer_model,
                       .progress = TRUE))
plan(sequential)

allout  %>% mutate(AIC = map_dbl(result, AIC), BIC = map_dbl(result, BIC)) %>% select(match_type, model_name, AIC, BIC) %>%  arrange(match_type, AIC) %>% View()
glimpse(allout)


allout %>% select(match_type, model_name, pblong, result) %>% 
  saveRDS(paste0("data/cleaned_R_results/iter_regress_check", suffix, ".rds"))

allout <- readRDS(paste0("data/cleaned_R_results/iter_regress_check", suffix, ".rds"))

allout <- allout %>% 
  mutate(tidyresult = map(result, broom::tidy, conf.int = TRUE),
         AIC = map_dbl(result, AIC),
         BIC = map_dbl(result, BIC), 
         nvoter = map_dbl(pblong, ~n_distinct(.$VANID)),
         pct39 = map_dbl(pblong, ~sum(.$pb == 1 & .$NYCCD == 39)/sum(.$pb == 1))) 

allout <- mutate(allout, match_label = paste0(match_type, ": ", nvoter))

lmers <- allout %>%  
  select(match_type = match_type, model_name, tidyresult, nvoter, pct39) %>% 
  unnest(cols = tidyresult) %>% 
  group_by(match_type, model_name) %>% 
  mutate(pb_effect = estimate[term == "after_pb"])

saveRDS(lmers, file = paste0("data/cleaned_R_results/iter_regress_lmers", suffix, ".rds"))

robust <- lmers %>% 
  filter(group == "fixed" & term != "I(age_at_vote < 18)TRUE" & !str_detect(term, "year") & term != "(Intercept)" & term != "RaceU") %>% 
  ggplot(aes(x = term, y = estimate, ymin = conf.low, ymax = conf.high, color = forcats::fct_reorder(match_type, pb_effect, .desc = TRUE))) +
    geom_pointrange(position = position_dodge(width = .2)) +
    geom_point(position = position_dodge(width =.2 )) +
    geom_hline(aes(yintercept = 0)) +
    facet_wrap(~model_name) +
    geom_text(aes(y = 2.5, label = nvoter), position = position_dodge(width = .2) , size = 2) +
  geom_text(aes(y = -3, label = round(pct39,2)), position = position_dodge(width = .2) , size = 2) +
    # ylim(-5, 5) +
    coord_flip()
# robust
plotly::ggplotly(robust)

## checking council district distribution across matches
# distabs <- allout %>% 
#   filter(match_type %in% c("All vars, fine", "All vars, coarse", "Dist medhhinc", "Excl comp + dist", "Excl compet", "Excl district")) %>% 
#   group_by(match_type) %>% slice(1) %>% ungroup %>% 
#   transmute(disttabs = map(pblong,~select(., VANID, NYCCD) %>% distinct() %>% count(NYCCD) %>% arrange(desc(n))),
#             nvoter = nvoter,
#             match_type = match_type) 
# full_join(distabs$disttabs[[1]] %>% rename(n_coarse = n),distabs$disttabs[[2]] %>% rename(n_fine = n)) %>% 
#   full_join(distabs$disttabs[[3]] %>% rename(n_medhhinc = n)) %>% 
#   full_join(distabs$disttabs[[4]] %>% rename(n_excl = n)) %>% 
#   full_join(distabs$disttabs[[5]] %>% rename(n_exclcomp = n)) %>% 
#   full_join(distabs$disttabs[[6]] %>% rename(n_excldist = n)) %>% 
#   mutate_at(vars(starts_with("n_")), ~./sum(., na.rm = T)) %>% View()

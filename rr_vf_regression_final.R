#################################################################################################################################################
### 
### PB and voter turnout: Regression analyses from matched dataset
### Calls  the pub_vf_matching.R script and the pub_balance_checking.R scripts
###
### Created by: Carolina Johnson
### Created date: 3/1/2018
###
#################################################################################################################################################

library(dplyr)
library(purrr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(stringr)
library(lme4)
library(margins)

### loader helper functions

source("create_pb_long.R")
source("rr_regression_functions.R")

## load data and process  

### Creating/loading matched datasets

set.seed(05012019)
matching_model <- "Tract super"
allout <- readRDS(paste0("data/cleaned_R_results/matching_res.RDS"))
matched_data <- allout %>% filter(match_type == matching_model) %>% pluck("outdf", 1) 
  
# Load and process voterfile - attaching full competitiveness measures
voterfile <- readRDS(paste0("data/cleaned_R_results/voterfile_for_matching.rds"))

# keep matched
voterfile <- voterfile %>% 
  semi_join(matched_data, by = "VANID")

# pull out previously attached partial competitiveness records to sreshape 
vf_compet <- voterfile %>% 
  select(VANID, starts_with("comp_"))

voterfile <- voterfile %>% 
  select(-starts_with("comp_"))

### Load/reshape competitiveness ---------------------------------------------------------------------------------
vf_compet <- reshape_vf_compet(vf_compet)

### process, make long, attach competitiveness
pb_long <- preprocess_lmer(matched_data)

#### Set reference levels for factor variables 
pb_long <- create_model_factors(pb_long) 

### remove 2011 + 2015 as non-informative years (no substantially city-wide elections)
pb_long <- pb_long %>% 
  filter(! year %in% c(2011, 2015))

### Models and output for paper ---------------------------------------------------------------------------------------------------------------------------------------------------------------------
### LMER base model, no covars:  ---------------------------------------------------------------------------------------------------------------------------------------------------------------------
logit_minimal_form <- turned_out ~ pb + after_pb + as.factor(year) + election_type + (1| VANID) + (1|NYCCD)
lme_minimal <- glmer(logit_minimal_form, data = pb_long, family = binomial(), nAGQ = 0)

### LMER model only demographics + base ---------------------------------------------------------------------------------------------------------------------------------------------------------------------
logit_demog_form <- turned_out ~ pb + after_pb  + as.factor(year) + election_type + Race + Female + age + I(age^2) + I(age_at_vote < 18) + (1| VANID) + (1|NYCCD)
lme_demog  <- glmer(logit_demog_form, data = pb_long, family = binomial(), nAGQ = 0)

### LMER model incl tract vars ---------------------------------------------------------------------------------------------------------------------------------------------------------------------
logit_tract_form <- turned_out ~ pb + after_pb  + as.factor(year) + election_type +  Race + Female + age + I(age^2) + 
  I(age_at_vote < 18) + college_pct + medhhinc_10k +(1| VANID) + (1|NYCCD)
lme_tract  <- glmer(logit_tract_form, data = pb_long, family = binomial(), nAGQ = 0)

### LMER model incl majmatch ---------------------------------------------------------------------------------------------------------------------------------------------------------------------
lme_final_form <- turned_out ~ pb + after_pb + Race + Female + as.factor(year) + election_type + age + I(age^2) + I(age_at_vote < 18) + medhhinc_10k + college_pct + majmatch + (1 | VANID) + (1|NYCCD)
lme_final <- glmer(lme_final_form, data = pb_long, family = binomial(), nAGQ = 0) 

### LMER model incl comp ---------------------------------------------------------------------------------------------------------------------------------------------------------------------
lme_compet_form <- turned_out ~ pb + after_pb + compet + Race + Female + as.factor(year) + election_type + age + I(age^2) + I(age_at_vote < 18) + medhhinc_10k + college_pct + majmatch + (1 | VANID) + (1|NYCCD)
lme_compet <- glmer(lme_compet_form, data = pb_long, family = binomial(), nAGQ = 0) 


### Table / effect output for paper. "mainregs.tex" ------------------------------------------------------------------------------------------------------------------------------------------------------ 
## calculating average effect from final model
alleffects <- simcf::extractdata(lme_compet_form, pb_long, na.rm = T) %>%
  margins::margins(lme_compet, data = ., variables = "after_pb") %>% 
  (function(x){
    res <- list()
    res$effect <- mean(x$dydx_after_pb)
    res$upper <- res$effect + sqrt(unique(x$Var_dydx_after_pb))*1.96
    res$lower <- res$effect - sqrt(unique(x$Var_dydx_after_pb))*1.96
    res
  })
print(alleffects)

## trying again with ggeffect

#----------------------------------------------------------------------------------------------------------------------------------------
### repeating final reg for compet match ----------------------------------------------------------------------------
set.seed(5022019)
matching_model <- "Compet + tract, fine"
allout <- readRDS(paste0("data/cleaned_R_results/matching_res.RDS"))
matched_data2 <- allout %>% filter(match_type == matching_model) %>% pluck("outdf", 1) 

# Load and process voterfile - attaching full competitiveness measures
voterfile <- readRDS(paste0("data/cleaned_R_results/voterfile_for_matching.rds"))

# keep matched
voterfile <- voterfile %>% 
  semi_join(matched_data, by = "VANID")

# pull out previously attached partial competitiveness records to sreshape 
vf_compet <- voterfile %>% 
  select(VANID, starts_with("comp_"))

voterfile <- voterfile %>% 
  select(-starts_with("comp_"))

### Load/reshape competitiveness ---------------------------------------------------------------------------------
vf_compet <- reshape_vf_compet(vf_compet)

### process, make long, attach competitiveness
pb_long <- preprocess_lmer(matched_data2)

#### Set reference levels for factor variables 
pb_long <- create_model_factors(pb_long) 

### remove 2011 + 2015 as non-informative years
pb_long <- pb_long %>% 
  filter(! year %in% c(2011, 2015))

lme_compet2 <- glmer(lme_compet_form, data = pb_long, family = binomial(), nAGQ = 0) 

alleffects2 <- simcf::extractdata(lme_compet_form, pb_long, na.rm = T) %>%
  margins::margins(lme_compet2, data = ., variables = "after_pb") %>% 
  (function(x){
    res <- list()
    res$effect <- mean(x$dydx_after_pb)
    res$upper <- res$effect + sqrt(unique(x$Var_dydx_after_pb))*1.96
    res$lower <- res$effect - sqrt(unique(x$Var_dydx_after_pb))*1.96
    res
  })
print(alleffects2)
#----------------------------------------------------------------------------------------------------------------------------------------

all_res <- list(lme_minimal = lme_minimal, lme_demog = lme_demog, lme_tract = lme_tract, lme_final = lme_final, 
                lme_compet =lme_compet, lme_compet2 = lme_compet2, alleffects = alleffects, alleffects2 = alleffects2)
save(all_res, file = "data/cleaned_R_results/main_effects.rds")

# load("data/cleaned_R_results/main_effects.rds")
# all_res$lme_compet2 <- lme_compet2

library(stargazer)
all_res[c("lme_minimal", "lme_demog", "lme_final", "lme_compet", "lme_compet2")] %>% 
  stargazer(out = "Paper_text/Tables/mainregs_raw.tex", label = "main_results",
          title = "Individual voter turnout difference-in-difference regression results: no interactions",
          column.labels = c("Minimal", "Demog.", "Tract",  "Compet.", "Matched Comp."),
          order = c("^pb$", "^after\\_pb$", "^election\\_typep$", "^election\\_typepp$",
                    "^RaceB$", "^RaceA$",  "^RaceH$", "^RaceU$", "^Female$",
                    "^age$", "^I\\(age\\^2\\)$", "I\\(age\\_at\\_vote < 18\\)TRUE",
                    "^college\\_pct$", "^medhhinc\\_10k$", "^majmatchTRUE$", "compet"),
          covariate.labels = c("PB district", "After PB", "Primary election", "Pres. Primary",
                               "Black", "Asian", "Hispanic", "Race Unknown", "Female",
                               "Age in years", "Age\\textsuperscript{2}", "18+ at vote",
                               "\\% college educated", "Median HH income", "Majority Race", "Competitiveness"),
          dep.var.labels.include = FALSE, dep.var.caption = "",
          model.numbers = FALSE,
          digit.separator = "",intercept.bottom = TRUE, no.space = TRUE,
          omit = c("year"), omit.labels = c("Year fixed effects?"), 
          keep.stat = c("n", "aic", "bic", "n"),
          star.char = "*", star.cutoffs = 0.05,
          align = TRUE,
          notes = "\\parbox[t]{.85\\textwidth}{\\footnotesize \\textit{Note:} Difference-in-difference regression results from multilevel mixed effect logistic models of individual turnout in a given election, including random effects for individual and council districts.  Standard errors reported in parentheses and statistical significance at $p<0.05$ indicated by $^{*}$.}",
          notes.label = "",
          notes.align = "l",
          notes.append = FALSE)

# more fully interacted model
# lme_interact_form <- turned_out ~ pb + after_pb*Race + after_pb*majmatch + after_pb*Female + compet + Race + Female + as.factor(year)*Race + as.factor(year)*Female + as.factor(year)*majmatch + election_type + age + I(age^2) + I(age_at_vote < 18) + medhhinc_10k + college_pct + majmatch + (1 | VANID) + (1|NYCCD)
# lme_interact <- glmer(lme_interact_form, data = pb_long, family = binomial(), nAGQ = 0) 

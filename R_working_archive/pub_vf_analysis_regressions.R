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
library(tidyr)
library(ggplot2)
library(lubridate)
library(stringr)
library(lme4)
library(margins)

### Creating/loading matched datasets
# source("pub_vf_matching.R")
load("vf_analysis.RData")

####  replicating transformation and first regressions with the matched data -------------------------------------------------------------------------------------

## process analysis df to pb_long df for analysis (creating wide pb table along the way)
source("create_pb_long.R")

pb_long <- create_pb_long(vf_analysis)

#### Set reference levels for factor variables 
pb_long <- pb_long %>%
  group_by() %>% 
  mutate(Race = relevel(as.factor(Race), ref = "W"),
         election_type = relevel(as.factor(election_type), ref = "g"))

####  Model explorations ---------------------------------------------------------------------------------------------------------------------------
# 
# ## looking at a very basic linear regression predicting turnout
# bas_log <- lm(turned_out ~ pb + after_pb + as.factor(year) + election_type , data = pb_long)
# summary(bas_log)
# 
# bas_log_all <- lm(turned_out ~ pb + after_pb + as.factor(year) + election_type +
#                     Female + Race + age + medhhinc + white + college + majmatch, data = pb_long)
# summary(bas_log_all) ## R-Squared = .31!
# 
# ## Quick comparison of linear and logit models with covariates - this is mostly just to give a sense of the relative magnitude of effects in the two model approaches
# library(margins)
# covar_formula <- turned_out ~ pb + after_pb + as.factor(year) +  election_type  + Race + age + Female + medhhinc + college + white + majmatch
# covar_logit <- pb_long %>% glm(covar_formula, data = ., family = binomial())
# summary(covar_logit)
# dydx(pb_long, covar_logit, "after_pb", change = c(0,1))[[1]] %>% mean
# covar_lm <- lm(covar_formula, data = pb_long)
# summary(covar_lm)
# dydx(pb_long, covar_lm, "after_pb", change = c(0,1))[[1]] %>% mean
# 
# ##### Trying with lmer getting random effects for individuals 
# 
# logit_lme_f <- turned_out ~ pb + after_pb + Race + as.factor(year) + election_type + age + medhhinc + white + college + majmatch + (1 | VANID) 
# lme_logit <- glmer(logit_lme_f, data = pb_long, family = binomial(), nAGQ = 0)   
# summary(lme_logit)
# 
# ## Comparing inclusion of NYCDD random effects - fit is improved by including NYCDD
# 
# logit_full_fm <- turned_out ~ pb + after_pb + Race + Female + as.factor(year) + election_type + age + medhhinc + white + college + majmatch + (1 | VANID) + (1|NYCCD)
# lme_full <-  glmer(logit_full_fm, data = pb_long, family = binomial(), nAGQ = 0) 
# summary(lme_full)
# 
# AIC(lme_full)
# AIC(lme_logit)
# BIC(lme_full)
# BIC(lme_logit)
# 
# AICcollege <- AIC(lme_full)
# BICcollege <- BIC(lme_full)
# 
# dydx(pb_long, lme_full, "after_pb", change = c(0,1))[[1]] %>% mean
# 
# ## testing not including college
# logit_full_fm_nocollege <- turned_out ~ pb + after_pb + as.factor(year) + election_type + Race + age + medhhinc + white + majmatch + (1 | VANID) + (1|NYCCD)
# lme_full_ncollege <-  glmer(logit_full_fm_nocollege, data = pb_long, family = binomial(), nAGQ = 0) 
# AIC(lme_full_ncollege)
# AIC(lme_full)
# BIC(lme_full_ncollege)
# BIC(lme_full)
# 
# dydx(pb_long, lme_full_ncollege, "after_pb", change = c(0,1))[[1]] %>% mean
# ## all this points to keeping college in the analyis. Not sure why I originally dropped it...
# 
# ## testing including non-linear effects for age and medhhinc (as suggested by plotting)
# logit_age2_form <- turned_out ~ pb + after_pb + Race + Female + as.factor(year) + election_type + age + I(age^2) + medhhinc + white + college + majmatch + (1 | VANID) + (1|NYCCD)
# logit_med2_form  <-  turned_out ~ pb + after_pb + Race + Female + as.factor(year) + election_type + age + I(medhhinc^2) + medhhinc + white + college + majmatch + (1 | VANID) + (1|NYCCD)
# logit_lmed_form  <-  turned_out ~ pb + after_pb + Race + Female + as.factor(year) + election_type + age + log(medhhinc) + white + college + majmatch + (1 | VANID) + (1|NYCCD)
# 
# lme_age2 <- glmer(logit_age2_form, data = pb_long, family = binomial(), nAGQ = 0) 
# lme_med2 <- glmer(logit_med2_form, data = pb_long, family = binomial(), nAGQ = 0) 
# lme_lmed <- glmer(logit_lmed_form, data = pb_long, family = binomial(), nAGQ = 0) 
# 
# AIC(lme_full)
# AIC(lme_age2)
# AIC(lme_med2)
# AIC(lme_lmed)
# 
# BIC(lme_full)
# BIC(lme_age2)
# BIC(lme_med2)
# BIC(lme_lmed)
# 
# #incl white?:
# lme_nowhite_form <- turned_out ~ pb + after_pb + Race + Female + as.factor(year) + election_type + age + I(age^2) + medhhinc + college + majmatch + (1 | VANID) + (1|NYCCD)
# lme_nowhite <- glmer(lme_nowhite_form, data = pb_long, family = binomial(), nAGQ = 0) 
# 
# AIC(lme_age2)
# AIC(lme_nowhite)
# BIC(lme_age2)
# BIC(lme_nowhite)
# 
# ## % white isn't contributing, much once majority race is included (esp. since matched on nonwhite)
# ##  BIC and AIC  agree that it does not improve the model
# table(pb_long$turned_out, fitted(lme_age2)>= .5)
# table(pb_long$turned_out == as.numeric(fitted(lme_age2)>= .5)) %>% prop.table() #---> 87% pcp
# 
# table(pb_long$turned_out, fitted(lme_nowhite)>= .5)
# table(pb_long$turned_out == as.numeric(fitted(lme_nowhite)>= .5)) %>% prop.table() #---> 87% pcp
# ## Percent correctly predicted is basically the same for the two models.
# 
# #incl gender?:
# lme_nosex_form <- turned_out ~ pb + after_pb + Race + as.factor(year) + election_type + age + I(age^2) + medhhinc + college + majmatch + (1 | VANID) + (1|NYCCD)
# lme_nosex <- glmer(lme_nosex_form, data = pb_long, family = binomial(), nAGQ = 0) 
# 
# AIC(lme_nowhite)
# AIC(lme_nosex)
# BIC(lme_nowhite)
# BIC(lme_nosex)
# 
# ## Both AIC and BIC actually encourage omitting gender from the modle - however, it's not
# ## a huge difference and I want to be able to include gender in the the subgroup breakdowns,
# ## so should include it for comparability
# 
# # Age at vote eligibility flag
# 
# logit_elig_fm <- turned_out ~ pb + after_pb + Race + Female + as.factor(year) + election_type + age + I(age^2) + I(age_at_vote < 18) + medhhinc  + college + majmatch + (1 | VANID) + (1|NYCCD)
# lme_elig <-  glmer(logit_elig_fm, data = pb_long, family = binomial(), nAGQ = 0) 
# summary(lme_elig)
# 
# AIC(lme_age2)
# AIC(lme_elig)
# BIC(lme_age2)
# BIC(lme_elig)
# 
# ## including flag for age at vote makes a huge improvement in model fit. Use it!

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

### LMER model incl compet  ---------------------------------------------------------------------------------------------------------------------------------------------------------------------
lme_compet_form <- turned_out ~ pb + after_pb + Race + Female + compet + as.factor(year) + election_type + age + I(age^2) + I(age_at_vote < 18) + medhhinc_10k + college_pct + majmatch + (1 | VANID) + (1|NYCCD)
lme_compet <- glmer(lme_compet_form, data = pb_long, family = binomial(), nAGQ = 0) 


### Table / effect output for paper. "mainregs.tex" ------------------------------------------------------------------------------------------------------------------------------------------------------ 
## calculating average effect from final model
meaneffect <-  pb_long %>% 
  filter(! year %in% c(2011,2015) )%>%
           simcf::extractdata(lme_compet_form,., na.rm = T) %>% 
           margins::dydx(., lme_compet, "after_pb", change = c(0,1)) %>% 
  .$dydx_after_pb %>% 
  mean() 
print(meaneffect)
all_models <- list(lme_minimal, lme_demog, lme_tract, lme_final, lme_compet)
save(all_models, file = "data/cleaned_R_results/mainresults.RData")

library(stargazer)
stargazer(all_models[2:5], #type = "text",
          out = "Paper_text/Tables/mainregs.tex", label = "main_results",
          title = "Individual voter turnout difference-in-difference regression results: no interactions",
          column.labels = c("Minimal", "Demog.", "Tract", "Majority Match"),
          order = c("^pb$", "^after\\_pb$", "^election\\_typep$", "^election\\_typepp$",
                    "^RaceB$", "^RaceA$",  "^RaceH$", "^RaceU$", "^Female$",
                    "^age$", "^I\\(age\\^2\\)$", "I\\(age\\_at\\_vote < 18\\)TRUE",
                    "^college\\_pct$", "^medhhinc\\_10k$", "^majmatchTRUE$"),
          covariate.labels = c("PB district", "After PB", "Primary election", "Pres. Primary",
                               "Black", "Asian", "Hispanic", "Race Unknown", "Female",
                               "Age in years", "Age\\textsuperscript{2}", "18+ at vote",
                               "\\% college educated", "Median HH income", "Majority Race"),
          dep.var.labels.include = FALSE, dep.var.caption = "",
          digit.separator = "",intercept.bottom = TRUE, no.space = TRUE,
          omit = c("year"), omit.labels = c("Year fixed effects?"), 
          keep.stat = c("n", "aic", "bic", "n"),
          star.char = "*", star.cutoffs = 0.05,
          align = TRUE,
          notes = "\\parbox[t]{.85\\textwidth}{\\footnotesize \\textit{Note:} Difference-in-difference regression results from multilevel mixed effect logistic models of individual turnout in a given election, including random effects for individual and council districts.  Standard errors reported in parentheses and statistical significance at $p<0.05$ indicated by $^{*}$.}",
          notes.label = "",
          notes.align = "l",
          notes.append = FALSE)

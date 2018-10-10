#################################################################################################################################################
### 
### PB and voter turnout: Regression analyses from matched dataset
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
library(simcf)
library(MASS)

### Creating/loading matched datasets
# source("vf_matching.R")

####  replicating transformation and first regressions with the matched data -------------------------------------------------------------------------------------

 load("vf_analysis.Rdata")    

## process analysis df to pb_long df for analysis (creating wide pb table along the way)
source("create_pb_long.R")
source("addNYCCD.R")
pb_long <- create_pb_long(vf_analysis)

#### Set reference levels for factor variables 
pb_long <- pb_long %>%
  group_by() %>% 
  mutate(Race = relevel(as.factor(Race), ref = "W"),
         election_type = relevel(as.factor(election_type), ref = "g"))

####  Model explorations ---------------------------------------------------------------------------------------------------------------------------

## looking at a very basic linear regression predicting turnout
bas_log <- lm(turned_out ~ pb + after_pb + as.factor(year) + election_type , data = pb_long)
summary(bas_log)

bas_log_all <- lm(turned_out ~ pb + after_pb + as.factor(year) + election_type +
                    Female + Race + age + medhhinc + white + college + majmatch, data = pb_long)
summary(bas_log_all) ## R-Squared = .31!

## Quick comparison of linear and logit models with covariates - this is mostly just to give a sense of the relative magnitude of effects in the two model approaches
library(margins)
covar_formula <- turned_out ~ pb + after_pb + as.factor(year) +  election_type  + Race + age + Female + medhhinc + college + white + majmatch
covar_logit <- pb_long %>% glm(covar_formula, data = ., family = binomial())
summary(covar_logit)
dydx(pb_long, covar_logit, "after_pb", change = c(0,1))[[1]] %>% mean
covar_lm <- lm(covar_formula, data = pb_long)
summary(covar_lm)
dydx(pb_long, covar_lm, "after_pb", change = c(0,1))[[1]] %>% mean

##### Trying with lmer getting random effects for individuals -----------------------------------------------------------------------------------------------

logit_lme_f <- turned_out ~ pb + after_pb + Race + as.factor(year) + election_type + age + medhhinc + white + college + majmatch + (1 | VANID) 
lme_logit <- glmer(logit_lme_f, data = pb_long, family = binomial(), nAGQ = 0)   
summary(lme_logit)

## Comparing inclusion of NYCDD random effects - fit is improved by including NYCDD

logit_full_fm <- turned_out ~ pb + after_pb + Race + Female + as.factor(year) + election_type + age + medhhinc + white + college + majmatch + (1 | VANID) + (1|NYCCD)
lme_full <-  glmer(logit_full_fm, data = pb_long, family = binomial(), nAGQ = 0) 
summary(lme_full)

AIC(lme_full)
AIC(lme_logit)
BIC(lme_full)
BIC(lme_logit)

AICcollege <- AIC(lme_full)
BICcollege <- BIC(lme_full)

dydx(pb_long, lme_full, "after_pb", change = c(0,1))[[1]] %>% mean

## testing not including college
logit_full_fm_nocollege <- turned_out ~ pb + after_pb + as.factor(year) + election_type + Race + age + medhhinc + white + majmatch + (1 | VANID) + (1|NYCCD)
lme_full_ncollege <-  glmer(logit_full_fm_nocollege, data = pb_long, family = binomial(), nAGQ = 0) 
AIC(lme_full_ncollege)
AIC(lme_full)
BIC(lme_full_ncollege)
BIC(lme_full)

dydx(pb_long, lme_full_ncollege, "after_pb", change = c(0,1))[[1]] %>% mean
## all this points to keeping college in the analyis. Not sure why I originally dropped it...

## testing including non-linear effects for age and medhhinc (as suggested by plotting)
logit_age2_form <- turned_out ~ pb + after_pb + Race + Female + as.factor(year) + election_type + age + I(age^2) + medhhinc + white + college + majmatch + (1 | VANID) + (1|NYCCD)
logit_med2_form  <-  turned_out ~ pb + after_pb + Race + Female + as.factor(year) + election_type + age + I(medhhinc^2) + medhhinc + white + college + majmatch + (1 | VANID) + (1|NYCCD)
logit_lmed_form  <-  turned_out ~ pb + after_pb + Race + Female + as.factor(year) + election_type + age + log(medhhinc) + white + college + majmatch + (1 | VANID) + (1|NYCCD)

lme_age2 <- glmer(logit_age2_form, data = pb_long, family = binomial(), nAGQ = 0) 
lme_med2 <- glmer(logit_med2_form, data = pb_long, family = binomial(), nAGQ = 0) 
lme_lmed <- glmer(logit_lmed_form, data = pb_long, family = binomial(), nAGQ = 0) 

AIC(lme_full)
AIC(lme_age2)
AIC(lme_med2)
AIC(lme_lmed)

BIC(lme_full)
BIC(lme_age2)
BIC(lme_med2)
BIC(lme_lmed)

#incl white?:
lme_nowhite_form <- turned_out ~ pb + after_pb + Race + Female + as.factor(year) + election_type + age + I(age^2) + medhhinc + college + majmatch + (1 | VANID) + (1|NYCCD)
lme_nowhite <- glmer(lme_nowhite_form, data = pb_long, family = binomial(), nAGQ = 0) 

AIC(lme_age2)
AIC(lme_nowhite)
BIC(lme_age2)
BIC(lme_nowhite)

## % white isn't contributing, much once majority race is included (esp. since matched on nonwhite)
##  BIC and AIC  agree that it does not improve the model
table(pb_long$turned_out, fitted(lme_age2)>= .5)
table(pb_long$turned_out == as.numeric(fitted(lme_age2)>= .5)) %>% prop.table() #---> 87% pcp

table(pb_long$turned_out, fitted(lme_nowhite)>= .5)
table(pb_long$turned_out == as.numeric(fitted(lme_nowhite)>= .5)) %>% prop.table() #---> 87% pcp
## Percent correctly predicted is basically the same for the two models.

#incl gender?:
lme_nosex_form <- turned_out ~ pb + after_pb + Race + as.factor(year) + election_type + age + I(age^2) + medhhinc + college + majmatch + (1 | VANID) + (1|NYCCD)
lme_nosex <- glmer(lme_nosex_form, data = pb_long, family = binomial(), nAGQ = 0) 

AIC(lme_nowhite)
AIC(lme_nosex)
BIC(lme_nowhite)
BIC(lme_nosex)

## Both AIC and BIC actually encourage omitting gender from the modle - however, it's not
## a huge difference and I want to be able to include gender in the the subgroup breakdowns,
## so should include it for comparability

# Age at vote eligibility flag

logit_elig_fm <- turned_out ~ pb + after_pb + Race + Female + as.factor(year) + election_type + age + I(age^2) + I(age_at_vote < 18) + medhhinc  + college + majmatch + (1 | VANID) + (1|NYCCD)
lme_elig <-  glmer(logit_elig_fm, data = pb_long, family = binomial(), nAGQ = 0) 
summary(lme_elig)

AIC(lme_age2)
AIC(lme_elig)
BIC(lme_age2)
BIC(lme_elig)

## including flag for age at vote makes a huge improvement in model fit. Use it!

### LMER base model, no covars:
logit_minimal_form <- turned_out ~ pb + after_pb + as.factor(year) + election_type + (1| VANID) + (1|NYCCD)
lme_minimal <- glmer(logit_minimal_form, data = pb_long, family = binomial(), nAGQ = 0)

### LMER model only demographics + base
logit_demog_form <- turned_out ~ pb + after_pb  + as.factor(year) + election_type + Race + Female + age + I(age^2) + I(age_at_vote < 18) + (1| VANID) + (1|NYCCD)
lme_demog  <- glmer(logit_demog_form, data = pb_long, family = binomial(), nAGQ = 0)

### LMER model incl tract vars
logit_tract_form <- turned_out ~ pb + after_pb  + as.factor(year) + election_type +  Race + Female + age + I(age^2) + 
  I(age_at_vote < 18) + college_pct + medhhinc_10k +(1| VANID) + (1|NYCCD)
lme_tract  <- glmer(logit_tract_form, data = pb_long, family = binomial(), nAGQ = 0)

### LMER model incl majmatch
lme_final_form <- turned_out ~ pb + after_pb + Race + Female + as.factor(year) + election_type + age + I(age^2) + I(age_at_vote < 18) + medhhinc_10k + college_pct + majmatch + (1 | VANID) + (1|NYCCD)
lme_final <- glmer(lme_final_form, data = pb_long, family = binomial(), nAGQ = 0) 

# lme_final <- lme_nowhite
# 
library(texreg)
screenreg(list(lme_minimal, lme_demog, lme_tract, lme_final))

library(stargazer)
stargazer((list(lme_minimal, lme_demog, lme_tract, lme_final)),
          out = "Paper_text/Tables/mainregs.tex", label = "main_results",
          dep.var.labels.include = FALSE, dep.var.caption = "",
          digit.separator = "",intercept.bottom = TRUE, no.space = TRUE,
          omit = c("year"), omit.labels = c("Year fixed effects?"), 
          keep.stat = c("n", "aic", "bic", "n"))

## explorign some basic predictions from thi
#ind effects - confirming that they're not correlated with race
inds <- ranef(lme_full)$VANID
inds$VANID <- as.numeric(rownames(inds))
inds <- inds %>% left_join(pb)
by(inds, inds$race, function(x) mean(x[["(Intercept)"]]))
by(inds, inds$pb, function(x) mean(x[["(Intercept)"]]))

## this highlights that there is a structure to the random effects for districts that sees a bump up for pb districts and down for non-pb
dists <- ranef(lme_full)$NYCCD
dists$NYCCD <- as.numeric(rownames(dists))
mean(dists[dists$NYCCD %in% pbdistricts, "(Intercept)"])
mean(dists[! dists$NYCCD %in% pbdistricts, "(Intercept)"])


preds <- predict(lme_full, type = "response")
preds <- pb_long %>%  select(VANID, race, year, age, election_type, medhhinc, white, NYCCD, after_pb) %>% 
  bind_cols(predvote = preds)
by(preds, list(preds$race, preds$after_pb), function(x) mean(x[["predvote"]]))
preds %>% group_by(race, after_pb, year) %>% summarize(predvote = mean(predvote)) %>% arrange(desc(year), race, after_pb) %>% View()

# fitting model unconditioned by race - established as reference point, not actually referred to anywhere before
unconditioned <- glmer(turned_out ~ pb + after_pb + as.factor(year) + election_type + age + medhhinc + white +  (1 | VANID) + (1|NYCCD)) 

#predicted turnout - simple version with predict and ggplot that doesn't return confidence intervals
nd <- expand.grid(pb = 0, after_pb = c(0, 1), Race = c("U", "A", "H", "B", "W"), 
                  year = c(2016), election_type = "g", age = median(pb_long$age), medhhinc = median(pb_long$medhhinc), white = median(pb_long$white), stringsAsFactors = FALSE)
nd$preds <-  predict(lme_full, newdata = nd, type = "response", re.form = NA)

nd  %>%  
  mutate(after_pb = factor(after_pb, levels = c(0,1), labels = c("No PB", "After PB")),
         Race = factor(Race, levels = c("W", "B", "H", "A", "U"), labels = c("White", "Black", "Hispanic", "Asian", "Unknown"))) %>% 
  ggplot(aes(y = preds, x = Race)) + 
  # geom_segment(aes(xend = Race, y = 0, yend = preds, color = as.factor(after_pb)))+
  geom_point(aes(color = as.factor(after_pb))) +
  labs(title = "Predicted probability of voting in general election in 2016 \n before and after PB (in a non-PB district)", 
       x = "", y = "Predicted probability of Voting", color = "") +
  coord_flip() +
  theme_minimal()

######### Conditioning on Median Income ---- 

logit_full_fm_mi <- turned_out ~ pb + after_pb*medhhinc + Race + as.factor(year) + election_type + age + white +  (1 | VANID) + (1|NYCCD)
lme_full_mi <-  glmer(logit_full_fm_mi, data = pb_long, family = binomial(), nAGQ = 0)   

AIC(lme_full)
AIC(lme_full_mi)
BIC(lme_full)
BIC(lme_full_mi)
### Model fit is worse when impact of PB is conditioned on tract median income

#predicted turnout
nd_mi <- expand.grid(pb = 0, after_pb = c(0, 1), Race = "B",
                     year = c(2016), election_type = "g", age = median(pb_long$age), medhhinc = quantile(pb_long$medhhinc, probs = c(.1, .9)),
                     white = median(pb_long$white), stringsAsFactors = FALSE)
nd_mi$preds <-  predict(lme_full_mi, newdata = nd_mi, type = "response", re.form = NA)


nd_mi  %>%  
  mutate(after_pb = factor(after_pb, levels = c(0,1), labels = c("No PB", "After PB")),
         medhhinc = factor(medhhinc)) %>% 
  ggplot(aes(y = preds, x = medhhinc)) + 
  #geom_segment(aes(xend = medhhinc, y = 0, yend = preds, color = as.factor(after_pb)))+
  geom_point(aes(color = as.factor(after_pb))) +
  labs(title = "Predicted probability of voting in general election in 2016 \n before and after PB (in a non-PB district)", 
       x = "Median Census Tract HH Income", y = "Predicted probability of Voting", color = "") +
  coord_flip() +
  theme_minimal()



################ OLDER CODE EXPERIMENTING WITH SOLUTIONS THAT DIDN'T USE SIMCF FOR PREDICTIONS & PLOTTING.  -----------------------------
## Running separate regressions for each race, no pooling - limited in that predict does not produce confidence intervals 
## Partial pooling with simcf for CIs was preferable.
library(broom)

for (t in c("after_pb", "after_pb:RaceB", "after_pb:RaceH", "after_pb:RaceW", "after_pb:RaceA")){
  mean(dydx(head(pb_long), lme_full, t))
}

newdat <-  expand.grid(pb = 0, after_pb = c(0, 1), year = c(2016), election_type = "g")
regressPredict <- function(df, form, newdat){
  newdat$pred <- glmer(form, data = df, family = binomial(), nAGQ = 0) %>% 
    predict(., newdata = newdat, type = "response", re.form = NA)
  return(newdat)
}

glmer_byRace <- pb_long %>% 
  group_by(Race) %>% 
  do(tidy(glmer(logit_lme_f, data = ., family = binomial(), nAGQ = 0)))

glmer_predbyRace <- pb_long %>% 
  group_by(Race) %>% 
  do(regressPredict(df = ., form = logit_lme_f, newdat = newdat))

ggplot(subset(glmer_byRace, term == "after_pb"), aes(y = estimate, x = Race)) + 
  geom_pointrange(aes(ymin = estimate - 1.96*std.error, ymax = estimate + 1.96*std.error)) +
  coord_flip() + theme_minimal()

tidy(lme_logit) %>% 
  mutate(Race = "All") %>% 
  bind_rows(glmer_byRace) %>% 
  filter(term == "after_pb") %>% 
  mutate(Race = factor(Race, levels = c("All", "W", "B", "H", "A", "U"), labels = c("All", "White", "Black", "Hispanic", "Asian", "Unknown"))) %>% 
  ggplot(aes(y = estimate, x = Race)) + 
  geom_pointrange(aes(ymin = estimate - 1.96*std.error, ymax = estimate + 1.96*std.error)) +
  coord_flip() + theme_minimal()


newdat %>% 
  mutate(pred = predict(lme_logit, newdata = ., type = "response", re.form = NA),
         Race = "All") %>%
  bind_rows(glmer_predbyRace) %>% 
  mutate(after_pb = factor(after_pb, levels = c(0,1), labels = c("No PB", "After PB")),
         Race = factor(Race, levels = c("All", "W", "B", "H", "A", "U"), labels = c("All", "White", "Black", "Hispanic", "Asian", "Unknown"))) %>% 
  ggplot(aes(y = pred, x = Race)) + 
  geom_point(aes(color = after_pb)) +
  labs(title = "Predicted probability of voting in general election in 2016 \nwith and without PB (in a non-PB district)", 
       x = "", y = "Predicted probability", color = "") +
  coord_flip() +
  theme_minimal()


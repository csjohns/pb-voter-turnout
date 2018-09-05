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
# Dealing with DoB, calculating total PB votes (note that these will usually be off by 1 because 2014 has too many voters (everyone who came before))

pb <- vf_analysis %>% 
  group_by(VANID) %>%
  mutate(DoB = ymd(DoB),
         totpb = sum(pb_2012, pb_2013, pb_2014, pb_2015 , pb_2016, na.rm = T)
  ) %>% 
  dplyr::select(-pp_2012, -p_2011)


### Reshaping long for regression -------------------------------------------------------------------------------------------------------------------------
## This is a multi-step process, creating the long pb votes, and the long regular votes separately, then joining.

elec_long <- pb %>% dplyr::select(-starts_with("pb_")) %>%
  gather(election, turned_out, starts_with("p_2"), starts_with("g_2"), starts_with("pp_")) %>%
  separate(election, c("election_type", "year")) %>%
  mutate(#turned_out = ifelse(turned_out != "", 1, 0),
    year = as.numeric(year)) 

pb_long <- pb %>% filter(pb == 1) %>% dplyr::select(VANID, totpb, starts_with("pb_")) %>% 
  gather( year, pb, starts_with("pb_")) %>% 
  mutate(year = as.numeric(str_replace(year, "pb_", "")),
         pb = as.numeric(pb)) %>% 
  full_join(filter(elec_long, pb == 1) %>% dplyr::select(VANID, year, totpb) %>% distinct())
glimpse(pb_long)
summary(pb_long)

# this code calculates pb start year - not that this code is good even though 2014 is a wacky error year since voters who didn't vote before 2014 will indeed have their start year be 2014
pb_long <- pb_long %>% group_by(VANID) %>%
  arrange(VANID, year) %>%
  mutate(pbyear  = ifelse(pb == 1, year, NA),
         pb_start = min(pbyear, na.rm = TRUE),
         pb_start = ifelse(pb_start == Inf, NA, pb_start)
  )
summary(pb_long)

pb_long <- pb_long %>% dplyr::select(-pb) %>% full_join(elec_long) 
pb_long <- pb_long %>%
  group_by(VANID) %>% 
  #mutate(pb_start_group = min(pb_start, na.rm = TRUE)) %>% filter(pb == 1 & year %in% c(2016,2017)) %>% arrange(VANID, year) %>% View
  group_by() %>% 
  mutate(pb = ifelse(is.na(pb), 0, pb),
         after_pb = as.numeric(year >= pb_start),
         after_pb = ifelse(is.na(after_pb), 0, after_pb),
         # repeater = totpb > 1, removing this because errors in 2014 means every early voter is a repeater, which isn't correct
         age_at_vote = year - year(DoB) ,
         Female = ifelse(Sex == "F", 1, 0))
## I think there are some nonsense ages in here and I need to investigate DoB coding more

pb_long <- pb_long %>%  filter(year >= 2008)

pb_long <- pb_long %>% 
  group_by(cem_group, year, election_type) %>% 
  mutate(post = max(after_pb))


####  Model explorations ---------------------------------------------------------------------------------------------------------------------------



## looking at a very basic linear regression predicting turnout
bas_log <- lm(turned_out ~ pb + after_pb + as.factor(year) + election_type , data = pb_long)
summary(bas_log)

bas_log_all <- lm(turned_out ~ pb + after_pb + as.factor(year) + election_type +
                    Sex + Race + age + medhhinc + white + college + majmatch, data = pb_long)
summary(bas_log_all) ## R-Squared = .31!

## Quick comparison of linear and logit models with covariates - this is mostly just to give a sense of the relative magnitude of effects in the two model approaches
library(margins)
covar_formula <- turned_out ~ pb + after_pb + as.factor(year) +  election_type  + Race + age + Sex + medhhinc + college + white + majmatch
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

logit_full_fm <- turned_out ~ pb + after_pb + Race + Sex + as.factor(year) + election_type + age + medhhinc + white + college + majmatch + (1 | VANID) + (1|NYCCD)
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
logit_age2_form <- turned_out ~ pb + after_pb + Race + Sex + as.factor(year) + election_type + age + I(age^2) + medhhinc + white + college + majmatch + (1 | VANID) + (1|NYCCD)
logit_med2_form  <-  turned_out ~ pb + after_pb + Race + Sex + as.factor(year) + election_type + age + I(medhhinc^2) + medhhinc + white + college + majmatch + (1 | VANID) + (1|NYCCD)
logit_lmed_form  <-  turned_out ~ pb + after_pb + Race + Sex + as.factor(year) + election_type + age + log(medhhinc) + white + college + majmatch + (1 | VANID) + (1|NYCCD)

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
lme_nowhite_form <- turned_out ~ pb + after_pb + Race + as.factor(year) + election_type + age + medhhinc + college + majmatch + (1 | VANID) + (1|NYCCD)
lme_nowhite <- glmer(lme_nowhite_form, data = pb_long, family = binomial(), nAGQ = 0) 

AIC(lme_full)
AIC(lme_nowhite)
BIC(lme_full)
BIC(lme_nowhite)

## % white isn't contributing much once majority race is included (esp. since matched on nonwhite)
##  BIC and AIC disagree on whether it is worth it (improves model fit) - maybe leave out

#incl gender?:
lme_nosex_form <- turned_out ~ pb + after_pb + Race + as.factor(year) + election_type + age + medhhinc + college + majmatch + (1 | VANID) + (1|NYCCD)
lme_nosex <- glmer(lme_nowhite_form, data = pb_long, family = binomial(), nAGQ = 0) 

AIC(lme_full)
AIC(lme_nosex)
BIC(lme_full)
BIC(lme_nosex)

## Both AIC and BIC agree that sex doesn't add anything to the model - however, it's not
## a huge difference and I want to be able to include gender in the the subgroup breakdowns,
## so should include it for comparability

# Age at vote eligibility flag

logit_elig_fm <- turned_out ~ pb + after_pb + Race + Sex + as.factor(year) + election_type + age + I(age^2) + I(age_at_vote < 18) + medhhinc + white + college + majmatch + (1 | VANID) + (1|NYCCD)
lme_elig <-  glmer(logit_elig_fm, data = pb_long, family = binomial(), nAGQ = 0) 
summary(lme_elig)

AIC(lme_age2)
AIC(lme_elig)
BIC(lme_age2)
BIC(lme_elig)

## including flag for age at vote makes a huge improvement in model fit. Use it!

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


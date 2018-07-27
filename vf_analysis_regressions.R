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
## SOMETHING WEIRD IS HAPPENING HERE WITH THE ADDITION OF 2017 - INVESTIGATE!!!

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
         age_at_vote = year - year(DoB) )
## I think there are some nonsense ages in here and I need to investigate DoB coding more

pb_long <- pb_long %>%  filter(year >= 2008)

pb_long <- pb_long %>% 
  group_by(cem_group, year, election_type) %>% 
  mutate(post = max(after_pb))


####  Model explorations ---------------------------------------------------------------------------------------------------------------------------

## this is doing some very basic plots exploring distribution of voting across subsets

ggplot(pb_long) + geom_bar(aes(x = as.factor(turned_out), fill = election_type), stat = "count", position = "dodge") + facet_wrap(~year)
ggplot(pb_long) + geom_bar(aes(x = as.factor(turned_out), fill = as.factor(after_pb)), position = "dodge") + facet_wrap(~year)

pb_long %>% mutate(turned_out = factor(turned_out, levels = c(0, 1), labels = c("Did not vote", "Voted")),
                   after_pb = factor(after_pb, levels = c(0,1), labels = c("No PB", "After PB"))) %>% 
  filter(election_type == "g" & year == 2008)  %>% 
  ggplot() + geom_bar(aes(x = as.factor(year),  fill = turned_out), position = "fill") + 
  facet_grid(Race~after_pb*pb, scales = "free") +coord_flip() + scale_y_continuous(labels = scales::percent) +
  labs(y="", x="") +theme_minimal() + labs(title = "Turnout in 2008 general election")


pb_long %>% mutate(turned_out = factor(turned_out, levels = c(0, 1), labels = c("Did not vote", "Voted")),
                   after_pb = factor(after_pb, levels = c(0,1), labels = c("No PB", "After PB"))) %>% 
  filter(election_type == "g" & year == 2016)  %>% 
  ggplot() + geom_bar(aes(x = as.factor(year),  fill = turned_out), position = "fill") + 
  facet_grid(Race~after_pb*pb, scales = "free") +coord_flip() + scale_y_continuous(labels = scales::percent) +
  labs(y="", x="") +theme_minimal() + labs(title = "Turnout in 2016 general election")

pb_long %>% mutate(turned_out = factor(turned_out, levels = c(0, 1), labels = c("Did not vote", "Voted")),
                   after_pb = factor(after_pb, levels = c(0,1), labels = c("No PB", "After PB"))) %>% 
  filter(election_type == "g" & year == 2012)  %>% 
  ggplot() + geom_bar(aes(x = as.factor(year),  fill = turned_out), position = "fill") + 
  facet_grid(Race~after_pb*pb, scales = "free") +coord_flip() + scale_y_continuous(labels = scales::percent) +
  labs(y="", x="") +theme_minimal() + labs(title = "Turnout in 2012 general election")



p <- pb_long %>% mutate(turned_out = factor(turned_out, levels = c(0, 1), labels = c("Did not vote", "Voted")),
                   after_pb = factor(after_pb, levels = c(0,1), labels = c("No PB", "After PB"))) %>% 
  filter(election_type == "g")  %>% 
  ggplot() + geom_bar(aes(x = as.factor(year),  fill = turned_out), position = "fill") + 
  facet_grid(Race~after_pb*pb, scales = "free") 
ggplotly(p)

pb_long %>%
  group_by(year, election_type,race, after_pb, pb) %>% 
  summarize(nvoters = n(),
            turnout = sum(turned_out, na.rm = T)/n()) %>% 
  filter(year == 2016, election_type == "g")

## looking at a very basic linear regression predicting turnout
bas_log <- lm(turned_out ~ pb + after_pb + as.factor(year) + election_type , data = pb_long)
summary(bas_log)


## Quick comparison of linear and logit models with covariates - this is mostly just to give a sense of the relative magnitude of effects in the two model approaches
library(margins)
covar_formula <- turned_out ~ pb + after_pb + as.factor(year) +  election_type  + Race + age + Sex + medhhinc + Sex
covar_logit <- pb_long %>% glm(covar_formula, data = ., family = binomial())
summary(covar_logit)
dydx(pb_long, covar_logit, "after_pb", change = c(0,1))[[1]] %>% mean
covar_lm <- lm(covar_formula, data = pb_long)
summary(covar_lm)
dydx(pb_long, covar_lm, "after_pb", change = c(0,1))[[1]] %>% mean

##### Trying with lmer getting random effects for individuals -----------------------------------------------------------------------------------------------

# Iterating for model selection: 
## increment over coefficient batches, compare AIC/BIC
## ** still to do

## making FE dummies for use with SIMCF - 
## Replicating Chris Adolph's makeFEdummies, from simcf package (not on CRAN, available from http://faculty.washington.edu/cadolph/?page=60)
makeFEdummies <- function (unit, names = NULL) {
  fe <- model.matrix(~factor(unit, levels = unique(as.character(unit))) - 1)
  if (is.null(names)) {
    colnames(fe) <- unique(as.character(unit))
  }
  else {
    colnames(fe) <- names
  }
  fe
}

pb_long_orig <- pb_long # making a copy in case all is f'ed up and don't want to rerun all processing code

pb_long <- pb_long_orig # resetting to the copied original 
raceFE <- makeFEdummies(pb_long$Race) 
colnames(raceFE) <- paste("race", colnames(raceFE), sep = "_")
yearFE <- makeFEdummies(pb_long$year)
colnames(yearFE) <- paste("year", colnames(yearFE), sep = "_")
election_typeFE <- makeFEdummies(pb_long$election_type)
colnames(election_typeFE) <- paste("election", colnames(election_typeFE), sep = "_")

pb_long <- bind_cols(pb_long, as.data.frame(raceFE), as.data.frame(yearFE), as.data.frame(election_typeFE))

## Confirming that models with FEs for simcf now produce identical coefs as base R models
# This was included as part of the process of de-bugging SIMCF but is kind of useful as a reality check/teaching moment so I left the code in.
logit_lme_f <- turned_out ~ pb + after_pb + as.factor(year) + election_type + (1 | VANID) 
lme_logit <- glmer(logit_lme_f, data = pb_long, family = binomial(), nAGQ = 0)   
summary(lme_logit)

logit_lme_simcf <- turned_out ~ pb + after_pb + 
  year_2009 + year_2010 + year_2011 + year_2012 + year_2013 + year_2014 + year_2015 + year_2016 +
  election_p + election_pp+ (1 | VANID) 
lme_logit_simcf <- glmer(logit_lme_simcf, data = pb_long, family = binomial(), nAGQ = 0)  
summary(lme_logit_simcf)

## Comparing inclusion of NYCDD random effects - fit is improved by including NYCDD
logit_full_fm <- turned_out ~ pb + after_pb*Race + as.factor(year) + election_type + age + medhhinc + white +  (1 | VANID) + (1|NYCCD)
lme_full <-  glmer(logit_full_fm, data = pb_long, family = binomial(), nAGQ = 0) 
summary(lme_full)

AIC(lme_full)
BIC(lme_full)
AIC(lme_logit)
BIC(lme_logit)

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


########################### CODE FOR MODEL TO PREDICT VIA SIMCF ------------------------------------------------------------
### Conditioned on Race: --------------------------------------

# specifying formula for model with xplicit dummies
logit_full_simcf <- turned_out ~ pb + after_pb*race_B + after_pb*race_A + after_pb*race_H + after_pb*race_U +
  year_2009 + year_2010 + year_2011 + year_2012 + year_2013 + year_2014 + year_2015 + year_2016 +
  election_p + election_pp + age + medhhinc + white + (1 | VANID) + (1|NYCCD)
# model formula omitting random effects for predictions of typical effects 
logit_full_form <- turned_out ~ pb + after_pb*race_B + after_pb*race_A + after_pb*race_H + after_pb*race_U +
  year_2009 + year_2010 + year_2011 + year_2012 + year_2013 + year_2014 + year_2015 + year_2016 +
  election_p + election_pp + age + medhhinc + white 

# fitting model
lme_full_simcf <- glmer(logit_full_simcf, data = pb_long, family = binomial(), nAGQ = 0)    #start = list(fixef = bas_log$coefficients), 

#identifying mean random effect for pb distrcts vs non pb districts
dists <- ranef(lme_full_simcf)$NYCCD
dists$NYCCD <- as.numeric(rownames(dists))
pb_ranef <- mean(dists[dists$NYCCD %in% pbdistricts, "(Intercept)"])
nopb_ranef <- mean(dists[! dists$NYCCD %in% pbdistricts, "(Intercept)"])

## creating simulated betas for nonpb districts
sims <- 1000
pe <- fixef(lme_full_simcf)
pe["(Intercept)"] <- pe["(Intercept)"] + nopb_ranef
vc <- vcov(lme_full_simcf) 
simbetas <- mvrnorm(sims, pe, vc)

## creating hypothetical scenarios for predictions

nscen <- length(unique(pb_long$Race))*2
xhyp <- cfMake(logit_full_form, pb_long, nscen = nscen, f = "min")

for (i in 1:nscen){
  xhyp <- cfChange(xhyp, "age", x = mean(pb_long$age), xpre = mean(pb_long$age), scen = i)
  xhyp <- cfChange(xhyp, "medhhinc", x = mean(pb_long$medhhinc), xpre = mean(pb_long$medhhinc), scen = i)
  xhyp <- cfChange(xhyp, "white", x = mean(pb_long$white), xpre = mean(pb_long$white), scen = i)
}
for (i in (nscen/2+1):nscen){
  xhyp <- cfChange(xhyp, "after_pb", x = 1, xpre = 0, scen = i)
  xhyp <- cfChange(xhyp, "year_2016", x = 1, xpre = 1, scen = i)
}
for (i in 1:(nscen/2)){
  xhyp <- cfChange(xhyp, "after_pb", x = 0, xpre = 0, scen = i)
}

for (i in c(2,7)){
  xhyp <- cfChange(xhyp, "race_B", x = 1, xpre = 1, scen = i)
}
for (i in c(3,8)){
  xhyp <- cfChange(xhyp, "race_H", x = 1, xpre = 1, scen = i)
}
for (i in c(4,9)){
  xhyp <- cfChange(xhyp, "race_A", x = 1, xpre = 1, scen = i)
}
for (i in c(5,10)){
  xhyp <- cfChange(xhyp, "race_U", x = 1, xpre = 1, scen = i)
}

## predicted probabilities in voting
yhyp <- logitsimev(xhyp, simbetas)

#expected change in predicted probabilities of voting
yhyp_fd <- logitsimfd(xhyp, simbetas)

preds <-  cbind(xhyp$x, as.data.frame(yhyp)) %>% 
  mutate(Race = rep(c("W", "B", "H", "A", "U"), 2)) 

### plotting predicted probabilities of voting with ggplot
preds %>% dplyr::select(after_pb, Race, pe, lower, upper) %>% 
  mutate(after_pb = factor(after_pb, levels = c(0,1), labels = c("No PB", "After PB")),
         Race = factor(Race, levels = c("W", "B", "H", "A", "U"), labels = c("White", "Black", "Hispanic", "Asian", "Unknown"))) %>% 
  ggplot(aes(y = pe, ymin = lower, ymax = upper, x = Race)) + 
  # geom_segment(aes(xend = Race, y = 0, yend = preds, color = as.factor(after_pb)))+
  geom_pointrange(aes(color = as.factor(after_pb))) +
  labs(title = "Predicted probability of voting in general election in 2016 \n before and after PB (in a non-PB district)", 
       x = "", y = "Predicted probability of Voting", color = "") +
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


library(simcf)
library(MASS)

rm(lme_age2, lme_elig, lme_full, lme_full_ncollege, lme_lmed, lme_logit, lme_med2, lme_nosex, lme_nowhite, covar_lm, covar_logit)

##### Predictions with SIMCF
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

#### Make FE Dummies
## Replicating Chris Adolph's makeFEdummies, from simcf package (not on CRAN, available from http://faculty.washington.edu/cadolph/?page=60)

## setting up pb_long from vf_analysis again

## process analysis df to pb_long df for analysis (creating wide pb table along the way)
load("vf_analysis.Rdata")
source("create_pb_long.R")
pb_long <- create_pb_long(vf_analysis)

pb_long_orig <- pb_long # making a copy in case all is f'ed up and don't want to rerun all processing code

pb_long <- pb_long_orig # resetting to the copied original 
raceFE <- makeFEdummies(pb_long$Race) 
colnames(raceFE) <- paste("race", colnames(raceFE), sep = "_")
yearFE <- makeFEdummies(pb_long$year)
colnames(yearFE) <- paste("year", colnames(yearFE), sep = "_")
election_typeFE <- makeFEdummies(pb_long$election_type)
colnames(election_typeFE) <- paste("election", colnames(election_typeFE), sep = "_")

pb_long <- bind_cols(pb_long, as.data.frame(raceFE), as.data.frame(yearFE), as.data.frame(election_typeFE))

################################################################################################################################################
######################## AGGREGATE MODEL ------------------------------------------------------------------------------------
### Estimated model
logit_base_simcf <- turned_out ~ pb + after_pb + election_p + election_pp +
  year_2009 + year_2010 + year_2011 + year_2012 + year_2013 + year_2014 + year_2015 + year_2016 + year_2017 +
  race_B + race_A + race_H + race_U + Female + age + I(age^2) + I(age_at_vote < 18) + 
  medhhinc_10k + college_pct + majmatch + (1 | VANID) + (1 | NYCCD)
lme_base_simcf <- glmer(logit_base_simcf, data = pb_long, family = binomial(), nAGQ = 0)    #start = list(fixef = bas_log$coefficients), 
#######################################################################################################################################
### Formula for simcf
logit_base_form <-  turned_out ~ pb + after_pb + election_p + election_pp +
  year_2009 + year_2010 + year_2011 + year_2012 + year_2013 + year_2014 + year_2015 + year_2016 + year_2017 +
  race_B + race_A + race_H + race_U + Female + age + I(age^2) + I(age_at_vote < 18) + 
  medhhinc_10k + college_pct + majmatch

#calculating PCP
compare <- data.frame(obs = lme_base_simcf@frame$turned_out, pred = as.numeric(fitted(lme_base_simcf)> 0.5))
table(compare$obs, compare$pred) %>% prop.table()

library(MASS)

sims <- 10000
pe <- fixef(lme_base_simcf)
vc <- vcov(lme_base_simcf) 
simbetas <- mvrnorm(sims, pe, vc)

nscen <- 4
xhyp <- cfMake(logit_base_form, pb_long, nscen = nscen, f = "mean")


for (i in 1:nscen){
  xhyp <- cfChange(xhyp, "year_2009", x = 0, xpre = 0, scen = i)
  xhyp <- cfChange(xhyp, "year_2010", x = 0, xpre = 0, scen = i)
  xhyp <- cfChange(xhyp, "year_2011", x = 0, xpre = 0, scen = i)
  xhyp <- cfChange(xhyp, "year_2012", x = 0, xpre = 0, scen = i)
  xhyp <- cfChange(xhyp, "year_2013", x = 0, xpre = 0, scen = i)
  xhyp <- cfChange(xhyp, "year_2014", x = 0, xpre = 0, scen = i)
  xhyp <- cfChange(xhyp, "year_2015", x = 0, xpre = 0, scen = i)
  xhyp <- cfChange(xhyp, "year_2016", x = 0, xpre = 0, scen = i)
  xhyp <- cfChange(xhyp, "year_2017", x = 0, xpre = 0, scen = i)
  xhyp <- cfChange(xhyp, "election_pp", x = 0, xpre = 0, scen = i)
  xhyp <- cfChange(xhyp, "election_p", x = 0, xpre = 0, scen = i)
  xhyp <- cfChange(xhyp, "pb", x = 0, xpre = 0, scen = i)
}

for (i in 1){
  xhyp <- cfChange(xhyp, "year_2013", x = 1, xpre = 1, scen = i)
}

for (i in 2){
  xhyp <- cfChange(xhyp, "year_2014", x = 1, xpre = 1, scen = i)
}

for (i in 3){
  xhyp <- cfChange(xhyp, "year_2016", x = 1, xpre = 1, scen = i)
}

for (i in 4){
  xhyp <- cfChange(xhyp, "year_2017", x = 1, xpre = 1, scen = i)
}

for (i in 1:nscen){
  xhyp <- cfChange(xhyp, "after_pb", x = 1, xpre = 0, scen = i)
}

xhyp_pre <- xhyp
for (i in 1:nscen){
  xhyp_pre <- cfChange(xhyp_pre, "after_pb", x = 0, xpre = 0, scen = i)
}

yhyp <- logitsimev(xhyp, simbetas)
yhyp_pre <- logitsimev(xhyp_pre, simbetas)

yhyp_fd <- logitsimfd(xhyp, simbetas)

preds <-  cbind(xhyp$x, as.data.frame(yhyp)) 
preds <- cbind(xhyp_pre$x, as.data.frame(yhyp_pre)) %>% 
  bind_rows(preds) %>% 
  gather(year, ind, starts_with("year_")) %>% 
  filter(ind ==1) %>% 
  separate(year, into = c("yr", "year")) %>% 
  dplyr::select(-yr, ind)
library(scales)
preds %>%
  dplyr::select(after_pb, year, pe, lower, upper) %>% 
  mutate(after_pb = factor(after_pb, levels = c(0,1), labels = c("No PB", "After PB"))) %>% 
  ggplot(aes(y = pe, ymin = lower, ymax = upper, x = year)) + 
  # geom_segment(aes(xend = Race, y = 0, yend = preds, color = as.factor(after_pb)))+
  geom_pointrange(aes(color = as.factor(after_pb))) +
  # scale_y_continuous(labels = percent) +
  labs(#title = "Probability of voting in a general election", 
       #subtitle = "Predictions for non-PB voters, showing effect of hypothetical participation in PB vote", 
       x = "", y = "Predicted probability of voting", color = "") +
  # coord_flip() +
  theme_minimal() +
  theme(axis.title=element_text(size=11))
ggsave("Paper/Figs/base_by_year.pdf", width = 6, height = 4)
#ggsave("Paper/Figs/base_by_year.png", width = 6, height = 5)


################################################################################################################################################
######################## DISAGGREGATE BY RACE ------------------------------------------------------------------------------------
### Estimated model
logit_race_simcf <- turned_out ~ pb + after_pb + election_p + election_pp +
  year_2009 + year_2010 + year_2011 + year_2012 + year_2013 + year_2014 + year_2015 + year_2016 + year_2017 +
  after_pb*race_B + after_pb*race_A + after_pb*race_H + after_pb*race_U +
  year_2009*race_B + year_2009*race_A + year_2009*race_H + year_2009*race_U +
  year_2010*race_B + year_2010*race_A + year_2010*race_H + year_2010*race_U +
  year_2011*race_B + year_2011*race_A + year_2011*race_H + year_2011*race_U +
  year_2012*race_B + year_2012*race_A + year_2012*race_H + year_2012*race_U +
  year_2013*race_B + year_2013*race_A + year_2013*race_H + year_2013*race_U +
  year_2014*race_B + year_2014*race_A + year_2014*race_H + year_2014*race_U +
  year_2015*race_B + year_2015*race_A + year_2015*race_H + year_2015*race_U +
  year_2016*race_B + year_2016*race_A + year_2016*race_H + year_2016*race_U +
  year_2017*race_B + year_2017*race_A + year_2017*race_H + year_2017*race_U +
  Female + age + I(age^2) + I(age_at_vote < 18) + 
  medhhinc_10k + college_pct + majmatch + (1 | VANID) + (1 | NYCCD)
    
lme_race_simcf <- glmer(logit_race_simcf, data = pb_long, family = binomial(), nAGQ = 0)    #start = list(fixef = bas_log$coefficients),


### Formula for simcf
logit_race_form <- turned_out ~ pb + after_pb + election_p + election_pp +
  year_2009 + year_2010 + year_2011 + year_2012 + year_2013 + year_2014 + year_2015 + year_2016 + year_2017 +
  after_pb*race_B + after_pb*race_A + after_pb*race_H + after_pb*race_U +
  year_2009*race_B + year_2009*race_A + year_2009*race_H + year_2009*race_U +
  year_2010*race_B + year_2010*race_A + year_2010*race_H + year_2010*race_U +
  year_2011*race_B + year_2011*race_A + year_2011*race_H + year_2011*race_U +
  year_2012*race_B + year_2012*race_A + year_2012*race_H + year_2012*race_U +
  year_2013*race_B + year_2013*race_A + year_2013*race_H + year_2013*race_U +
  year_2014*race_B + year_2014*race_A + year_2014*race_H + year_2014*race_U +
  year_2015*race_B + year_2015*race_A + year_2015*race_H + year_2015*race_U +
  year_2016*race_B + year_2016*race_A + year_2016*race_H + year_2016*race_U +
  year_2017*race_B + year_2017*race_A + year_2017*race_H + year_2017*race_U +
  Female + age + I(age^2) + I(age_at_vote < 18) + 
  medhhinc_10k + college_pct + majmatch


################################################################################################################################################
######################## DISAGGREGATE BY majmatch ---------------------------------------------------------------------------------------------------------
### Estimated model

logit_majmatch_simcf <- turned_out ~ pb + after_pb + election_p + election_pp +
  year_2009 + year_2010 + year_2011 + year_2012 + year_2013 + year_2014 + year_2015 + year_2016 + year_2017 +
  race_B + race_A + race_H + race_U + 
  majmatch*after_pb +
  year_2009*majmatch + year_2010*majmatch + year_2011*majmatch + year_2012*majmatch + year_2013*majmatch + year_2014*majmatch + year_2015*majmatch + year_2016*majmatch + year_2017*majmatch +
  Female + age + I(age^2) + I(age_at_vote < 18) + 
  medhhinc_10k + college_pct + (1 | VANID) + (1 | NYCCD)

lme_majmatch_simcf <- glmer(logit_majmatch_simcf, data = pb_long, family = binomial(), nAGQ = 0)    #start = list(fixef = bas_log$coefficients),

### Formula for simcf
logit_majmatch_form <-  turned_out ~ pb + after_pb + election_p + election_pp +
  year_2009 + year_2010 + year_2011 + year_2012 + year_2013 + year_2014 + year_2015 + year_2016 + year_2017 +
  race_B + race_A + race_H + race_U + 
  majmatch*after_pb +
  year_2009*majmatch + year_2010*majmatch + year_2011*majmatch + year_2012*majmatch + year_2013*majmatch + year_2014*majmatch + year_2015*majmatch + year_2016*majmatch + year_2017*majmatch +
  Female + age + I(age^2) + I(age_at_vote < 18) + 
  medhhinc_10k + college_pct


################################################################################################################################################
######################## DISAGGREGATE BY gender ---------------------------------------------------------------------------------------------------------
### Estimated model

logit_gender_simcf <- turned_out ~ pb + after_pb + election_p + election_pp +
  year_2009 + year_2010 + year_2011 + year_2012 + year_2013 + year_2014 + year_2015 + year_2016 + year_2017 +
  race_B + race_A + race_H + race_U + 
  Female*after_pb +
  year_2009*Female + year_2010*Female + year_2011*Female + year_2012*Female + year_2013*Female + year_2014*Female + year_2015*Female + year_2016*Female + year_2017*Female +
  age + I(age^2) + I(age_at_vote < 18) + 
  medhhinc_10k + college_pct + majmatch + (1 | VANID) + (1 | NYCCD)

lme_gender_simcf <- glmer(logit_gender_simcf, data = pb_long, family = binomial(), nAGQ = 0)    #start = list(fixef = bas_log$coefficients),

### Formula for simcf
logit_gender_form <-  turned_out ~ pb + after_pb + election_p + election_pp +
  year_2009 + year_2010 + year_2011 + year_2012 + year_2013 + year_2014 + year_2015 + year_2016 + year_2017 +
  race_B + race_A + race_H + race_U + 
  Female*after_pb +
  year_2009*Female + year_2010*Female + year_2011*Female + year_2012*Female + year_2013*Female + year_2014*Female + year_2015*Female + year_2016*Female + year_2017*Female +
  age + I(age^2) + I(age_at_vote < 18) + 
  medhhinc_10k + college_pct + majmatch


################################################################################################################################################
######################## DISAGGREGATE BY well educ ---------------------------------------------------------------------------------------------------------
### Estimated model
pb_long <- pb_long %>% mutate(well_educ = as.numeric(college_pct > quantile(college_pct, probs = .5)))
logit_educ_simcf <-  turned_out ~ pb + after_pb + election_p + election_pp +
  year_2009 + year_2010 + year_2011 + year_2012 + year_2013 + year_2014 + year_2015 + year_2016 + year_2017 +
  race_B + race_A + race_H + race_U + 
  Female + age + I(age^2) + I(age_at_vote < 18) + 
  medhhinc_10k + college_pct + majmatch +
  college_pct*after_pb +
  college_pct*year_2009 + college_pct*year_2010 + college_pct*year_2011 + college_pct*year_2012 + college_pct*year_2013 + college_pct*year_2014 + college_pct*year_2015 + college_pct*year_2016 + college_pct*year_2016 +
  (1 | VANID) + (1 | NYCCD)

lme_educ_simcf <- glmer(logit_educ_simcf, data = pb_long, family = binomial(), nAGQ = 0)    #start = list(fixef = bas_log$coefficients),

### Formula for simcf
logit_educ_form <-  turned_out ~ pb + after_pb + election_p + election_pp +
  year_2009 + year_2010 + year_2011 + year_2012 + year_2013 + year_2014 + year_2015 + year_2016 + year_2017 +
  race_B + race_A + race_H + race_U + 
  Female + age + I(age^2) + I(age_at_vote < 18) + 
  medhhinc_10k + college_pct + majmatch +
  college_pct*after_pb +
  college_pct*year_2009 + college_pct*year_2010 + college_pct*year_2011 + college_pct*year_2012 + college_pct*year_2013 + college_pct*year_2014 + college_pct*year_2015 + college_pct*year_2016 + college_pct*year_2016


################################################################################################################################################
######################## DISAGGREGATE BY income ---------------------------------------------------------------------------------------------------------
### Estimated model
#pb_long <- pb_long %>% mutate(income = as.numeric(medhhinc_10k > quantile(medhhinc_10k, probs = .5)))
logit_income_simcf <- turned_out ~ pb + after_pb + election_p + election_pp +
  year_2009 + year_2010 + year_2011 + year_2012 + year_2013 + year_2014 + year_2015 + year_2016 + year_2017 +
  race_B + race_A + race_H + race_U + 
  Female + age + I(age^2) + I(age_at_vote < 18) + 
  medhhinc_10k + college_pct + majmatch +
  medhhinc_10k*after_pb +
  medhhinc_10k*year_2009 + medhhinc_10k*year_2010 + medhhinc_10k*year_2011 + medhhinc_10k*year_2012 + medhhinc_10k*year_2013 + medhhinc_10k*year_2014 + medhhinc_10k*year_2015 + medhhinc_10k*year_2016 + medhhinc_10k*year_2016  + 
  (1 | VANID) + (1 | NYCCD)
lme_income_simcf <- glmer(logit_income_simcf, data = pb_long, family = binomial(), nAGQ = 0)    #start = list(fixef = bas_log$coefficients),

### Formula for simcf
logit_income_form <-  turned_out ~ pb + after_pb + election_p + election_pp +
  year_2009 + year_2010 + year_2011 + year_2012 + year_2013 + year_2014 + year_2015 + year_2016 + year_2017 +
  race_B + race_A + race_H + race_U + 
  Female + age + I(age^2) + I(age_at_vote < 18) + 
  medhhinc_10k + college_pct + majmatch +
  medhhinc_10k*after_pb +
  medhhinc_10k*year_2009 + medhhinc_10k*year_2010 + medhhinc_10k*year_2011 + medhhinc_10k*year_2012 + medhhinc_10k*year_2013 + medhhinc_10k*year_2014 + medhhinc_10k*year_2015 + medhhinc_10k*year_2016 + medhhinc_10k*year_2016  


################################################################################################################################################
######################## DISAGGREGATE BY Youth ---------------------
### Estimated model
pb_long <- pb_long %>% mutate(youth = ifelse(age <= 29, 1, 0))
logit_youth_simcf <- turned_out ~ pb + after_pb*youth + race_B + race_A + race_H + race_U +
  year_2009*youth + year_2010*youth + year_2011*youth + year_2012*youth + year_2013*youth + year_2014*youth + year_2015*youth + year_2016*youth + year_2017*youth +
  election_p + election_pp + Female + age + I(age^2) + I(age_at_vote < 18) + medhhinc_10k + college_pct + majmatch  + (1 | VANID) + (1 | NYCCD)
lme_youth_simcf <- glmer(logit_youth_simcf, data = pb_long, family = binomial(), nAGQ = 0)    #start = list(fixef = bas_log$coefficients),

### Formula for simcf
logit_youth_form <-  turned_out ~ pb + after_pb*youth + race_B + race_A + race_H + race_U +
  year_2009*youth + year_2010*youth + year_2011*youth + year_2012*youth + year_2013*youth + year_2014*youth + year_2015*youth + year_2016*youth + year_2017*youth +
  election_p + election_pp + Female + age + I(age^2) + I(age_at_vote < 18) + medhhinc_10k + college_pct + majmatch


######################## DISAGGREGATE BY Youth - ONLY 18+ ---------------------
# See scratchpad - reran these models for only elections 2011 on, this made the differential effect for youth disappear.  Suspicion that the fact that youth didn't vote in 2008 may be driving the apparent greater impact of PB
### Estimated model
pb_long_18p <- pb_long %>% mutate(youth = ifelse(age <= 29, 1, 0)) %>%  filter(age_at_vote >= 18)
logit_youth18p_simcf <- turned_out ~ pb + after_pb*youth + race_B + race_A + race_H + race_U +
  year_2009*youth + year_2010*youth + year_2011*youth + year_2012*youth + year_2013*youth + year_2014*youth + year_2015*youth + year_2016*youth + year_2017*youth +
  election_p + election_pp + age + medhhinc_10k + white + majmatch  + (1 | VANID) + (1 | NYCCD)
lme_youth18p_simcf <- glmer(logit_youth18p_simcf, data = pb_long_18p, family = binomial(), nAGQ = 0)    #start = list(fixef = bas_log$coefficients), 

### Formula for simcf
logit_youth18p_form <-   turned_out ~ pb + after_pb*youth + race_B + race_A + race_H + race_U +
  year_2009*youth + year_2010*youth + year_2011*youth + year_2012*youth + year_2013*youth + year_2014*youth + year_2015*youth + year_2016*youth + year_2017*youth +
  election_p + election_pp + age + medhhinc_10k + white + majmatch


################################################################################################################################################
######################## SIMCF preds for 2016 ---------------------------------------------------------------------------------------------------------

### Race in 2016 ------------------------------------------------------------------------------------------------------
sims <- 10000
pe <- fixef(lme_race_simcf)
vc <- vcov(lme_race_simcf) 
simbetas <- mvrnorm(sims, pe, vc)

nscen_race <- nscen <- length(unique(pb_long$Race))*2
xhyp <- cfMake(logit_race_form, pb_long, nscen = nscen, f = "min")

simyear <- "2016"
for (i in 1:nscen){
  xhyp <- cfChange(xhyp, "age", x = mean(pb_long$age), xpre = mean(pb_long$age), scen = i)
  xhyp <- cfChange(xhyp, "medhhinc_10k", x = mean(pb_long$medhhinc_10k), xpre = mean(pb_long$medhhinc_10k), scen = i)
  xhyp <- cfChange(xhyp, "white", x = mean(pb_long$white[pb_long$Race == "W"]), xpre = mean(pb_long$white[pb_long$Race == "W"]), scen = i)
  xhyp <- cfChange(xhyp, "majmatch", x = mean(pb_long$majmatch[pb_long$Race == "W"]), xpre = mean(pb_long$majmatch[pb_long$Race == "W"]), scen = i)
  xhyp <- cfChange(xhyp, paste0("year_", simyear), x = 1, xpre = 1, scen = i)
  xhyp <- cfChange(xhyp, "age_at_vote", x = mean(pb_long$age_at_vote), xpre = mean(pb_long$age_at_vote), scen = i)
  xhyp <- cfChange(xhyp, "Female", x = mean(pb_long$Female), xpre = mean(pb_long$Female), scen = i)
  xhyp <- cfChange(xhyp, "college_pct", x = mean(pb_long$college_pct), xpre = mean(pb_long$college_pct), scen = i)
  xhyp <- cfChange(xhyp, "pb", x = 0, xpre = 0, scen = i)
}
for (i in (nscen/2+1):nscen){
  xhyp <- cfChange(xhyp, "after_pb", x = 1, xpre = 0, scen = i)
}
for (i in 1:(nscen/2)){
  xhyp <- cfChange(xhyp, "after_pb", x = 0, xpre = 0, scen = i)
}

for (i in c(2,7)){
  xhyp <- cfChange(xhyp, "race_B", x = 1, xpre = 1, scen = i)
  xhyp <- cfChange(xhyp, "majmatch", x = mean(pb_long$majmatch[pb_long$Race == "B"]), xpre =  mean(pb_long$majmatch[pb_long$Race == "B"]), scen = i)
  xhyp <- cfChange(xhyp, "white", x = mean(pb_long$white[pb_long$Race == "B"]),  mean(pb_long$majmatch[pb_long$Race == "B"]), scen = i)
}
for (i in c(3,8)){
  xhyp <- cfChange(xhyp, "race_H", x = 1, xpre = 1, scen = i)
  xhyp <- cfChange(xhyp, "majmatch", x = mean(pb_long$majmatch[pb_long$Race == "H"]), xpre =  mean(pb_long$majmatch[pb_long$Race == "H"]), scen = i)
  xhyp <- cfChange(xhyp, "white", x = mean(pb_long$white[pb_long$Race == "H"]),  mean(pb_long$majmatch[pb_long$Race == "H"]), scen = i)
}
for (i in c(4,9)){
  xhyp <- cfChange(xhyp, "race_A", x = 1, xpre = 1, scen = i)
  xhyp <- cfChange(xhyp, "majmatch", x = mean(pb_long$majmatch[pb_long$Race == "A"]), xpre =  mean(pb_long$majmatch[pb_long$Race == "A"]), scen = i)
  xhyp <- cfChange(xhyp, "white", x = mean(pb_long$white[pb_long$Race == "A"]),  mean(pb_long$majmatch[pb_long$Race == "A"]), scen = i)
}
for (i in c(5,10)){
  xhyp <- cfChange(xhyp, "race_U", x = 1, xpre = 1, scen = i)
  xhyp <- cfChange(xhyp, "majmatch", x = mean(pb_long$majmatch[pb_long$Race == "U"]), xpre =  mean(pb_long$majmatch[pb_long$Race == "U"]), scen = i)
  xhyp <- cfChange(xhyp, "white", x = mean(pb_long$white[pb_long$Race == "U"]),  mean(pb_long$majmatch[pb_long$Race == "U"]), scen = i)
}

xhyp_race <- xhyp
rm(xhyp)

yhyp_race <- logitsimev(xhyp_race, simbetas)

yhyp_fd_race <- logitsimfd(xhyp_race, simbetas)

preds_race <-  cbind(xhyp_race$x, as.data.frame(yhyp_race)) %>% 
  mutate(Race = rep(c("W", "B", "H", "A", "U"), 2)) 

preds_fd_race <-  cbind(xhyp_race$x[6:10,], as.data.frame(yhyp_fd_race)[6:10,]) %>% 
  mutate(Race = rep(c("W", "B", "H", "A", "U"), 1)) 

preds_race %>% dplyr::select(after_pb, Race, pe, lower, upper) %>% 
  mutate(after_pb = factor(after_pb, levels = c(0,1), labels = c("No PB", "After PB")),
         Race = factor(Race, levels = c("W", "B", "H", "A", "U"), labels = c("White", "Black", "Hispanic", "Asian", "Unknown"))) %>% 
  ggplot(aes(y = pe, ymin = lower, ymax = upper, x = Race)) + 
  # geom_segment(aes(xend = Race, y = 0, yend = preds, color = as.factor(after_pb)))+
  geom_pointrange(aes(color = as.factor(after_pb))) +
  labs(title = paste0("By race: redicted probability of voting in general election in ", simyear, "\n before and after PB (in a non-PB district)"), 
       x = "", y = "Predicted probability of Voting", color = "") +
  coord_flip() +
  theme_minimal()
 ggsave("Paper/Figs/byrace.pdf", width = 5, height = 4)

# #race ropeladder 
# trace_race <- ropeladder(x = preds_fd$pe,
#                     lower = preds_fd$lower,
#                     upper = preds_fd$upper,
#                     plot = 1)
# trace_race$sublabels = preds_fd$R
# tile(trace_race, trace_youth,
#      RxC = c(1,2))

preds_fd_race %>% 
  dplyr::select(after_pb, Race, pe, lower, upper) %>% 
  mutate(after_pb = factor(after_pb, levels = c(0,1), labels = c("No PB", "After PB")),
         Race = factor(Race, levels = c("W", "B", "H", "A", "U"), labels = c("White", "Black", "Hispanic", "Asian", "Unknown"))) %>% 
  ggplot(aes(y = pe, ymin = lower, ymax = upper, x = Race)) + 
  # geom_segment(aes(xend = Race, y = 0, yend = preds, color = as.factor(after_pb)))+
  geom_pointrange() +
  labs(title = paste0("Change in predicted probability of voting in general election in ", simyear, "\n before and after PB (in a non-PB district)"), 
       x = "", y = "Change in predicted probability of voting", color = "") +
  ylim(0,.3) +
  coord_flip() +
  theme_minimal()
  # ggsave("Paper/Figs/fd_byrace.pdf", width = 5, height = 4)

### Majmatch in 2016 ------------------------------------------------------------------------------------------------------
pe <- fixef(lme_majmatch_simcf)
vc <- vcov(lme_majmatch_simcf) 
simbetas <- mvrnorm(sims, pe, vc)

nscen <- length(unique(pb_long$majmatch))*2
xhyp <- cfMake(logit_majmatch_form, pb_long, nscen = nscen, f = "mean")

simyear <- "2016"

for (i in 1:nscen){
  xhyp <- cfChange(xhyp, "year_2008", x = 0, xpre = 0, scen = i)
  xhyp <- cfChange(xhyp, "year_2009", x = 0, xpre = 0, scen = i)
  xhyp <- cfChange(xhyp, "year_2010", x = 0, xpre = 0, scen = i)
  xhyp <- cfChange(xhyp, "year_2011", x = 0, xpre = 0, scen = i)
  xhyp <- cfChange(xhyp, "year_2012", x = 0, xpre = 0, scen = i)
  xhyp <- cfChange(xhyp, "year_2013", x = 0, xpre = 0, scen = i)
  xhyp <- cfChange(xhyp, "year_2014", x = 0, xpre = 0, scen = i)
  xhyp <- cfChange(xhyp, "year_2015", x = 0, xpre = 0, scen = i)
  xhyp <- cfChange(xhyp, "year_2016", x = 0, xpre = 0, scen = i)
  xhyp <- cfChange(xhyp, "year_2017", x = 0, xpre = 0, scen = i)
  xhyp <- cfChange(xhyp, paste0("year_", simyear), x = 1, xpre = 1, scen = i)
  xhyp <- cfChange(xhyp, "pb", x = 0, xpre = 0, scen = i)
  xhyp <- cfChange(xhyp, "election_p", x = 0, xpre = 0, scen = i)
  xhyp <- cfChange(xhyp, "election_pp", x = 0, xpre = 0, scen = i)
}

for (i in (nscen/2+1):nscen){
  xhyp <- cfChange(xhyp, "after_pb", x = 1, xpre = 0, scen = i)
}
for (i in 1:(nscen/2)){
  xhyp <- cfChange(xhyp, "after_pb", x = 0, xpre = 0, scen = i)
}

for (i in c(1,3)){
  xhyp <- cfChange(xhyp, "majmatch", x = 1, xpre = 1, scen = i)
}
for (i in c(2,4)){
  xhyp <- cfChange(xhyp, "majmatch", x = 0, xpre = 0, scen = i)
}

xhyp_majmatch <- xhyp
rm(xhyp)

yhyp_majmatch <- logitsimev(xhyp_majmatch, simbetas)

yhyp_fd_majmatch <- logitsimfd(xhyp_majmatch, simbetas)

preds_majmatch <-  cbind(xhyp_majmatch$x, as.data.frame(yhyp_majmatch))

preds_fd_majmatch <- cbind(xhyp_majmatch$x[3:4,], as.data.frame(yhyp_fd_majmatch)[3:4,] )

preds_majmatch %>% dplyr::select(after_pb, majmatch, pe, lower, upper) %>% 
  mutate(after_pb = factor(after_pb, levels = c(0,1), labels = c("No PB", "After PB")),
         majmatch = factor(majmatch, levels = c(0,1), labels = c("Not majority race ID", "Majority race ID"))) %>% 
  ggplot(aes(y = pe, ymin = lower, ymax = upper, x = majmatch)) + 
  # geom_segment(aes(xend = Race, y = 0, yend = preds, color = as.factor(after_pb)))+
  geom_pointrange(aes(color = as.factor(after_pb))) +
  labs(title = paste0("By majority race alignment: predicted probability of voting in general election in ", simyear, "\n before and after PB (in a non-PB district)"), 
       x = "", y = "Predicted probability of Voting", color = "") +
  coord_flip() +
  theme_minimal()
ggsave("Paper/Figs/bymajmatch.pdf", width = 5, height = 4)


### Gender in 2016 ---------------------------------------------------------------------------------------------
pe <- fixef(lme_gender_simcf)
vc <- vcov(lme_gender_simcf) 
simbetas <- mvrnorm(sims, pe, vc)

nscen <- length(unique(pb_long$Female))*2
xhyp <- cfMake(logit_gender_form, pb_long, nscen = nscen, f = "mean")

simyear <- "2016"

for (i in 1:nscen){
  xhyp <- cfChange(xhyp, "year_2008", x = 0, xpre = 0, scen = i)
  xhyp <- cfChange(xhyp, "year_2009", x = 0, xpre = 0, scen = i)
  xhyp <- cfChange(xhyp, "year_2010", x = 0, xpre = 0, scen = i)
  xhyp <- cfChange(xhyp, "year_2011", x = 0, xpre = 0, scen = i)
  xhyp <- cfChange(xhyp, "year_2012", x = 0, xpre = 0, scen = i)
  xhyp <- cfChange(xhyp, "year_2013", x = 0, xpre = 0, scen = i)
  xhyp <- cfChange(xhyp, "year_2014", x = 0, xpre = 0, scen = i)
  xhyp <- cfChange(xhyp, "year_2015", x = 0, xpre = 0, scen = i)
  xhyp <- cfChange(xhyp, "year_2016", x = 0, xpre = 0, scen = i)
  xhyp <- cfChange(xhyp, "year_2017", x = 0, xpre = 0, scen = i)
  xhyp <- cfChange(xhyp, paste0("year_", simyear), x = 1, xpre = 1, scen = i)
  xhyp <- cfChange(xhyp, "pb", x = 0, xpre = 0, scen = i)
  xhyp <- cfChange(xhyp, "election_p", x = 0, xpre = 0, scen = i)
  xhyp <- cfChange(xhyp, "election_pp", x = 0, xpre = 0, scen = i)
}

for (i in (nscen/2+1):nscen){
  xhyp <- cfChange(xhyp, "after_pb", x = 1, xpre = 0, scen = i)
}
for (i in 1:(nscen/2)){
  xhyp <- cfChange(xhyp, "after_pb", x = 0, xpre = 0, scen = i)
}

for (i in c(1,3)){
  xhyp <- cfChange(xhyp, "Female", x = 1, xpre = 1, scen = i)
}
for (i in c(2,4)){
  xhyp <- cfChange(xhyp, "Female", x = 0, xpre = 0, scen = i)
}

xhyp_gender <- xhyp
rm(xhyp)

yhyp_gender <- logitsimev(xhyp_gender, simbetas)

yhyp_fd_gender <- logitsimfd(xhyp_gender, simbetas)

preds_gender <-  cbind(xhyp_gender$x, as.data.frame(yhyp_gender))

preds_fd_gender <- cbind(xhyp_gender$x[3:4,], as.data.frame(yhyp_fd_gender)[3:4,] )

preds_gender %>% dplyr::select(after_pb, Female, pe, lower, upper) %>% 
  mutate(after_pb = factor(after_pb, levels = c(0,1), labels = c("No PB", "After PB")),
         youth = factor(Female, levels = c(0,1), labels = c("Male", "Female"))) %>% 
  ggplot(aes(y = pe, ymin = lower, ymax = upper, x = youth)) + 
  # geom_segment(aes(xend = Race, y = 0, yend = preds, color = as.factor(after_pb)))+
  geom_pointrange(aes(color = as.factor(after_pb))) +
  labs(title = paste0("By gender: predicted probability of voting in general election in ", simyear, "\n before and after PB (in a non-PB district)"), 
       x = "", y = "Predicted probability of Voting", color = "") +
  coord_flip() +
  theme_minimal()
ggsave("Paper/Figs/bygender.pdf", width = 5, height = 4)


### Educ in 2016 --------------------------------------------------------------------------------------------------
educ_levels <- quantile(pb_long$college_pct, probs = c(.25,.75))
names(educ_levels) <- c("low", "high")

sims <- 10000
pe <- fixef(lme_educ_simcf)
vc <- vcov(lme_educ_simcf) 
simbetas <- mvrnorm(sims, pe, vc)

nscen <- length(educ_levels)*2
xhyp <- cfMake(logit_educ_form, pb_long, nscen = nscen, f = "mean")

simyear <- "2016"

for (i in 1:nscen){
  xhyp <- cfChange(xhyp, "year_2008", x = 0, xpre = 0, scen = i)
  xhyp <- cfChange(xhyp, "year_2009", x = 0, xpre = 0, scen = i)
  xhyp <- cfChange(xhyp, "year_2010", x = 0, xpre = 0, scen = i)
  xhyp <- cfChange(xhyp, "year_2011", x = 0, xpre = 0, scen = i)
  xhyp <- cfChange(xhyp, "year_2012", x = 0, xpre = 0, scen = i)
  xhyp <- cfChange(xhyp, "year_2013", x = 0, xpre = 0, scen = i)
  xhyp <- cfChange(xhyp, "year_2014", x = 0, xpre = 0, scen = i)
  xhyp <- cfChange(xhyp, "year_2015", x = 0, xpre = 0, scen = i)
  xhyp <- cfChange(xhyp, "year_2016", x = 0, xpre = 0, scen = i)
  xhyp <- cfChange(xhyp, "year_2017", x = 0, xpre = 0, scen = i)
  xhyp <- cfChange(xhyp, paste0("year_", simyear), x = 1, xpre = 1, scen = i)
  xhyp <- cfChange(xhyp, "pb", x = 0, xpre = 0, scen = i)
  xhyp <- cfChange(xhyp, "election_pp", x = 0, xpre = 0, scen = i)
  xhyp <- cfChange(xhyp, "election_p", x = 0, xpre = 0, scen = i)
}

for (i in (nscen/2+1):nscen){
  xhyp <- cfChange(xhyp, "after_pb", x = 1, xpre = 0, scen = i)
}
for (i in 1:(nscen/2)){
  xhyp <- cfChange(xhyp, "after_pb", x = 0, xpre = 0, scen = i)
}

for (i in c(1,3)){
  xhyp <- cfChange(xhyp, "college_pct", x = educ_levels["high"], xpre = educ_levels["high"], scen = i)
}
for (i in c(2,4)){
  xhyp <- cfChange(xhyp, "college_pct", x = educ_levels["low"], xpre = educ_levels["low"], scen = i)
}

xhyp_educ <- xhyp
rm(xhyp)

yhyp_educ <- logitsimev(xhyp_educ, simbetas)

yhyp_fd_educ <- logitsimfd(xhyp_educ, simbetas)

preds_educ <-  cbind(xhyp_educ$x, as.data.frame(yhyp_educ))
preds_fd_educ <- cbind(xhyp_educ$x[3:4,], as.data.frame(yhyp_fd_educ)[3:4,])

preds_educ %>% dplyr::select(after_pb, college_pct, pe, lower, upper) %>% 
  mutate(after_pb = factor(after_pb, levels = c(0,1), labels = c("No PB", "After PB")),
         college_pct = factor(college_pct, levels = educ_levels, labels = c("Lower education area", "Higher education area"))) %>% 
  ggplot(aes(y = pe, ymin = lower, ymax = upper, x = college_pct)) + 
  # geom_segment(aes(xend = Race, y = 0, yend = preds, color = as.factor(after_pb)))+
  geom_pointrange(aes(color = as.factor(after_pb))) +
  labs(title = paste0("By education: predicted probability of voting in general election in ", simyear, "\n before and after PB (in a non-PB district)"), 
       x = "", y = "Predicted probability of Voting", color = "") +
  coord_flip() +
  theme_minimal()
ggsave("Paper/Figs/byeduc.pdf", width = 5, height = 4)


### Income in 2016 --------------------------------------------------------------------------------------------
income_levels <- quantile(pb_long$medhhinc_10k, probs = c(.25, .75))
names(income_levels) <- c("lower income", "higher income")

sims <- 10000
pe <- fixef(lme_income_simcf)
vc <- vcov(lme_income_simcf) 
simbetas <- mvrnorm(sims, pe, vc)

nscen <- length(income_levels)*2
xhyp <- cfMake(logit_income_form, pb_long, nscen = nscen, f = "mean")

simyear <- "2016"

for (i in 1:nscen){
  xhyp <- cfChange(xhyp, "year_2008", x = 0, xpre = 0, scen = i)
  xhyp <- cfChange(xhyp, "year_2009", x = 0, xpre = 0, scen = i)
  xhyp <- cfChange(xhyp, "year_2010", x = 0, xpre = 0, scen = i)
  xhyp <- cfChange(xhyp, "year_2011", x = 0, xpre = 0, scen = i)
  xhyp <- cfChange(xhyp, "year_2012", x = 0, xpre = 0, scen = i)
  xhyp <- cfChange(xhyp, "year_2013", x = 0, xpre = 0, scen = i)
  xhyp <- cfChange(xhyp, "year_2014", x = 0, xpre = 0, scen = i)
  xhyp <- cfChange(xhyp, "year_2015", x = 0, xpre = 0, scen = i)
  xhyp <- cfChange(xhyp, "year_2016", x = 0, xpre = 0, scen = i)
  xhyp <- cfChange(xhyp, "year_2017", x = 0, xpre = 0, scen = i)
  xhyp <- cfChange(xhyp, paste0("year_", simyear), x = 1, xpre = 1, scen = i)
  xhyp <- cfChange(xhyp, "pb", x = 0, xpre = 0, scen = i)
  xhyp <- cfChange(xhyp, "election_pp", x = 0, xpre = 0, scen = i)
  xhyp <- cfChange(xhyp, "election_p", x = 0, xpre = 0, scen = i)
}

for (i in (nscen/2+1):nscen){
  xhyp <- cfChange(xhyp, "after_pb", x = 1, xpre = 0, scen = i)
}
for (i in 1:(nscen/2)){
  xhyp <- cfChange(xhyp, "after_pb", x = 0, xpre = 0, scen = i)
}

for (i in c(1,3)){
  xhyp <- cfChange(xhyp, "medhhinc_10k", x = income_levels["higher income"], xpre = income_levels["higher income"], scen = i)
}
for (i in c(2,4)){
  xhyp <- cfChange(xhyp, "medhhinc_10k", x = income_levels["lower income"], xpre = income_levels["lower income"], scen = i)
}

xhyp_income <- xhyp
rm(xhyp)

yhyp_income <- logitsimev(xhyp_income, simbetas)

yhyp_fd_income <- logitsimfd(xhyp_income, simbetas)

preds_income <-  cbind(xhyp_income$x, as.data.frame(yhyp_income))
preds_fd_income <- cbind(xhyp_income$x[3:4,], as.data.frame(yhyp_fd_income)[3:4,])

preds_income %>% dplyr::select(after_pb, medhhinc_10k, pe, lower, upper) %>% 
  mutate(after_pb = factor(after_pb, levels = c(0,1), labels = c("No PB", "After PB")),
         medhhinc_10k = factor(medhhinc_10k, levels = income_levels, labels = c("Lower income area", "Higher income area"))) %>% 
  ggplot(aes(y = pe, ymin = lower, ymax = upper, x = medhhinc_10k)) + 
  # geom_segment(aes(xend = Race, y = 0, yend = preds, color = as.factor(after_pb)))+
  geom_pointrange(aes(color = as.factor(after_pb))) +
  labs(title = paste0("By income: predicted probability of voting in general election in ", simyear, "\n before and after PB (in a non-PB district)"), 
       x = "", y = "Predicted probability of Voting", color = "") +
  coord_flip() +
  theme_minimal()
ggsave("Paper/Figs/byincome.pdf", width = 5, height = 4)

### Youth in 2016 ------------------------------------------------------------------------------------------------
sims <- 10000
pe <- fixef(lme_youth_simcf)
vc <- vcov(lme_youth_simcf) 
simbetas <- mvrnorm(sims, pe, vc)

nscen <- length(unique(pb_long$youth))*2
xhyp <- cfMake(logit_youth_form, pb_long, nscen = nscen, f = "mean")

simyear <- "2016"

for (i in 1:nscen){
  xhyp <- cfChange(xhyp, "year_2008", x = 0, xpre = 0, scen = i)
  xhyp <- cfChange(xhyp, "year_2009", x = 0, xpre = 0, scen = i)
  xhyp <- cfChange(xhyp, "year_2010", x = 0, xpre = 0, scen = i)
  xhyp <- cfChange(xhyp, "year_2011", x = 0, xpre = 0, scen = i)
  xhyp <- cfChange(xhyp, "year_2012", x = 0, xpre = 0, scen = i)
  xhyp <- cfChange(xhyp, "year_2013", x = 0, xpre = 0, scen = i)
  xhyp <- cfChange(xhyp, "year_2014", x = 0, xpre = 0, scen = i)
  xhyp <- cfChange(xhyp, "year_2015", x = 0, xpre = 0, scen = i)
  xhyp <- cfChange(xhyp, "year_2016", x = 0, xpre = 0, scen = i)
  xhyp <- cfChange(xhyp, "year_2017", x = 0, xpre = 0, scen = i)
  xhyp <- cfChange(xhyp, paste0("year_", simyear), x = 1, xpre = 1, scen = i)
  xhyp <- cfChange(xhyp, "pb", x = 0, xpre = 0, scen = i)
  xhyp <- cfChange(xhyp, "election_pp", x = 0, xpre = 0, scen = i)
  xhyp <- cfChange(xhyp, "election_p", x = 0, xpre = 0, scen = i)
}

for (i in (nscen/2+1):nscen){
  xhyp <- cfChange(xhyp, "after_pb", x = 1, xpre = 0, scen = i)
}
for (i in 1:(nscen/2)){
  xhyp <- cfChange(xhyp, "after_pb", x = 0, xpre = 0, scen = i)
}

for (i in c(1,3)){
  xhyp <- cfChange(xhyp, "youth", x = 1, xpre = 1, scen = i)
  xhyp <- cfChange(xhyp, "age", x = 22, xpre = 22, scen = i)
  xhyp <- cfChange(xhyp, "age_at_vote", x = 20, xpre = 20, scen = i)
}
for (i in c(2,4)){
  xhyp <- cfChange(xhyp, "youth", x = 0, xpre = 0, scen = i)
  xhyp <- cfChange(xhyp, "age", x = mean(pb_long$age[pb_long$youth == 0]), xpre = mean(pb_long$age[pb_long$youth == 0]), scen = i)
  xhyp <- cfChange(xhyp, "age_at_vote", x = mean(pb_long$age[pb_long$youth == 0])-2, xpre = mean(pb_long$age[pb_long$youth == 0])-2, scen = i)
}

xhyp_youth <- xhyp
rm(xhyp)

yhyp_youth <- logitsimev(xhyp_youth, simbetas)

yhyp_fd_youth <- logitsimfd(xhyp_youth, simbetas)

preds_youth <-  cbind(xhyp_youth$x, as.data.frame(yhyp_youth))

preds_fd_youth <- cbind(xhyp_youth$x[3:4,], as.data.frame(yhyp_fd_youth)[3:4,])

# 
# 
# trace_youth <- ropeladder(x = preds_fd$pe,
#                           lower = preds_fd$lower,
#                           upper = preds_fd$upper,
#                           col = "red",
#                           plot = 1)
# tile( trace_youth, limits =c(-1,1,0,10))
# 
# preds_fd_all <- bind_rows(preds_fd[1:5,], preds_fd_youth)
# preds_fd_all$names <- c(preds_fd$Race[1:5], preds_fd_youth$youth)
# preds_fd_all$variable <- c(rep("Race", 5), rep("Youth",2))
# preds_fd_all %>% ggplot(aes(x = names, y = pe, ymin = lower, ymax = upper, color = variable)) +
#   geom_pointrange() + coord_flip()

preds_youth %>% dplyr::select(after_pb, youth, pe, lower, upper) %>% 
  mutate(after_pb = factor(after_pb, levels = c(0,1), labels = c("No PB", "After PB")),
         youth = factor(youth, levels = c(1, 0), labels = c("29 and under", "Older than 29"))) %>% 
  ggplot(aes(y = pe, ymin = lower, ymax = upper, x = youth)) + 
  # geom_segment(aes(xend = Race, y = 0, yend = preds, color = as.factor(after_pb)))+
  geom_pointrange(aes(color = as.factor(after_pb))) +
  labs(title = paste0("By age: predicted probability of voting in general election in ", simyear, "\n before and after PB (in a non-PB district)"), 
       x = "", y = "Predicted probability of Voting", color = "") +
  coord_flip() +
  theme_minimal()
ggsave("Paper/Figs/byyouth.pdf", width = 5, height = 4)

### Youth18+ in 2016 -------------------------------------------------------------------------------------------
sims <- 10000
pe <- fixef(lme_youth18p_simcf)
vc <- vcov(lme_youth18p_simcf) 
simbetas <- mvrnorm(sims, pe, vc)

nscen <- length(unique(pb_long_18p$youth))*2
xhyp <- cfMake(logit_youth18p_form, pb_long_18p, nscen = nscen, f = "mean")

simyear <- "2016"

for (i in 1:nscen){
  xhyp <- cfChange(xhyp, "year_2009", x = 0, xpre = 0, scen = i)
  xhyp <- cfChange(xhyp, "year_2010", x = 0, xpre = 0, scen = i)
  xhyp <- cfChange(xhyp, "year_2011", x = 0, xpre = 0, scen = i)
  xhyp <- cfChange(xhyp, "year_2012", x = 0, xpre = 0, scen = i)
  xhyp <- cfChange(xhyp, "year_2013", x = 0, xpre = 0, scen = i)
  xhyp <- cfChange(xhyp, "year_2014", x = 0, xpre = 0, scen = i)
  xhyp <- cfChange(xhyp, "year_2015", x = 0, xpre = 0, scen = i)
  xhyp <- cfChange(xhyp, "year_2016", x = 0, xpre = 0, scen = i)
  xhyp <- cfChange(xhyp, "year_2017", x = 0, xpre = 0, scen = i)
  xhyp <- cfChange(xhyp, paste0("year_", simyear), x = 1, xpre = 1, scen = i)
  xhyp <- cfChange(xhyp, "pb", x = 0, xpre = 0, scen = i)
}

for (i in (nscen/2+1):nscen){
  xhyp <- cfChange(xhyp, "after_pb", x = 1, xpre = 0, scen = i)
}
for (i in 1:(nscen/2)){
  xhyp <- cfChange(xhyp, "after_pb", x = 0, xpre = 0, scen = i)
}

for (i in c(1,3)){
  xhyp <- cfChange(xhyp, "youth", x = 1, xpre = 1, scen = i)
}
for (i in c(2,4)){
  xhyp <- cfChange(xhyp, "youth", x = 0, xpre = 0, scen = i)
  xhyp <- cfChange(xhyp, "age", x = mean(pb_long$age_at_vote[pb_long$youth == 0]), xpre = mean(pb_long$age_at_vote[pb_long$youth == 0]), scen = i)
}

yhyp <- logitsimev(xhyp, simbetas)

yhyp_fd <- logitsimfd(xhyp, simbetas)

preds_youth18p <-  cbind(xhyp$x, as.data.frame(yhyp))
preds_fd_youth18p <-  cbind(xhyp$x[3:4,], as.data.frame(yhyp_fd)[3:4,])

preds_youth18p %>% dplyr::select(after_pb, youth, pe, lower, upper) %>% 
  mutate(after_pb = factor(after_pb, levels = c(0,1), labels = c("No PB", "After PB")),
         youth = factor(youth, levels = c(1, 0), labels = c("Under 30", "30 and above"))) %>% 
  ggplot(aes(y = pe, ymin = lower, ymax = upper, x = youth)) + 
  # geom_segment(aes(xend = Race, y = 0, yend = preds, color = as.factor(after_pb)))+
  geom_pointrange(aes(color = as.factor(after_pb))) +
  labs(title = paste0("By age: predicted probability of voting in general election in ", simyear, "\n before and after PB (in a non-PB district)"), 
       x = "", y = "Predicted probability of Voting", color = "") +
  coord_flip() +
  theme_minimal()
ggsave("Paper/Figs/byyouth_only18+.pdf", width = 5, height = 4)

### 2016 Ropeladder all  ------------------------------------------------------------------
# 
# trace_race <- ropeladder(x = preds_fd_race$pe,
#                          lower = preds_fd_race$lower,
#                          upper = preds_fd_race$upper,
#                          labels = c("White", "Black", "Hispanic", "Asian", "Unknown"),
#                          plot = 1)
# 
# trace_gender <- ropeladder(x = preds_fd_gender$pe,
#                            lower = preds_fd_gender$lower,
#                            upper = preds_fd_gender$upper,
#                            labels = c("Male", "Female"),
#                            col = "red",
#                            plot = 2)
#   
# trace_incomey <- ropeladder(x = preds_fd_incomey$pe,
#                          lower = preds_fd_incomey$lower,
#                          upper = preds_fd_incomey$upper,
#                          labels = c("Higher income area", "Lower income area"),
#                          col = "blue",
#                          shadowrow = TRUE,
#                          plot = 3)
# 
# tile(trace_race, trace_gender, trace_incomey, RxC = c(3,1))

#### trying with ggplot
preds_fd_race$group <- "Race"
preds_fd_race$level <- c("White", "Black", "Hispanic", "Asian", "Unknown")

preds_fd_gender$group <- "Gender"
preds_fd_gender$level <- c("Male", "Female")

preds_fd_income$group <- "Income"
preds_fd_income$level <- c("Higher income area", "Lower income area")

preds_fd_educ$group <- "Education"
preds_fd_educ$level <- c("More college degrees", "Fewer college degrees")


preds_fd_youth$group <- "Youth"
preds_fd_youth$level <- c("Under 30", "30 and older")

preds_fd_majmatch$group <- "Majority Race"
preds_fd_majmatch$level <- c("Majority race", "Not Majority race")



preds_fd_plot <- bind_rows(preds_fd_race, preds_fd_gender, preds_fd_income, preds_fd_youth, preds_fd_educ, preds_fd_majmatch)

ggplot(preds_fd_plot, aes(x = group, y = pe, ymin = lower, ymax =upper, color = group, group = level)) +
  geom_pointrange(position = position_dodge(width = .6)) +
  geom_text(aes(y = -0.0025, label = level), hjust = 1, position = position_dodge(width = .6), size = 3) +
  geom_hline(aes(yintercept = 0)) +
  labs(x = "", y = "Change in predicted probability of voting") +
  ylim(-.075,.32) +
  coord_flip() +
  theme_minimal() +
  theme(legend.position = "none", 
        axis.title = element_text(size = 11))
ggsave("Paper/Figs/group_fds.pdf", width = 6.5, height = 5.5)

### Predictions for 2017 --------------------------------------------------------------------------------------------------------

xhyp_update <- function(cf, simyear, oldsimyear){
  for (i in 1:nrow(cf$x)){
    cf <- cfChange(cf, paste0("year_", simyear), x = 1, xpre = 1, scen = i)
    cf <- cfChange(cf, paste0("year_", oldsimyear), x = 0, xpre = 0, scen = i)
  }
  cf
}

oldsimyear <- "2016"
simyear <- "2017"
sims <- 10000

## Race
pe <- fixef(lme_race_simcf)
vc <- vcov(lme_race_simcf) 
simbetas <- mvrnorm(sims, pe, vc)

xhyp_race <- xhyp_update(xhyp_race, simyear, oldsimyear)

yhyp_race <- logitsimev(xhyp_race, simbetas)
yhyp_fd_race <- logitsimfd(xhyp_race, simbetas)

preds_fd_race <-  cbind(xhyp_race$x[6:10,], as.data.frame(yhyp_fd_race)[6:10,]) %>% 
  mutate(Race = rep(c("W", "B", "H", "A", "U"), 1)) 

## Majority match

pe <- fixef(lme_majmatch_simcf)
vc <- vcov(lme_majmatch_simcf) 
simbetas <- mvrnorm(sims, pe, vc)

xhyp_majmatch <- xhyp_update(xhyp_majmatch, simyear, oldsimyear)

yhyp_majmatch <- logitsimev(xhyp_majmatch, simbetas)
yhyp_fd_majmatch <- logitsimfd(xhyp_majmatch, simbetas)

preds_majmatch <-  cbind(xhyp_majmatch$x, as.data.frame(yhyp_majmatch))
preds_fd_majmatch <- cbind(xhyp_majmatch$x[3:4,], as.data.frame(yhyp_fd_majmatch)[3:4,] )

## Gender

pe <- fixef(lme_gender_simcf)
vc <- vcov(lme_gender_simcf) 
simbetas <- mvrnorm(sims, pe, vc)

xhyp_gender <- xhyp_update(xhyp_gender, simyear, oldsimyear)

yhyp_gender <- logitsimev(xhyp_gender, simbetas)
yhyp_fd_gender <- logitsimfd(xhyp_gender, simbetas)

preds_gender <-  cbind(xhyp_gender$x, as.data.frame(yhyp_gender))
preds_fd_gender <- cbind(xhyp_gender$x[3:4,], as.data.frame(yhyp_fd_gender)[3:4,] )

## well educ
pe <- fixef(lme_educ_simcf)
vc <- vcov(lme_educ_simcf) 
simbetas <- mvrnorm(sims, pe, vc)

xhyp_educ <- xhyp_update(xhyp_educ, simyear, oldsimyear)

yhyp_educ <- logitsimev(xhyp_educ, simbetas)
yhyp_fd_educ <- logitsimfd(xhyp_educ, simbetas)

preds_educ <-  cbind(xhyp_educ$x, as.data.frame(yhyp_educ))
preds_fd_educ <- cbind(xhyp_educ$x[3:4,], as.data.frame(yhyp_fd_educ)[3:4,])

## preds by incomey
pe <- fixef(lme_income_simcf)
vc <- vcov(lme_income_simcf) 
simbetas <- mvrnorm(sims, pe, vc)

xhyp_income <- xhyp_update(xhyp_income, simyear, oldsimyear)

yhyp_income <- logitsimev(xhyp_income, simbetas)
yhyp_fd_income <- logitsimfd(xhyp_income, simbetas)

preds_income <-  cbind(xhyp_income$x, as.data.frame(yhyp_income))
preds_fd_income <- cbind(xhyp_income$x[3:4,], as.data.frame(yhyp_fd_income)[3:4,])

### youth


pe <- fixef(lme_youth_simcf)
vc <- vcov(lme_youth_simcf) 
simbetas <- mvrnorm(sims, pe, vc)

xhyp_youth <- xhyp_update(xhyp_youth, simyear, oldsimyear)

yhyp_youth <- logitsimev(xhyp_youth, simbetas)
yhyp_fd_youth <- logitsimfd(xhyp_youth, simbetas)

preds_youth <-  cbind(xhyp_youth$x, as.data.frame(yhyp_youth))

preds_fd_youth <- cbind(xhyp_youth$x[3:4,], as.data.frame(yhyp_fd_youth)[3:4,])


#### trying with ggplot

#### trying with ggplot
preds_fd_race$group <- "Race"
preds_fd_race$level <- c("White", "Black", "Hispanic", "Asian", "Unknown")

preds_fd_gender$group <- "Gender"
preds_fd_gender$level <- c("Male", "Female")

preds_fd_income$group <- "Income"
preds_fd_income$level <- c("Higher income area", "Lower income area")

preds_fd_educ$group <- "Education"
preds_fd_educ$level <- c("More college degrees", "Fewer college degrees")


preds_fd_youth$group <- "Youth"
preds_fd_youth$level <- c("Under 30", "30 and older")


preds_fd_majmatch$group <- "Majority Race"
preds_fd_majmatch$level <- c("Majority race", "Not Majority race")

preds_fd_plot_2017 <- bind_rows(preds_fd_race, preds_fd_gender, preds_fd_income, preds_fd_youth, preds_fd_educ, preds_fd_majmatch)


ggplot(preds_fd_plot_2017, aes(x = group, y = pe, ymin = lower, ymax =upper, color = group, group = level)) +
  geom_pointrange(position = position_dodge(width = .6)) +
  geom_text(aes(y = -0.0025, label = level), hjust = 1, position = position_dodge(width = .6), size = 3) +
  geom_hline(aes(yintercept = 0)) +
  labs(x = "", y = "Change in predicted probability of voting in 2017 for \nnon-PB voters after hypothetical participation in PB") +
  ylim(-.075,.4) +
  coord_flip() +
  theme_minimal() +
  theme(legend.position = "none")
ggsave("Paper/Figs/group_fds_2017.pdf", width = 6.5, height = 5.5)



## experimenting both years one plot ---
preds_fd_plot_2017$predyear <- "2017"
preds_fd_plot$predyear <- "2016"

preds_fd_comb <- bind_rows(preds_fd_plot, preds_fd_plot_2017) 

ggplot(preds_fd_comb, aes(x = group, y = pe, ymin = lower, ymax =upper, color = group, 
                          shape = predyear, linetype = predyear, alpha = predyear, group = level)) +
  geom_pointrange(position = position_dodge(width = .6)) +
  geom_text(aes(y = -0.0025, label = level), hjust = 1, position = position_dodge(width = .6), size = 3) +
  geom_hline(aes(yintercept = 0)) +
  scale_alpha_discrete("Year", range = c(.9, .3)) +
  scale_color_discrete(guide = FALSE) +
  labs(x = "", shape = "Year", linetype = "Year",
       y = "Change in predicted probability of voting for \nnon-PB voters after counterfactual participation in PB") +
  ylim(-.08,.3) +
  coord_flip() +
  theme_minimal() +
  theme(legend.position = "bottom",
        axis.title=element_text(size=10))
ggsave("Paper_text/Figs/group_fds_bothyears.pdf", width = 6.75, height = 6.5)


### Fit statistics for each ------------------------------------------------------------------------------------
library(stargazer)

modelist <- list(lme_base_simcf, lme_race_simcf, lme_majmatch_simcf, lme_gender_simcf, lme_educ_simcf, lme_income_simcf, lme_youth_simcf)
# stargazer(lme_base_simcf, lme_race_simcf, lme_gender_simcf, lme_educ_simcf, lme_income_simcf, lme_youth_simcf)
stargazer(modelist, 
          out = "Paper_text/Tables/subgroups_SG.tex", label = "coefficients",
          title = "Individual voter turnout difference-in-difference regression results: including sub-group interactions for `triple-difference' results",
          column.labels = c("Base", "*Race", " *Maj. Match", " *Gender", "*Education", "*Income", "*Youth"),
          model.numbers = TRUE,
          order = c("^pb$", "^after\\_pb$", "^election\\_p$", "^election\\_pp$",
                    "^race\\_B$", "^race\\_A$",  "^race\\_H$", "^race\\_U$",
                    "^Female$", "^age$", "^I\\(age\\^2\\)$", "I\\(age\\_at\\_vote < 18\\)TRUE", 
                    "^college\\_pct$", "^medhhinc\\_10k$", "^majmatchTRUE$",
                    "^after\\_pb\\:race\\_B$", "^after\\_pb\\:race\\_A$", "^after\\_pb\\:race\\_H$", "^after\\_pb\\:race\\_U$",
                    "^after\\_pb\\:majmatchTRUE$", "^after\\_pb\\:Female$", "^after\\_pb\\:college\\_pct$",
                      "^after\\_pb\\:medhhinc\\_10k$", "^youth$", "^after\\_pb\\:youth$"),
          covariate.labels = c("PB district", "After PB", "Primary election", "Pres. Primary",
                               "Black", "Asian", "Hispanic", "Race Unknown",
                               "Female", "Age in years", "Age\\textsuperscript{2}", "18+ at vote", 
                               "\\% college educated", "Median HH income", "Majority Race",
                               "After PB * Black", "After PB * Asian", "After PB * Hispanic", "After PB * Unknown",
                               "After PB * Majority match", "After PB * Female",
                               "After PB * \\% college", "After PB * Median HH. inc.", "Age < 30", "After PB * Age < 30"),
          dep.var.labels.include = FALSE, dep.var.caption = "",
          digit.separator = "", digits = 2, digits.extra = 0, align = TRUE,
          intercept.bottom = TRUE, no.space = TRUE,
          single.row = TRUE, float.env = "sidewaystable",
          column.sep.width = "-30pt",
          omit = c("year_20\\d\\d$",
                   "year_20\\d\\d\\:|:year_20\\d\\d$"),
          add.lines = list(
            c("Year fixed effects?", rep("\\multicolumn{1}{c}{Yes}", length(modelist))),
              c("Subgroup time trends?", "\\multicolumn{1}{c}{None}", "\\multicolumn{1}{c}{Race}", "\\multicolumn{1}{c}{Maj. Match}", "\\multicolumn{1}{c}{Gender}", "\\multicolumn{1}{c}{Education}", "\\multicolumn{1}{c}{Income}", "\\multicolumn{1}{c}{Youth}")),
          # omit.labels = c("Year fixed effects?",
          #                 "Year*Covariate interactions?"),
          keep.stat = c("n", "aic", "bic", "n"),
          star.char = "*", star.cutoffs = 0.05,
          notes = "\\parbox[t]{\\textwidth}{\\footnotesize \\textit{Note:} ``Triple difference'' regression results, including interactions conditioning treatment effect by designated covariates, from multilevel mixed effect logistic models of individual turnout in a given election, including random effects for individual and council districts.  Standard errors reported in parentheses and statistical significance at $p<0.05$: $^{*}$.}",
          notes.label = "",
          notes.align = "l",
          notes.append = FALSE
          )


####  ----
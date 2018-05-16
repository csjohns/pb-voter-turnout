### Semi-systematic tests  ##############################################
## Increment variable inclusion
## Confirming that models with FEs for simcf now produce identical coefs as base R models
# This was included as part of the process of de-bugging SIMCF but is kind of useful as a reality check/teaching moment so I left the code in.
logit_full_fm <- turned_out ~ pb + after_pb*Race + as.factor(year) + election_type + age + medhhinc + white +  (1 | VANID) + (1|NYCCD)
lme_full <-  glmer(logit_full_fm, data = pb_long, family = binomial(), nAGQ = 0) 
summary(lme_full)

lme_base <- glmer(turned_out ~ pb + after_pb + election_type + (1 | VANID) + (1|NYCCD), data = pb_long, family = binomial(), nAGQ = 0)
lme_indiv <- glmer(turned_out ~ pb + after_pb + election_type + age + Sex + Race + (1 | VANID) + (1|NYCCD), data = pb_long, family = binomial(), nAGQ = 0)
lme_indiv_ageatvote <- glmer(turned_out ~ pb + after_pb + election_type + age_at_vote + Sex + Race + (1 | VANID) + (1|NYCCD), data = pb_long, family = binomial(), nAGQ = 0)
lme_indiv_poly <-  glmer(turned_out ~ pb + after_pb + election_type + age_at_vote + I(age_at_vote^2) + Sex + Race + (1 | VANID) + (1|NYCCD), data = pb_long, family = binomial(), nAGQ = 0)
lme_indiv_interact <-  glmer(turned_out ~ pb + after_pb*Race + election_type + age_at_vote + I(age_at_vote^2) + Sex + Race + (1 | VANID) + (1|NYCCD), data = pb_long, family = binomial(), nAGQ = 0)
lme_district <- glmer(turned_out ~ pb + after_pb*Race + election_type + age_at_vote + I(age_at_vote^2) + Sex + Race + 
                        medhhinc + white + college + (1 | VANID) + (1|NYCCD), data = pb_long, family = binomial(), nAGQ = 0)
lme_district_nocollege <- glmer(turned_out ~ pb + after_pb*Race + election_type + age_at_vote + I(age_at_vote^2) + Sex + Race + 
                                  medhhinc + white + (1 | VANID) + (1|NYCCD), data = pb_long, family = binomial(), nAGQ = 0)

BIC(lme_base)
BIC(lme_indiv)
BIC(lme_indiv_ageatvote)
BIC(lme_indiv_poly)
BIC(lme_indiv_interact)
BIC(lme_district) # incl college is better
BIC(lme_district_nocollege)

summary(lme_base)
summary(lme_indiv)
summary(lme_indiv_ageatvote)
summary(lme_indiv_poly)
summary(lme_indiv_interact)
summary(lme_district)
summary(lme_district_nocollege)

# verdict - lme_district is the best based on AIC (haven't done any explorations of AvP plots...)

## Omit people who can't vote at time of vote

## Include previous year's vote
pb_long <- pb_long %>% 
  group_by(VANID, election_type) %>% 
  arrange(year) %>% 
  mutate(lag_turned_out = lag(turned_out))

lme_lag <- glmer(turned_out ~ lag_turned_out + pb + after_pb*Race + election_type + age_at_vote + I(age_at_vote^2) + Sex + Race + 
                   medhhinc + white + (1 | VANID) + (1|NYCCD), data = pb_long, family = binomial(), nAGQ = 0)
AIC(lme_lag)
AIC(lme_district)
summary(lme_lag)
summary(lme_district)

lme_lag_set <- pb_long %>% filter(!is.na(lag_turned_out)) %>% 
  glmer(turned_out ~ pb + after_pb*Race + election_type + age_at_vote + I(age_at_vote^2) + Sex + Race + 
          medhhinc + white + (1 | VANID) + (1|NYCCD), data = ., family = binomial(), nAGQ = 0)

AIC(lme_lag)
AIC(lme_lag_set)
summary(lme_lag_set)
summary(lme_lag)

# verdict - there's no strong benefit or impact from including a lag term for turnout in the model.

#including a lag of previous vote does result 
## exploring in PLM

library(plm)
plmdata <-  pb_long %>% filter(election_type == "g") %>% 
  pdata.frame(index = c("VANID", "year"))

  pgmm(formula(turned_out ~ pb + after_pb*Race + age + medhhinc_rs + white), data = plmdata)#, effect = "twoways", model = "onestep")

  starts <- glm( turned_out ~ pb + after_pb*Race + age + medhhinc_rs + white, data = plmdata, family = binomial())$coefficients
pglmtest <-   pglm( turned_out ~ pb + after_pb*Race + age + medhhinc_rs + white, data = plmdata, effect = "twoways", model = "random", family = binomial(), method = "bfgs")

plmtest <- plm( turned_out ~ pb + after_pb*Race + age + medhhinc_rs + white, data = plmdata, effect = "twoways", model = "random")
lmecompare <- lmer(turned_out ~ pb + after_pb*Race + age_at_vote
                   + medhhinc_rs + white + as.factor(year) + (1 | VANID) , data = plmdata_lag)
lcompare <- lm( turned_out ~ pb + after_pb*Race + age_at_vote + medhhinc_rs + white, data = plmdata_lag)
  
left_join(data.frame(varnames = names(coefficients(plmtest)), coefficients(plmtest), stringsAsFactors = FALSE), data.frame(varnames = names(fixef(lmecompare)), fixef(lmecompare), stringsAsFactors = FALSE))
#what I learn twoways random = ranef for id and fixef for year

plmdata_lag <-pb_long %>% filter(election_type == "g") %>% 
  filter(age_at_vote >17) %>% 
  pdata.frame(index = c("VANID", "year"))

plmtest_lag <- plm( turned_out ~ lag(turned_out, 1) +pb + after_pb*Race + age_at_vote + medhhinc_rs + white, data = plmdata_lag, effect = "twoways", model = "random")
summary(plmtest_lag)
## effect of pb (after_pb) appears to persist - but not when limiting to older voters?

## also tried loimiting analysis to only voters older than 17 at age of vote. This reinforces effect even more.

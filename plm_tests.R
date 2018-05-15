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

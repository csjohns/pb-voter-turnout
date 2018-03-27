
########################### CODE FOR MODEL TO PREDICT VIA SIMCF ------------------------------------------------------------
logit_full_simcf <- turned_out ~ pb + after_pb*race_B + after_pb*race_A + after_pb*race_H + after_pb*race_U +
  year_2009 + year_2010 + year_2011 + year_2012 + year_2013 + year_2014 + year_2015 + year_2016 +
  election_p + election_pp + age + medhhinc + white + (1 | VANID) 
lme_full_simcf <- glmer(logit_full_simcf, data = pb_long, family = binomial(), nAGQ = 0)    #start = list(fixef = bas_log$coefficients), 
#######################################################################################################################################
logit_full_form <- turned_out ~ pb + after_pb*race_B + after_pb*race_A + after_pb*race_H + after_pb*race_U +
  year_2009 + year_2010 + year_2011 + year_2012 + year_2013 + year_2014 + year_2015 + year_2016 +
  election_p + election_pp + age + medhhinc + white 

library(MASS)

sims <- 1000
pe <- fixef(lme_full_simcf)
vc <- vcov(lme_full_simcf) 
simbetas <- mvrnorm(sims, pe, vc)

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

yhyp <- logitsimev(xhyp, simbetas)

yhyp_fd <- logitsimfd(xhyp, simbetas)

preds <-  cbind(xhyp$x, as.data.frame(yhyp)) %>% 
  mutate(Race = rep(c("W", "B", "H", "A", "U"), 2)) 

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



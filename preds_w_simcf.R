
################################################################################################################################################
######################## DISAGGREGATE BY RACE ------------------------------------------------------------------------------------
### Estimated model
logit_full_simcf <- turned_out ~ pb + after_pb*race_B + after_pb*race_A + after_pb*race_H + after_pb*race_U +
  year_2009 + year_2010 + year_2011 + year_2012 + year_2013 + year_2014 + year_2015 + year_2016 +
  election_p + election_pp + age + medhhinc + white + (1 | VANID) + (1 | NYCCD)
lme_full_simcf <- glmer(logit_full_simcf, data = pb_long, family = binomial(), nAGQ = 0)    #start = list(fixef = bas_log$coefficients), 
#######################################################################################################################################
### Formula for simcf
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

simyear <- "2016"
for (i in 1:nscen){
  xhyp <- cfChange(xhyp, "age", x = mean(pb_long$age), xpre = mean(pb_long$age), scen = i)
  xhyp <- cfChange(xhyp, "medhhinc", x = mean(pb_long$medhhinc), xpre = mean(pb_long$medhhinc), scen = i)
  xhyp <- cfChange(xhyp, "white", x = mean(pb_long$white), xpre = mean(pb_long$white), scen = i)
  xhyp <- cfChange(xhyp, paste0("year_", simyear), x = 1, xpre = 1, scen = i)
}
for (i in (nscen/2+1):nscen){
  xhyp <- cfChange(xhyp, "after_pb", x = 1, xpre = 0, scen = i)
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
  labs(title = paste0("By race: redicted probability of voting in general election in ", simyear, "\n before and after PB (in a non-PB district)"), 
       x = "", y = "Predicted probability of Voting", color = "") +
  coord_flip() +
  theme_minimal()
ggsave("Paper/Figs/byrace.pdf", width = 5, height = 4)


################################################################################################################################################
######################## DISAGGREGATE BY Youth ---------------------
### Estimated model
pb_long <- pb_long %>% mutate(youth = ifelse(age <= 25, 1, 0))
logit_full_simcf <- turned_out ~ pb + after_pb*youth + race_B + race_A + race_H + race_U +
  year_2009 + year_2010 + year_2011 + year_2012 + year_2013 + year_2014 + year_2015 + year_2016 +
  election_p + election_pp + age + medhhinc + white + (1 | VANID) + (1 | NYCCD)
lme_full_simcf <- glmer(logit_full_simcf, data = pb_long, family = binomial(), nAGQ = 0)    #start = list(fixef = bas_log$coefficients), 
#######################################################################################################################################
### Formula for simcf
logit_full_form <-  turned_out ~ pb + after_pb*youth + race_B + race_A + race_H + race_U +
  year_2009 + year_2010 + year_2011 + year_2012 + year_2013 + year_2014 + year_2015 + year_2016 +
  election_p + election_pp + age + medhhinc + white

sims <- 1000
pe <- fixef(lme_full_simcf)
vc <- vcov(lme_full_simcf) 
simbetas <- mvrnorm(sims, pe, vc)

nscen <- length(unique(pb_long$youth))*2
xhyp <- cfMake(logit_full_form, pb_long, nscen = nscen, f = "mean")

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
  xhyp <- cfChange(xhyp, paste0("year_", simyear), x = 1, xpre = 1, scen = i)
}

for (i in (nscen/2+1):nscen){
  xhyp <- cfChange(xhyp, "after_pb", x = 1, xpre = 0, scen = i)
}
for (i in 1:(nscen/2)){
  xhyp <- cfChange(xhyp, "after_pb", x = 0, xpre = 0, scen = i)
}

for (i in c(1,3)){
  xhyp <- cfChange(xhyp, "youth", x = 1, xpre = 1, scen = i)
  xhyp <- cfChange(xhyp, "age", x = mean(pb_long$age[pb_long$youth == 1]), xpre = mean(pb_long$age[pb_long$youth == 0]), scen = i)
}
for (i in c(2,4)){
  xhyp <- cfChange(xhyp, "youth", x = 0, xpre = 0, scen = i)
  xhyp <- cfChange(xhyp, "age", x = mean(pb_long$age[pb_long$youth == 0]), xpre = mean(pb_long$age[pb_long$youth == 0]), scen = i)
}

yhyp <- logitsimev(xhyp, simbetas)

yhyp_fd <- logitsimfd(xhyp, simbetas)

preds <-  cbind(xhyp$x, as.data.frame(yhyp))

preds %>% dplyr::select(after_pb, youth, pe, lower, upper) %>% 
  mutate(after_pb = factor(after_pb, levels = c(0,1), labels = c("No PB", "After PB")),
         youth = factor(youth, levels = c(1, 0), labels = c("25 and under", "Older than 25"))) %>% 
  ggplot(aes(y = pe, ymin = lower, ymax = upper, x = youth)) + 
  # geom_segment(aes(xend = Race, y = 0, yend = preds, color = as.factor(after_pb)))+
  geom_pointrange(aes(color = as.factor(after_pb))) +
  labs(title = paste0("By age: predicted probability of voting in general election in ", simyear, "\n before and after PB (in a non-PB district)"), 
       x = "", y = "Predicted probability of Voting", color = "") +
  coord_flip() +
  theme_minimal()
ggsave("Paper/Figs/byyouth.pdf", width = 5, height = 4)



################################################################################################################################################
######################## DISAGGREGATE BY gender ---------------------------------------------------------------------------------------------------------
### Estimated model

logit_full_simcf <- turned_out ~ pb + after_pb*Sex + race_B + race_A + race_H + race_U +
  year_2009 + year_2010 + year_2011 + year_2012 + year_2013 + year_2014 + year_2015 + year_2016 +
  election_p + election_pp + age + medhhinc + white + (1 | VANID) + (1 | NYCCD)
lme_full_simcf <- glmer(logit_full_simcf, data = pb_long, family = binomial(), nAGQ = 0)    #start = list(fixef = bas_log$coefficients), 
#######################################################################################################################################
### Formula for simcf
logit_full_form <-  turned_out ~ pb + after_pb*Sex + race_B + race_A + race_H + race_U +
  year_2009 + year_2010 + year_2011 + year_2012 + year_2013 + year_2014 + year_2015 + year_2016 +
  election_p + election_pp + age + medhhinc + white

sims <- 1000
pe <- fixef(lme_full_simcf)
vc <- vcov(lme_full_simcf) 
simbetas <- mvrnorm(sims, pe, vc)

nscen <- length(unique(pb_long$youth))*2
xhyp <- cfMake(logit_full_form, pb_long, nscen = nscen, f = "mean")

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
  xhyp <- cfChange(xhyp, paste0("year_", simyear), x = 1, xpre = 1, scen = i)
}

for (i in (nscen/2+1):nscen){
  xhyp <- cfChange(xhyp, "after_pb", x = 1, xpre = 0, scen = i)
}
for (i in 1:(nscen/2)){
  xhyp <- cfChange(xhyp, "after_pb", x = 0, xpre = 0, scen = i)
}

for (i in c(1,3)){
  xhyp <- cfChange(xhyp, "Sex", x = "F", xpre = "F", scen = i)
}
for (i in c(2,4)){
  xhyp <- cfChange(xhyp, "Sex", x = "M", xpre = "M", scen = i)
}

yhyp <- logitsimev(xhyp, simbetas)

yhyp_fd <- logitsimfd(xhyp, simbetas)

preds <-  cbind(xhyp$x, as.data.frame(yhyp))

preds %>% dplyr::select(after_pb, Sex, pe, lower, upper) %>% 
  mutate(after_pb = factor(after_pb, levels = c(0,1), labels = c("No PB", "After PB")),
         youth = factor(Sex, levels = c("M", "F"), labels = c("Male", "Female"))) %>% 
  ggplot(aes(y = pe, ymin = lower, ymax = upper, x = youth)) + 
  # geom_segment(aes(xend = Race, y = 0, yend = preds, color = as.factor(after_pb)))+
  geom_pointrange(aes(color = as.factor(after_pb))) +
  labs(title = paste0("By age: predicted probability of voting in general election in ", simyear, "\n before and after PB (in a non-PB district)"), 
       x = "", y = "Predicted probability of Voting", color = "") +
  coord_flip() +
  theme_minimal()
ggsave("Paper/Figs/bygender.pdf", width = 5, height = 4)



################################################################################################################################################
######################## DISAGGREGATE BY well educ ---------------------------------------------------------------------------------------------------------
### Estimated model
pb_long <- pb_long %>% mutate(wealthy = as.numeric(medhhinc > quantile(medhhinc, probs = .5)))
logit_full_simcf <- turned_out ~ pb + after_pb*wealthy + race_B + race_A + race_H + race_U +
  year_2009 + year_2010 + year_2011 + year_2012 + year_2013 + year_2014 + year_2015 + year_2016 +
  election_p + election_pp + age +  white + (1 | VANID) + (1 | NYCCD)
lme_full_simcf <- glmer(logit_full_simcf, data = pb_long, family = binomial(), nAGQ = 0)    #start = list(fixef = bas_log$coefficients), 
#######################################################################################################################################
### Formula for simcf
logit_full_form <-  turned_out ~ pb + after_pb*wealthy + race_B + race_A + race_H + race_U +
  year_2009 + year_2010 + year_2011 + year_2012 + year_2013 + year_2014 + year_2015 + year_2016 +
  election_p + election_pp + age + white

sims <- 1000
pe <- fixef(lme_full_simcf)
vc <- vcov(lme_full_simcf) 
simbetas <- mvrnorm(sims, pe, vc)

nscen <- length(unique(pb_long$wealthy))*2
xhyp <- cfMake(logit_full_form, pb_long, nscen = nscen, f = "mean")

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
  xhyp <- cfChange(xhyp, paste0("year_", simyear), x = 1, xpre = 1, scen = i)
}

for (i in (nscen/2+1):nscen){
  xhyp <- cfChange(xhyp, "after_pb", x = 1, xpre = 0, scen = i)
}
for (i in 1:(nscen/2)){
  xhyp <- cfChange(xhyp, "after_pb", x = 0, xpre = 0, scen = i)
}

for (i in c(1,3)){
  xhyp <- cfChange(xhyp, "Wealthy area", x = 1, xpre = 1, scen = i)
}
for (i in c(2,4)){
  xhyp <- cfChange(xhyp, "Not wealthy area", x = 0, xpre = 0, scen = i)
}

yhyp <- logitsimev(xhyp, simbetas)

yhyp_fd <- logitsimfd(xhyp, simbetas)

preds <-  cbind(xhyp$x, as.data.frame(yhyp))

preds %>% dplyr::select(after_pb, Sex, pe, lower, upper) %>% 
  mutate(after_pb = factor(after_pb, levels = c(0,1), labels = c("No PB", "After PB")),
         wealthy = factor(wealthy, levels = c(1, 0), labels = c("Wealthy area", "Not wealthy area"))) %>% 
  ggplot(aes(y = pe, ymin = lower, ymax = upper, x = youth)) + 
  # geom_segment(aes(xend = Race, y = 0, yend = preds, color = as.factor(after_pb)))+
  geom_pointrange(aes(color = as.factor(after_pb))) +
  labs(title = paste0("By age: predicted probability of voting in general election in ", simyear, "\n before and after PB (in a non-PB district)"), 
       x = "", y = "Predicted probability of Voting", color = "") +
  coord_flip() +
  theme_minimal()
ggsave("Paper/Figs/bygender.pdf", width = 5, height = 4)

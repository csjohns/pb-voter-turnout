

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
logit_base_simcf <- turned_out ~ pb + after_pb + race_B + race_A + race_H + race_U +
  year_2009 + year_2010 + year_2011 + year_2012 + year_2013 + year_2014 + year_2015 + year_2016 +
  election_p + election_pp + age_at_vote + medhhinc + white + (1 | VANID) + (1 | NYCCD)
lme_base_simcf <- glmer(logit_base_simcf, data = pb_long, family = binomial(), nAGQ = 0)    #start = list(fixef = bas_log$coefficients), 
#######################################################################################################################################
### Formula for simcf
logit_base_form <-  turned_out ~ pb + after_pb + race_B + race_A + race_H + race_U +
  year_2009 + year_2010 + year_2011 + year_2012 + year_2013 + year_2014 + year_2015 + year_2016 +
election_p + election_pp + age_at_vote + medhhinc + white

library(MASS)

sims <- 10000
pe <- fixef(lme_base_simcf)
vc <- vcov(lme_base_simcf) 
simbetas <- mvrnorm(sims, pe, vc)

nscen <- 3
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
  xhyp <- cfChange(xhyp, "election_pp", x = 0, xpre = 0, scen = i)
  xhyp <- cfChange(xhyp, "election_p", x = 0, xpre = 0, scen = i)
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
  scale_y_continuous(labels = percent) +
  labs(title = "Probability of voting in a general election", 
       subtitle = "Predictions for non-PB voters, showing effect of hypothetical participation in PB vote", 
       x = "", y = "Predicted probability of Voting", color = "") +
  # coord_flip() +
  theme_minimal()
ggsave("Paper/Figs/base_by_year.pdf", width = 6, height = 5)
ggsave("Paper/Figs/base_by_year.png", width = 6, height = 5)

################################################################################################################################################
######################## DISAGGREGATE BY RACE ------------------------------------------------------------------------------------
### Estimated model
logit_full_simcf <- turned_out ~ pb + pb*race_B + pb*race_A + pb*race_H + pb*race_U + 
  after_pb*race_B + after_pb*race_A + after_pb*race_H + after_pb*race_U +
  year_2009*race_B + year_2009*race_A + year_2009*race_H + year_2009*race_U +
  year_2010*race_B + year_2010*race_A + year_2010*race_H + year_2010*race_U +
  year_2011*race_B + year_2011*race_A + year_2011*race_H + year_2011*race_U +
  year_2012*race_B + year_2012*race_A + year_2012*race_H + year_2012*race_U +
  year_2013*race_B + year_2013*race_A + year_2013*race_H + year_2013*race_U +
  year_2014*race_B + year_2014*race_A + year_2014*race_H + year_2014*race_U +
  year_2015*race_B + year_2015*race_A + year_2015*race_H + year_2015*race_U +
  year_2016*race_B + year_2016*race_A + year_2016*race_H + year_2016*race_U +
  election_p + election_pp + age + medhhinc + white + (1 | VANID) + (1 | NYCCD)
lme_full_simcf <- glmer(logit_full_simcf, data = pb_long, family = binomial(), nAGQ = 0)    #start = list(fixef = bas_log$coefficients), 



#######################################################################################################################################
### Formula for simcf
logit_full_form <- logit_full_simcf <- turned_out ~ pb + pb*race_B + pb*race_A + pb*race_H + pb*race_U + 
  after_pb*race_B + after_pb*race_A + after_pb*race_H + after_pb*race_U +
  year_2009*race_B + year_2009*race_A + year_2009*race_H + year_2009*race_U +
  year_2010*race_B + year_2010*race_A + year_2010*race_H + year_2010*race_U +
  year_2011*race_B + year_2011*race_A + year_2011*race_H + year_2011*race_U +
  year_2012*race_B + year_2012*race_A + year_2012*race_H + year_2012*race_U +
  year_2013*race_B + year_2013*race_A + year_2013*race_H + year_2013*race_U +
  year_2014*race_B + year_2014*race_A + year_2014*race_H + year_2014*race_U +
  year_2015*race_B + year_2015*race_A + year_2015*race_H + year_2015*race_U +
  year_2016*race_B + year_2016*race_A + year_2016*race_H + year_2016*race_U +
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

cbind(xhyp$x[6:10,], as.data.frame(yhyp_fd)[6:10,]) %>% 
  mutate(Race = c("W", "B", "H", "A", "U")) %>% 
  dplyr::select(after_pb, Race, pe, lower, upper) %>% 
  mutate(after_pb = factor(after_pb, levels = c(0,1), labels = c("No PB", "After PB")),
         Race = factor(Race, levels = c("W", "B", "H", "A", "U"), labels = c("White", "Black", "Hispanic", "Asian", "Unknown"))) %>% 
  ggplot(aes(y = pe, ymin = lower, ymax = upper, x = Race)) + 
  # geom_segment(aes(xend = Race, y = 0, yend = preds, color = as.factor(after_pb)))+
  geom_pointrange() +
  labs(title = paste0("Change in predicted probability of voting in general election in ", simyear, "\n before and after PB (in a non-PB district)"), 
       x = "", y = "Change in predicted probability of voting", color = "") +
  coord_flip() +
  theme_minimal()
ggsave("Paper/Figs/fd_byrace.pdf", width = 5, height = 4)


################################################################################################################################################
######################## DISAGGREGATE BY Youth ---------------------
### Estimated model
pb_long <- pb_long %>% mutate(youth = ifelse(age <= 25, 1, 0))
logit_full_simcf <- turned_out ~ pb + after_pb*youth + race_B + race_A + race_H + race_U +
  year_2009*youth + year_2010*youth + year_2011*youth + year_2012*youth + year_2013*youth + year_2014*youth + year_2015*youth + year_2016*youth +
  election_p + election_pp + age_at_vote + medhhinc + white + (1 | VANID) + (1 | NYCCD)
lme_full_simcf <- glmer(logit_full_simcf, data = pb_long, family = binomial(), nAGQ = 0)    #start = list(fixef = bas_log$coefficients), 
#######################################################################################################################################
### Formula for simcf
logit_full_form <-  turned_out ~ pb + after_pb*youth + race_B + race_A + race_H + race_U +
  year_2009*youth + year_2010*youth + year_2011*youth + year_2012*youth + year_2013*youth + year_2014*youth + year_2015*youth + year_2016*youth +
  election_p + election_pp + age_at_vote + medhhinc + white

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
  xhyp <- cfChange(xhyp, "age_at_vote", x = mean(pb_long$age_at_vote[pb_long$youth == 1]), xpre = mean(pb_long$age_at_vote[pb_long$youth == 0]), scen = i)
}
for (i in c(2,4)){
  xhyp <- cfChange(xhyp, "youth", x = 0, xpre = 0, scen = i)
  xhyp <- cfChange(xhyp, "age", x = mean(pb_long$age_at_vote[pb_long$youth == 0]), xpre = mean(pb_long$age_at_vote[pb_long$youth == 0]), scen = i)
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

######################## DISAGGREGATE BY Youth - ONLY 18+ ---------------------
# See scratchpad - reran these models for only elections 2011 on, this made the differential effect for youth disappear.  Suspicion that the fact that youth didn't vote in 2008 may be driving the apparent greater impact of PB
### Estimated model
pb_long <- pb_long %>% filter(age_at_vote >= 18)
logit_full_simcf <- turned_out ~ pb + after_pb*youth + race_B + race_A + race_H + race_U +
  year_2009 + year_2010 + year_2011 + year_2012*youth + year_2013*youth + year_2014*youth + year_2015*youth + year_2016*youth +
  election_p + election_pp + age_at_vote + medhhinc + white + (1 | VANID) + (1 | NYCCD)
lme_full_simcf <- glmer(logit_full_simcf, data = pb_long, family = binomial(), nAGQ = 0)    #start = list(fixef = bas_log$coefficients), 
#######################################################################################################################################
### Formula for simcf
logit_full_form <-  turned_out ~ pb + after_pb*youth + race_B + race_A + race_H + race_U +
  year_2009 + year_2010 + year_2011 + year_2012*youth + year_2013*youth + year_2014*youth + year_2015*youth + year_2016*youth +
  election_p + election_pp + age_at_vote + medhhinc + white

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
  xhyp <- cfChange(xhyp, "age_at_vote", x = mean(pb_long$age_at_vote[pb_long$youth == 1]), xpre = mean(pb_long$age_at_vote[pb_long$youth == 0]), scen = i)
}
for (i in c(2,4)){
  xhyp <- cfChange(xhyp, "youth", x = 0, xpre = 0, scen = i)
  xhyp <- cfChange(xhyp, "age", x = mean(pb_long$age_at_vote[pb_long$youth == 0]), xpre = mean(pb_long$age_at_vote[pb_long$youth == 0]), scen = i)
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
  year_2009*Sex + year_2010*Sex + year_2011*Sex + year_2012*Sex + year_2013*Sex + year_2014*Sex + year_2015*Sex + year_2016*Sex +
  election_p + election_pp + age + medhhinc + white + (1 | VANID) + (1 | NYCCD)
lme_full_simcf <- glmer(logit_full_simcf, data = pb_long, family = binomial(), nAGQ = 0)    #start = list(fixef = bas_log$coefficients), 
#######################################################################################################################################
### Formula for simcf
logit_full_form <-  turned_out ~ pb + after_pb*Sex + race_B + race_A + race_H + race_U +
  year_2009*Sex + year_2010*Sex + year_2011*Sex + year_2012*Sex + year_2013*Sex + year_2014*Sex + year_2015*Sex + year_2016*Sex +
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

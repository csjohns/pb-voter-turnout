

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


#pb_long_orig <- pb_long # making a copy in case all is f'ed up and don't want to rerun all processing code

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
  election_p + election_pp + age + I(age_at_vote < 18) + medhhinc + white + (1 | VANID) + (1 | NYCCD)
lme_base_simcf <- glmer(logit_base_simcf, data = pb_long, family = binomial(), nAGQ = 0)    #start = list(fixef = bas_log$coefficients), 
#######################################################################################################################################
### Formula for simcf
logit_base_form <-  turned_out ~ pb + after_pb + race_B + race_A + race_H + race_U +
  year_2009 + year_2010 + year_2011 + year_2012 + year_2013 + year_2014 + year_2015 + year_2016 +
  election_p + election_pp + age + I(age_at_vote < 18) + medhhinc + white

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
logit_race_simcf <- turned_out ~ pb + pb*race_B + pb*race_A + pb*race_H + pb*race_U + 
  after_pb*race_B + after_pb*race_A + after_pb*race_H + after_pb*race_U +
  year_2009*race_B + year_2009*race_A + year_2009*race_H + year_2009*race_U +
  year_2010*race_B + year_2010*race_A + year_2010*race_H + year_2010*race_U +
  year_2011*race_B + year_2011*race_A + year_2011*race_H + year_2011*race_U +
  year_2012*race_B + year_2012*race_A + year_2012*race_H + year_2012*race_U +
  year_2013*race_B + year_2013*race_A + year_2013*race_H + year_2013*race_U +
  year_2014*race_B + year_2014*race_A + year_2014*race_H + year_2014*race_U +
  year_2015*race_B + year_2015*race_A + year_2015*race_H + year_2015*race_U +
  year_2016*race_B + year_2016*race_A + year_2016*race_H + year_2016*race_U +
  election_p + election_pp + age + I(age_at_vote < 18) + medhhinc + white + (1 | VANID) + (1 | NYCCD)
lme_race_simcf <- glmer(logit_race_simcf, data = pb_long, family = binomial(), nAGQ = 0)    #start = list(fixef = bas_log$coefficients), 



#######################################################################################################################################
### Formula for simcf
logit_race_form <- logit_race_simcf <- turned_out ~ pb + pb*race_B + pb*race_A + pb*race_H + pb*race_U + 
  after_pb*race_B + after_pb*race_A + after_pb*race_H + after_pb*race_U +
  year_2009*race_B + year_2009*race_A + year_2009*race_H + year_2009*race_U +
  year_2010*race_B + year_2010*race_A + year_2010*race_H + year_2010*race_U +
  year_2011*race_B + year_2011*race_A + year_2011*race_H + year_2011*race_U +
  year_2012*race_B + year_2012*race_A + year_2012*race_H + year_2012*race_U +
  year_2013*race_B + year_2013*race_A + year_2013*race_H + year_2013*race_U +
  year_2014*race_B + year_2014*race_A + year_2014*race_H + year_2014*race_U +
  year_2015*race_B + year_2015*race_A + year_2015*race_H + year_2015*race_U +
  year_2016*race_B + year_2016*race_A + year_2016*race_H + year_2016*race_U +
  election_p + election_pp + age + I(age_at_vote < 18) +medhhinc + white

library(MASS)

sims <- 1000
pe <- fixef(lme_race_simcf)
vc <- vcov(lme_race_simcf) 
simbetas <- mvrnorm(sims, pe, vc)

nscen <- length(unique(pb_long$Race))*2
xhyp <- cfMake(logit_race_form, pb_long, nscen = nscen, f = "min")

simyear <- "2016"
for (i in 1:nscen){
  xhyp <- cfChange(xhyp, "age", x = mean(pb_long$age), xpre = mean(pb_long$age), scen = i)
  xhyp <- cfChange(xhyp, "medhhinc", x = mean(pb_long$medhhinc), xpre = mean(pb_long$medhhinc), scen = i)
  xhyp <- cfChange(xhyp, "white", x = mean(pb_long$white), xpre = mean(pb_long$white), scen = i)
  xhyp <- cfChange(xhyp, paste0("year_", simyear), x = 1, xpre = 1, scen = i)
  xhyp <- cfChange(xhyp, "age_at_vote", x = mean(pb_long$age_at_vote), xpre = mean(pb_long$age_at_vote), scen = i)
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

preds_race <-  cbind(xhyp$x, as.data.frame(yhyp)) %>% 
  mutate(Race = rep(c("W", "B", "H", "A", "U"), 2)) 

preds_fd_race <-  cbind(xhyp$x[6:10,], as.data.frame(yhyp_fd)[6:10,]) %>% 
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

#race ropeladder 
trace_race <- ropeladder(x = preds_fd$pe,
                    lower = preds_fd$lower,
                    upper = preds_fd$upper,
                    plot = 1)
trace_race$sublabels = preds_fd$R
tile(trace_race, trace_youth,
     RxC = c(1,2))

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
######################## DISAGGREGATE BY gender ---------------------------------------------------------------------------------------------------------
### Estimated model

pb_long <- pb_long %>% mutate(Female = ifelse(Sex == "F", 1, 0))
logit_gender_simcf <- turned_out ~ pb + after_pb*Female + race_B + race_A + race_H + race_U +
  year_2009*Female + year_2010*Female + year_2011*Female + year_2012*Female + year_2013*Female + year_2014*Female + year_2015*Female + year_2016*Female +
  election_p + election_pp + age + I(age_at_vote < 18) + medhhinc + white + (1 | VANID) + (1 | NYCCD)
lme_gender_simcf <- glmer(logit_gender_simcf, data = pb_long, family = binomial(), nAGQ = 0)    #start = list(fixef = bas_log$coefficients), 
#######################################################################################################################################
### Formula for simcf
logit_gender_form <-  turned_out ~ pb + after_pb*Female + race_B + race_A + race_H + race_U +
  year_2009*Female + year_2010*Female + year_2011*Female + year_2012*Female + year_2013*Female + year_2014*Female + year_2015*Female + year_2016*Female +
  election_p + election_pp + age + I(age_at_vote < 18) + medhhinc + white

sims <- 1000
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
  xhyp <- cfChange(xhyp, paste0("year_", simyear), x = 1, xpre = 1, scen = i)
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

yhyp <- logitsimev(xhyp, simbetas)

yhyp_fd <- logitsimfd(xhyp, simbetas)

preds_gender <-  cbind(xhyp$x, as.data.frame(yhyp))

preds_fd_gender <- cbind(xhyp$x[3:4,], as.data.frame(yhyp_fd)[3:4,] )

preds_gender %>% dplyr::select(after_pb, Female, pe, lower, upper) %>% 
  mutate(after_pb = factor(after_pb, levels = c(0,1), labels = c("No PB", "After PB")),
         youth = factor(Female, levels = c(0,1), labels = c("Male", "Female"))) %>% 
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
logit_educ_simcf <- turned_out ~ pb + after_pb*wealthy + race_B + race_A + race_H + race_U +
  year_2009 + year_2010 + year_2011 + year_2012 + year_2013 + year_2014 + year_2015 + year_2016 +
  election_p + election_pp + age + I(age_at_vote < 18) + white + (1 | VANID) + (1 | NYCCD)
lme_educ_simcf <- glmer(logit_educ_simcf, data = pb_long, family = binomial(), nAGQ = 0)    #start = list(fixef = bas_log$coefficients), 
#######################################################################################################################################
### Formula for simcf
logit_educ_form <-  turned_out ~ pb + after_pb*wealthy + race_B + race_A + race_H + race_U +
  year_2009 + year_2010 + year_2011 + year_2012 + year_2013 + year_2014 + year_2015 + year_2016 +
  election_p + election_pp + age + I(age_at_vote < 18) + white

sims <- 1000
pe <- fixef(lme_educ_simcf)
vc <- vcov(lme_educ_simcf) 
simbetas <- mvrnorm(sims, pe, vc)

nscen <- length(unique(pb_long$wealthy))*2
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
  xhyp <- cfChange(xhyp, paste0("year_", simyear), x = 1, xpre = 1, scen = i)
}

for (i in (nscen/2+1):nscen){
  xhyp <- cfChange(xhyp, "after_pb", x = 1, xpre = 0, scen = i)
}
for (i in 1:(nscen/2)){
  xhyp <- cfChange(xhyp, "after_pb", x = 0, xpre = 0, scen = i)
}

for (i in c(1,3)){
  xhyp <- cfChange(xhyp, "wealthy", x = 1, xpre = 1, scen = i)
}
for (i in c(2,4)){
  xhyp <- cfChange(xhyp, "wealthy", x = 0, xpre = 0, scen = i)
}

yhyp <- logitsimev(xhyp, simbetas)

yhyp_fd <- logitsimfd(xhyp, simbetas)

preds_educ <-  cbind(xhyp$x, as.data.frame(yhyp))
preds_fd_educ <- cbind(xhyp$x[3:4,], as.data.frame(yhyp_fd)[3:4,])

preds_educ %>% dplyr::select(after_pb, wealthy, pe, lower, upper) %>% 
  mutate(after_pb = factor(after_pb, levels = c(0,1), labels = c("No PB", "After PB")),
         wealthy = factor(wealthy, levels = c(1, 0), labels = c("Wealthy area", "Not wealthy area"))) %>% 
  ggplot(aes(y = pe, ymin = lower, ymax = upper, x = wealthy)) + 
  # geom_segment(aes(xend = Race, y = 0, yend = preds, color = as.factor(after_pb)))+
  geom_pointrange(aes(color = as.factor(after_pb))) +
  labs(title = paste0("By age: predicted probability of voting in general election in ", simyear, "\n before and after PB (in a non-PB district)"), 
       x = "", y = "Predicted probability of Voting", color = "") +
  coord_flip() +
  theme_minimal()
ggsave("Paper/Figs/byeduc.pdf", width = 5, height = 4)

################################################################################################################################################
######################## DISAGGREGATE BY wealthy ---------------------------------------------------------------------------------------------------------
### Estimated model
pb_long <- pb_long %>% mutate(wealthy = as.numeric(medhhinc > quantile(medhhinc, probs = .5)))
logit_wealth_simcf <- turned_out ~ pb + after_pb*wealthy + race_B + race_A + race_H + race_U +
  year_2009 + year_2010 + year_2011 + year_2012 + year_2013 + year_2014 + year_2015 + year_2016 +
  election_p + election_pp + age + I(age_at_vote < 18) + white + (1 | VANID) + (1 | NYCCD)
lme_wealth_simcf <- glmer(logit_wealth_simcf, data = pb_long, family = binomial(), nAGQ = 0)    #start = list(fixef = bas_log$coefficients), 
#######################################################################################################################################
### Formula for simcf
logit_wealth_form <-  turned_out ~ pb + after_pb*wealthy + race_B + race_A + race_H + race_U +
  year_2009 + year_2010 + year_2011 + year_2012 + year_2013 + year_2014 + year_2015 + year_2016 +
  election_p + election_pp + age + I(age_at_vote < 18) + white

sims <- 1000
pe <- fixef(lme_wealth_simcf)
vc <- vcov(lme_wealth_simcf) 
simbetas <- mvrnorm(sims, pe, vc)

nscen <- length(unique(pb_long$wealthy))*2
xhyp <- cfMake(logit_wealth_form, pb_long, nscen = nscen, f = "mean")

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
  xhyp <- cfChange(xhyp, "wealthy", x = 1, xpre = 1, scen = i)
}
for (i in c(2,4)){
  xhyp <- cfChange(xhyp, "wealthy", x = 0, xpre = 0, scen = i)
}

yhyp <- logitsimev(xhyp, simbetas)

yhyp_fd <- logitsimfd(xhyp, simbetas)

preds_wealth <-  cbind(xhyp$x, as.data.frame(yhyp))
preds_fd_wealth <- cbind(xhyp$x[3:4,], as.data.frame(yhyp_fd)[3:4,])

preds_wealth %>% dplyr::select(after_pb, wealthy, pe, lower, upper) %>% 
  mutate(after_pb = factor(after_pb, levels = c(0,1), labels = c("No PB", "After PB")),
         wealthy = factor(wealthy, levels = c(1, 0), labels = c("Wealthy area", "Not wealthy area"))) %>% 
  ggplot(aes(y = pe, ymin = lower, ymax = upper, x = wealthy)) + 
  # geom_segment(aes(xend = Race, y = 0, yend = preds, color = as.factor(after_pb)))+
  geom_pointrange(aes(color = as.factor(after_pb))) +
  labs(title = paste0("By age: predicted probability of voting in general election in ", simyear, "\n before and after PB (in a non-PB district)"), 
       x = "", y = "Predicted probability of Voting", color = "") +
  coord_flip() +
  theme_minimal()
ggsave("Paper/Figs/bywealth.pdf", width = 5, height = 4)

################################################################################################################################################
######################## DISAGGREGATE BY Youth ---------------------
### Estimated model
pb_long <- pb_long %>% mutate(youth = ifelse(age <= 25, 1, 0))
logit_youth_simcf <- turned_out ~ pb + after_pb*youth + race_B + race_A + race_H + race_U +
  year_2009*youth + year_2010*youth + year_2011*youth + year_2012*youth + year_2013*youth + year_2014*youth + year_2015*youth + year_2016*youth +
  election_p + election_pp + age + I(age_at_vote < 18) + medhhinc + white + (1 | VANID) + (1 | NYCCD)
lme_youth_simcf <- glmer(logit_youth_simcf, data = pb_long, family = binomial(), nAGQ = 0)    #start = list(fixef = bas_log$coefficients), 
#######################################################################################################################################
### Formula for simcf
logit_youth_form <-  turned_out ~ pb + after_pb*youth + race_B + race_A + race_H + race_U +
  year_2009*youth + year_2010*youth + year_2011*youth + year_2012*youth + year_2013*youth + year_2014*youth + year_2015*youth + year_2016*youth +
  election_p + election_pp + age + I(age_at_vote < 18) + medhhinc + white

sims <- 1000
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
  xhyp <- cfChange(xhyp, "age_at_vote", x = mean(pb_long$age_at_vote[pb_long$youth == 1 & pb_long$year == 2016 &pb_long$age_at_vote >= 18]), xpre = mean(pb_long$age_at_vote[pb_long$youth == 1 & pb_long$year == 2016 &pb_long$age_at_vote >= 18]), scen = i)
}
for (i in c(2,4)){
  xhyp <- cfChange(xhyp, "youth", x = 0, xpre = 0, scen = i)
  xhyp <- cfChange(xhyp, "age", x = mean(pb_long$age_at_vote[pb_long$youth == 0]), xpre = mean(pb_long$age_at_vote[pb_long$youth == 0]), scen = i)
}

yhyp <- logitsimev(xhyp, simbetas)

yhyp_fd <- logitsimfd(xhyp, simbetas)

preds_youth <-  cbind(xhyp$x, as.data.frame(yhyp))

preds_fd_youth <- cbind(xhyp$x[3:4,], as.data.frame(yhyp_fd)[3:4,])



trace_youth <- ropeladder(x = preds_fd$pe,
                          lower = preds_fd$lower,
                          upper = preds_fd$upper,
                          col = "red",
                          plot = 1)
tile( trace_youth, limits =c(-1,1,0,10))

preds_fd_all <- bind_rows(preds_fd[1:5,], preds_fd_youth)
preds_fd_all$names <- c(preds_fd$Race[1:5], preds_fd_youth$youth)
preds_fd_all$variable <- c(rep("Race", 5), rep("Youth",2))
preds_fd_all %>% ggplot(aes(x = names, y = pe, ymin = lower, ymax = upper, color = variable)) +
  geom_pointrange() + coord_flip()

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
pb_long_18p <- pb_long %>% mutate(youth = ifelse(age <= 29, 1, 0)) %>%  filter(age_at_vote >= 18)
logit_youth18p_simcf <- turned_out ~ pb + after_pb*youth + race_B + race_A + race_H + race_U +
  year_2009*youth + year_2010*youth + year_2011*youth + year_2012*youth + year_2013*youth + year_2014*youth + year_2015*youth + year_2016*youth +
  election_p + election_pp + age + medhhinc + white + (1 | VANID) + (1 | NYCCD)
lme_youth18p_simcf <- glmer(logit_youth18p_simcf, data = pb_long_18p, family = binomial(), nAGQ = 0)    #start = list(fixef = bas_log$coefficients), 
#######################################################################################################################################
### Formula for simcf
logit_youth18p_form <-   turned_out ~ pb + after_pb*youth + race_B + race_A + race_H + race_U +
  year_2009*youth + year_2010*youth + year_2011*youth + year_2012*youth + year_2013*youth + year_2014*youth + year_2015*youth + year_2016*youth +
  election_p + election_pp + age + medhhinc + white

sims <- 1000
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
         youth = factor(youth, levels = c(1, 0), labels = c("25 and under", "Older than 25"))) %>% 
  ggplot(aes(y = pe, ymin = lower, ymax = upper, x = youth)) + 
  # geom_segment(aes(xend = Race, y = 0, yend = preds, color = as.factor(after_pb)))+
  geom_pointrange(aes(color = as.factor(after_pb))) +
  labs(title = paste0("By age: predicted probability of voting in general election in ", simyear, "\n before and after PB (in a non-PB district)"), 
       x = "", y = "Predicted probability of Voting", color = "") +
  coord_flip() +
  theme_minimal()
ggsave("Paper/Figs/byyouth.pdf", width = 5, height = 4)

### Trying to make a ropeladder of all fds ------------------------------------------------------------------

trace_race <- ropeladder(x = preds_fd_race$pe,
                         lower = preds_fd_race$lower,
                         upper = preds_fd_race$upper,
                         labels = c("White", "Black", "Hispanic", "Asian", "Unknown"),
                         plot = 1)

trace_gender <- ropeladder(x = preds_fd_gender$pe,
                           lower = preds_fd_gender$lower,
                           upper = preds_fd_gender$upper,
                           labels = c("Male", "Female"),
                           col = "red",
                           plot = 2)
  
trace_wealthy <- ropeladder(x = preds_fd_wealthy$pe,
                         lower = preds_fd_wealthy$lower,
                         upper = preds_fd_wealthy$upper,
                         labels = c("Wealthy area", "Not wealthy area"),
                         col = "blue",
                         shadowrow = TRUE,
                         plot = 3)

tile(trace_race, trace_gender, trace_wealthy, RxC = c(3,1))

#### trying with ggplot
preds_fd_race$group <- "Race"
preds_fd_race$level <- c("White", "Black", "Hispanic", "Asian", "Unknown")

preds_fd_gender$group <- "Gender"
preds_fd_gender$level <- c("Male", "Female")

preds_fd_wealthy$group <- "Neighborhood Wealth"
preds_fd_wealthy$level <- c("Wealthy area", "Not wealthy area")

preds_fd_youth$group <- "Youth"
preds_fd_youth$level <- c("25 and under", "Older than 25")

preds_fd_plot <- bind_rows(preds_fd_race, preds_fd_gender, preds_fd_wealthy, preds_fd_youth)

ggplot(preds_fd_plot, aes(x = group, y = pe, ymin = lower, ymax =upper, color = group, group = level)) +
  geom_pointrange(position = position_dodge(width = .5)) +
  geom_text(aes(y = 0, label = level), hjust = 0, position = position_dodge(width = .5), size = 3) +
  geom_hline(aes(yintercept = 0)) +
  labs(y = "Change in predicted probability of voting in 2016 for \nnon-PB voters after hypothetical participation in PB") +
  ylim(-.025,.2) +
  coord_flip() +
  theme_minimal() +
  theme(legend.position = "none")
ggsave("Paper/Figs/group_fds.pdf", width = 6, height = 8)

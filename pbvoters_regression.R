## Exploring initial PB voter regression
library(tidyr)
library(dplyr)
library(stringr)
library(margins)
library(lme4)

# must run "ed_explore.R" first


names(pb)

pb <- pb %>%
  rename(g_2012 = `2012G`,
         g_2013 = `2013G`,
         g_2014 = `2014G`,
         g_2015 = `2015G`,
         p_2016 = `2016PP`,
         p_2008 = `2008PP`,
         p_2012 = `2012P`,
         p_2013 = `2013P`,
         p_2014 = `2014P`,
         p_2015 = `2015P`,
         g_2016 = `2016G`,
         g_2010 = `2010G`,
         g_2008 = `2008G`,
         g_2009 = `2009G`,
         g_2011 = `2011G`
  )

pb_long <- pb %>% select(-starts_with("g_2"), -starts_with("p_2")) %>%
  gather( year, pb, starts_with("pb")) %>%
  mutate(year = as.numeric(str_replace(year, "pb_", "")),
         pb = as.numeric(pb))
glimpse(pb_long)

pb_long <- pb_long %>% group_by(VANID) %>%
  arrange(VANID, year) %>%
  mutate(pbyear  = ifelse(pb == 1, year, NA),
         pb_start = min(pbyear, na.rm = TRUE), 
         pb_count = sum(pb, na.rm = TRUE))
summary(pb_long)

elec_long <- pb %>% select(-starts_with("pb_")) %>%
  gather(election, turned_out, starts_with("p_2"), starts_with("g_2")) %>%
  separate(election, c("election_type", "year")) %>%
  mutate(turned_out = ifelse(turned_out == "Y", 1, 0),
         year = as.numeric(year)) 

pb_long <- pb_long %>% full_join(elec_long)
pb_long <- pb_long %>%
  mutate(after_pb = year > pb_start,
         #after_pb = ifelse(is.na(after_pb), 0, after_pb),
         repeater = pb_count > 1)

base_formula = turned_out ~ after_pb + as.factor(year)
base_lm <- lm(base_formula, data = pb_long)
summary(base_lm)

base_logit <- glm(base_formula, data = pb_long, family = binomial())
summary(base_logit)

predict(base_logit, newdata = expand.grid(year = 2013, after_pb = c(FALSE,TRUE)), type = "response")

dydx(pb_long, base_logit, "after_pbTRUE", change = c(0,1))

## expansions
mod = turned_out ~ after_pb + as.factor(year) + repeater
logit <- glm(mod, data = pb_long, family = binomial())
summary(logit)

# fixed effects
base_formula = turned_out ~ after_pb + (as.factor(year)|VANID) #this model doesn't converge, may have to do w/ VANID
base_fe <- glmer(base_formula, data = pb_long, family = binomial())
summary(base_fe)

# expansions
mod <- glmer(turned_out ~ after_pb + election_type + (as.factor(year)|VANID),
             data = pb_long, family = binomial())
summary(mod)

mmod <- turned_out ~ after_pb + election_type + 
                    
  
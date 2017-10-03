library(RColorBrewer)
library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)
library(margins)


source("credentials.R") # loads the access credentials
source("dbDownload.R")

stringNAs <- function(x){
  ifelse(x, "", NA)
}

### Loading and checking the data ###

pb_orig <- dbDownload(table = "pb", username = username, password = password, dbname = db.name, host = hostname, port = port)
rm(password, username, hostname, db.name, port) # if you want to remove the credentials from your environment 
pb <- pb_orig



## Exploring initial PB voter regression

names(pb)

## renaming/recoding variables----
pb <- pb %>%
  rename(g_2012 = `2012G`,
         g_2013 = `2013G`,
         g_2014 = `2014G`,
         g_2015 = `2015G`,
         pp_2016 = `2016PP`,
         pp_2008 = `2008PP`,
         p_2012 = `2012P`,
         p_2013 = `2013P`,
         p_2014 = `2014P`,
         p_2015 = `2015P`,
         p_2016 = `2016P`,
         g_2016 = `2016G`,
         g_2010 = `2010G`,
         g_2008 = `2008G`,
         g_2009 = `2009G`,
         g_2011 = `2011G`
  ) 


## source("fullvfmerge.R") ##------------
pb <- pb %>% rename(pb_2012 = `2012PB`,
                    pb_2013 = `2013PB`,
                    pb_2014 = `2014PB`,
                    pb_2015 = `2015PB`,
                    pb_2016 = `2016PB`) %>%
  mutate_at(vars(starts_with("pb_")), funs(ifelse(. == "", 0, 1)))



pb <- pb %>% 
  group_by(VANID) %>%
  mutate(DoB = ifelse(DoB == "", NA, DoB),
         DoB = as.Date(format(as.Date(DoB, format = "%m/%d/%y"), "19%y-%m-%d")),
         totpb = sum(pb_2012, pb_2013, pb_2014, pb_2015 , pb_2016, na.rm = T)
)


## Reshaping long for regression ----------------------------------------------------------------
pb_long <- pb %>% filter(totpb >0) %>% select(VANID, totpb, starts_with("pb_")) %>%
  gather( year, pb, starts_with("pb")) %>%
  mutate(year = as.numeric(str_replace(year, "pb_", "")),
         pb = as.numeric(pb))
glimpse(pb_long)
summary(pb_long)

pb_long <- pb_long %>% group_by(VANID) %>%
  arrange(VANID, year) %>%
  mutate(pbyear  = ifelse(pb == 1, year, NA),
         pb_start = min(pbyear, na.rm = TRUE)
         )
summary(pb_long)

elec_long <- pb %>% select(-starts_with("pb_")) %>%
  gather(election, turned_out, starts_with("p_2"), starts_with("g_2"), starts_with("pp_")) %>%
  separate(election, c("election_type", "year")) %>%
  mutate(turned_out = ifelse(turned_out %in% c("A", "D", "R", "Y"), 1, 0),
         year = as.numeric(year)) 

pb_long <- pb_long %>% full_join(elec_long)
pb_long <- pb_long %>%
  mutate(pb = ifelse(is.na(pb), 0, pb),
         after_pb = as.numeric(year > pb_start),
         after_pb = ifelse(is.na(after_pb), 0, after_pb),
         repeater = totpb > 1,
         age_at_vote = year - year(DoB) )

ggplot(pb_long) + geom_bar(aes(x = as.factor(turned_out), fill = election_type), stat = "count", position = "dodge") + facet_wrap(~year)
ggplot(pb_long) + geom_bar(aes(x = as.factor(turned_out), fill = as.factor(after_pb)), position = "dodge") + facet_wrap(~year)

## joining pb_long with census data
pb_long <- pb_long %>% mutate(countycode = recode(County, BRONX = "005", KINGS = "047", `NEW YORK` = "061", QUEENS = "081", RICHMOND = "085")) %>%
  mutate(countycode = ifelse(countycode %in% c("005", "047", "061", "081", "085"), countycode, NA),
         tract = paste0(countycode, str_pad(CensusTract, 6, "left", "0")))

pb_long <- pb_long %>% 
  left_join(educ) %>%
  left_join(inc) %>% 
  left_join(race)

## preliminary year fixed effect regressions --------------------------------
#define regression equation
base_formula = turned_out ~ after_pb + as.factor(year)

base_lm <- lm(base_formula, data = pb_long)
summary(base_lm)

base_logit <- glm(base_formula, data = pb_long, family = binomial())
summary(base_logit)

predict(base_logit, newdata = expand.grid(year = 2013, after_pb = c(0,1)), type = "response", se.fit = TRUE)

dydx(pb_long, base_logit, "after_pb", change = c(0,1))

## FE logit with covariates ----------------------------------------
covar_formula <- turned_out ~ after_pb + as.factor(year) + Race + Sex + age_at_vote + election_type
covar_logit <- glm(covar_formula, data = pb_long, family = binomial())
summary(covar_logit)

dydx(pb_long, covar_logit, "after_pb", change = c(0,1))

covar_formula <- turned_out ~ after_pb*repeater + as.factor(year) + Race + Sex + age_at_vote + election_type + high_school + 
  medhhinc + white 
covar_logit <- glm(covar_formula, data = pb_long, family = binomial())
summary(covar_logit)

##------ INDIVIDUAL FES

base_formula <- turned_out ~ -1 + after_pb + year + (year|VANID)
base_fe <- pb_long %>% mutate(year = as.factor(year)) %>% glmer(base_formula, data = ., family = binomial())

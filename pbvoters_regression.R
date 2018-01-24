library(RColorBrewer)
library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)
library(margins)
library(ggplot2)

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

## more renaming fun (resulting from adding additional years from full voter file)
pb <- pb %>% rename(pb_2012 = `2012PB`,
                    pb_2013 = `2013PB`,
                    pb_2014 = `2014PB`,
                    pb_2015 = `2015PB`,
                    pb_2016 = `2016PB`) %>%
  mutate_at(vars(starts_with("pb_")), funs(ifelse(. == "", 0, 1)))


# Dealing with DoB
pb <- pb %>% 
  group_by(VANID) %>%
  mutate(DoB = ifelse(DoB == "", NA, DoB),
         DoB = as.Date(format(as.Date(DoB, format = "%m/%d/%y"), "19%y-%m-%d")),
         totpb = sum(pb_2012, pb_2013, pb_2014, pb_2015 , pb_2016, na.rm = T)
  )

#### TO RERUN THIS WITH THE DATA FROM OUR UPDATED MATCH!
source("credentials.R") # loads the access credentials
pb <- dbDownload(table = "pb_update", username = username, password = password, dbname = db.name, host = hostname, port = port)
rm(password, username, hostname, db.name, port) # if you want to remove the credentials from your environment 


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

#exploring distribution of PB voters across districts:
pb_long %>% filter(pb == 1) %>% group_by(NYCCD, year) %>% tally() %>% 
  ggplot() + geom_bar(aes(x = NYCCD, y = n, fill = NYCCD), stat = "identity") + facet_wrap(~year, ncol = 4) + 
  coord_flip() + theme_minimal() + guides(fill = FALSE)

#exploring distributions of voters across districts

ggplot(pb_long) + geom_bar(aes(x = as.factor(turned_out), fill = election_type), stat = "count", position = "dodge") + facet_wrap(~year)
ggplot(pb_long) + geom_bar(aes(x = as.factor(turned_out), fill = as.factor(after_pb)), position = "dodge") + facet_wrap(~year)

## joining pb_long with census data
source("censustables.R")
pb_long <- pb_long %>% mutate(countycode = recode(County, BRONX = "005", KINGS = "047", `NEW YORK` = "061", QUEENS = "081", RICHMOND = "085")) %>%
  mutate(countycode = ifelse(countycode %in% c("005", "047", "061", "081", "085"), countycode, NA),
         tract = paste0(countycode, str_pad(CensusTract, 6, "left", "0")))

pb_long <- pb_long %>% 
  left_join(educ) %>%
  left_join(inc) %>% 
  left_join(race)

## preliminary year fixed effect regressions --------------------------------
#define regression equation

# subsetting pb_long to only PB districts 23 & 39, as they're the only districts we have multiple years' lists:
pb_long <- pb_long %>% filter(pbdistrict %in% c(23,39))

base_formula = turned_out ~ after_pb + as.factor(year)

base_lm <- lm(base_formula, data = pb_long)
summary(base_lm)

base_logit <- glm(base_formula, data = pb_long, family = binomial())
summary(base_logit)

#getting marginal effect of having voted in PB (on predicted turnout) for two years (1-2 are not after pb, 3-4 are the same years assuming person did vote in a previous PB)
predict(base_logit, newdata = expand.grid(year = c(2013, 2016), after_pb = c(0,1)), type = "response", se.fit = TRUE)


#getting marginal effect of vote after pb
dydx(pb_long, base_logit, "after_pb", change = c(0,1))[[1]] %>% mean

##  logit with covariates ----------------------------------------
covar_formula <- turned_out ~ after_pb + as.factor(year) + Race + age_at_vote + election_type  #+ Sex commented out for new data - prefect prediction in Sex- not enough variation in all xtabs. Could make sex Binary and it would run
covar_logit <- glm(covar_formula, data = pb_long, family = binomial())
summary(covar_logit)

dydx(pb_long, covar_logit, "after_pb", change = c(0,1))[[1]] %>% mean

## logit with even more covariates -> local census tract info AND interacting with whether voter was a repeated voter
## both after PB and repeater both positive and significant - the interaction is negative, dampening the joint effect a bit but not enough to eliminate all amplification
covar_formula <- turned_out ~ after_pb*repeater + as.factor(year) + Race  + age_at_vote + election_type + high_school + 
  medhhinc + white #+ Sex again commented out sex for updated match
covar_logit <- glm(covar_formula, data = pb_long, family = binomial())
summary(covar_logit)

##------ HIERARCHICAL MODEL WITH RANDOM EFFECTS FOR INDIVIDUALS
## These don't work - non-converging! First hunch is that there are too many zeros
# base_formula <- turned_out ~  after_pb + year + (1|VANID)
# base_fe <- pb_long %>% mutate(year = as.factor(year)) %>% glmer(base_formula, data = ., family = binomial())

# estimating with a linear model as converges more easily - adding individual level effects dampens the effect of PB a bit, but not substantially, stil positive and significant
base_formula <- turned_out ~  after_pb + year + (1|VANID)
base_fe <- pb_long %>% mutate(year = as.factor(year)) %>% lmer(base_formula, data = .)


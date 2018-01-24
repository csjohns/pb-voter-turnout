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

### Loading new pb table (from ExploringSonyaMaster.R - using Sonya's updated export) #### 

pb_orig <- dbDownload(table = "pb", username = username, password = password, dbname = db.name, host = hostname, port = port)
rm(password, username, hostname, db.name, port) # if you want to remove the credentials from your environment 
pb <- pb_orig

# Dealing with DoB, calculating total PB votes (not that these will usually be off by 1 because 2014 has too many voters (everyone who came before))
pb <- pb %>% 
  group_by(VANID) %>%
  mutate(DoB = mdy(DoB),
         totpb = sum(pb_2012, pb_2013, pb_2014, pb_2015 , pb_2016, na.rm = T)
)


## Reshaping long for regression ----------------------------------------------------------------
pb_long <- pb %>% filter(totpb >0) %>% select(VANID, totpb, starts_with("pb_")) %>%
  gather( year, pb, starts_with("pb")) %>%
  mutate(year = as.numeric(str_replace(year, "pb_", "")),
         pb = as.numeric(pb))
glimpse(pb_long)
summary(pb_long)

# this code calculates pb start year - not that this code is good even though 2014 is a wacky error year since voters who didn't vote before 2014 will indeed have their start year be 2014
pb_long <- pb_long %>% group_by(VANID) %>%
  arrange(VANID, year) %>%
  mutate(pbyear  = ifelse(pb == 1, year, NA),
         pb_start = min(pbyear, na.rm = TRUE)
         )
summary(pb_long)

elec_long <- pb %>% select(-starts_with("pb_")) %>%
  gather(election, turned_out, starts_with("p_2"), starts_with("g_2"), starts_with("pp_")) %>%
  separate(election, c("election_type", "year")) %>%
  mutate(turned_out = ifelse(turned_out != "", 1, 0),
         year = as.numeric(year)) 

pb_long <- pb_long %>% full_join(elec_long)
pb_long <- pb_long %>%
  mutate(pb = ifelse(is.na(pb), 0, pb),
         after_pb = as.numeric(year > pb_start),
         after_pb = ifelse(is.na(after_pb), 0, after_pb),
        # repeater = totpb > 1, removing this because errors in 2014 means every early voter is a repeater, which isn't correct
         age_at_vote = year - year(DoB) )
## I think there are some nonsense ages in here and I need to investigate DoB coding more

pb_long <- pb_long %>%  filter(year >= 2008)

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

## basic model with only fixed effects for year
base_formula = turned_out ~ after_pb + as.factor(year)

base_lm <- lm(base_formula, data = pb_long)
summary(base_lm)

## as logit
base_logit <- glm(base_formula, data = pb_long, family = binomial())
summary(base_logit)

#getting marginal effect of having voted in PB (on predicted turnout) for two years (1-2 are not after pb, 3-4 are the same years assuming person did vote in a previous PB)
predict(base_logit, newdata = expand.grid(year = c(2013, 2016), after_pb = c(0,1)), type = "response", se.fit = TRUE)

#getting marginal effect of vote after pb
dydx(pb_long, base_logit, "after_pb", change = c(0,1))[[1]] %>% mean

## simple logit model with covariates ----------------------------------------
covar_formula <- turned_out ~ after_pb + as.factor(year) + Race + age_at_vote + election_type  #+ Sex commented out for new data - prefect prediction in Sex- not enough variation in all xtabs. Could make sex Binary and it would run
covar_logit <- glm(covar_formula, data = pb_long, family = binomial())
summary(covar_logit)

dydx(pb_long, covar_logit, "after_pb", change = c(0,1))[[1]] %>% mean(na.rm = T)

## logit with even more covariates -> local census tract info AND interacting with whether voter was a repeated voter
covar_formula <- turned_out ~ after_pb + as.factor(year) + Race  + age_at_vote + election_type + high_school +  medhhinc + white #+ Sex again commented out sex for updated match
covar_logit <- glm(covar_formula, data = pb_long, family = binomial())
summary(covar_logit)

##------ HIERARCHICAL MODEL WITH RANDOM EFFECTS FOR INDIVIDUALS
## These don't work - non-converging! First hunch is that there are too many zeros
# base_formula <- turned_out ~ after_pb + year + (1|VANID)
# base_fe <- pb_long %>% mutate(year = as.factor(year)) %>% glmer(base_formula, data = ., family = binomial())

 
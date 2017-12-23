#####################################################################################################################################################################
## PB Voter Turnout Analysis
## December 2017 - Preliminary analysis with corrected results


### Preliminaries ------------------------------------------------------------------------------------------------------------------------------------------------

library(RColorBrewer)
library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)
library(margins)
library(ggplot2)

#source("credentials.R") # loads the access credentials
#source("dbDownload.R")

stringNAs <- function(x){
  ifelse(x, "", NA)
}

pbnyc <- read.csv(file = "pbnyc_district_votes.csv", as.is = TRUE)

#### Read in PB voter tally ------------------------------------------------------------------------------------------------------------------------------------------------

### adding in our "new" voters 
new_pb <- read.delim("PBNYCvoters-Source.txt", header = TRUE, as.is = TRUE)

new_van <- read.delim("PBNYCvoters-VoterfileData.xls", header = TRUE, as.is = TRUE)

pb_new <- new_pb %>% 
  rename(pb_cycle = COL10.CYCLE,
         pb_district = COL7.PB.VOTE.DISTRICT) %>%
  select(DWID, pb_cycle, pb_district) %>%
  inner_join(new_van) %>%
  distinct()

names(pb_new) <- names(pb_new) %>% 
  str_replace("General", "g_20") %>%
  str_replace("PresidentialPrimary", "pp_20") %>%
  str_replace("Primary", "p_20")

pb <- pb_new %>%
  select(-starts_with("Special"), -ends_with("Party"))

pb <- pb %>% 
  rename(Ethnicity = EthnicCatalistName,
         DoB = DOB,
         DoR = DateReg,
         RegStatus = RegistrationStatusName,
         County = CountyName,
        # City = CityName,
         Zip = Zip5,
         Lat = Latitude,
         Long = Longitude,
         NYCCD = CityCouncilName,
        pbdistrict = pb_district,
        pbcycle = pb_cycle)


# add cycle years, not just cycle id
pb <- pbnyc %>%
  select(pbnycCycle, voteYear) %>% 
  distinct() %>%
  rename(pbcycle = pbnycCycle,
         pbyear = voteYear) %>%
  left_join(pb,.)

pb <- pb %>% mutate(pbyear = paste0("pb_", pbyear))
## now just need to spread out the pb!
pb <- pb %>%
  mutate(voted = 1) %>%
  spread(pbyear, voted, fill = 0)
  

### need to get year and pb/electoral votes into order here next - 


### Read in PB data from Sonya's DB ------------------------------------------------------------------------------------------------------------------------------------------------

#pb_orig <- dbDownload(table = "pb", username = username, password = password, dbname = db.name, host = hostname, port = port)
#rm(password, username, hostname, db.name, port) # if you want to remove the credentials from your environment 
#pb <- pb_orig

## Reshape a la pbvoters_regression


pb <- pb %>% 
  group_by(VANID) %>%
  mutate(DoB = mdy(DoB),
         totpb = sum(pb_2013, pb_2014, pb_2015 , pb_2016, na.rm = T)
  )

pb <- pbnyc %>%
  rename(pbdistrict = district) %>%
  group_by(pbdistrict) %>%
  summarize(start_year = min(voteYear)) %>%
  left_join(pb, .)
  
  

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
  mutate(turned_out = ifelse(turned_out == "", 0, 1),
         year = as.numeric(year)) 

pb_long <- pb_long %>% full_join(elec_long)
pb_long <- pb_long %>%
  mutate(pb = ifelse(is.na(pb), 0, pb),
         after_pb = as.numeric(year >= pb_start),
         after_pb = ifelse(is.na(after_pb), 0, after_pb),
         repeater = totpb > 1,
         age_at_vote = year - year(DoB) )

#exploring distribution of PB voters across districts:
pb_long %>% filter(pb == 1) %>% group_by(pbdistrict, year) %>% tally() %>% 
  ggplot() + geom_bar(aes(x = pbdistrict, y = n, fill = pbdistrict), stat = "identity") + facet_wrap(~year, ncol = 4) + 
  coord_flip() + theme_minimal() + guides(fill = FALSE)

#exploring distributions of voters across districts

ggplot(pb_long) + geom_bar(aes(x = as.factor(turned_out), fill = election_type), stat = "count", position = "dodge") + facet_wrap(~year)
ggplot(pb_long) + geom_bar(aes(x = as.factor(turned_out), fill = as.factor(after_pb)), position = "dodge") + facet_wrap(~year)

## joining pb_long with census data
#source("censustables.R")
pb_long <- pb_long %>% mutate(countycode = recode(County, BRONX = "005", KINGS = "047", `NEW YORK` = "061", QUEENS = "081", RICHMOND = "085")) %>%
  mutate(countycode = ifelse(countycode %in% c("005", "047", "061", "081", "085"), countycode, NA),
         tract = paste0(countycode, str_pad(CensusTractName, 6, "left", "0")))

pb_long <- pb_long %>%
  left_join(educ) %>%
  left_join(inc) %>% 
  left_join(race)


## Try running the ed_explore file to just look at distributions

## FE logit with covariates ----------------------------------------
covar_formula <- turned_out ~ after_pb + as.factor(year) + Race + Sex + age_at_vote + election_type
covar_logit <-pb_long %>% filter(year >= 2008) %>%  glm(covar_formula, data = ., family = binomial())
summary(covar_logit)

dydx(pb_long, covar_logit, "after_pb", change = c(0,1))

covar_formula <- turned_out ~ after_pb*repeater + as.factor(year) + Race + Sex + age_at_vote + election_type + high_school + 
  medhhinc + white 
covar_logit <- glm(covar_formula, data = pb_long, family = binomial())
summary(covar_logit)

## different after pb measure - district treatment


# try lmer with fixed effects for year and random effects (incl slope for district)
# Also, possibly try the analysis with only the two districts with good time coverage; interacting PB with the district dummy (for 39 or not) and 
# Can also try an analysis where PB voters are coded as receiving 'treatment' the first year that their district receives PB.  THis is helped by the fact that not much happened electroally in 2015
## Reshaping etc

### Read in full vf data (if necessary) to merge earlier turnout years with PB data ------------------------------------------------------------------------------------------------------------------------------------------------

#### Read in PB voter tally ------------------------------------------------------------------------------------------------------------------------------------------------
pbnyc <- read.csv(file = "pbnyc_district_votes.csv", as.is = TRUE)

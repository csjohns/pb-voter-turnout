#################################################################################
### Code for comparing turnout among PB and and all voters, at the ED level.  ###
### Carolina Johnson                                                          ###
### 4/28/2017                                                                 ###
#################################################################################

require(RMySQL)
library(stringr)
library(dplyr)
library(tidyr)
library(ggplot2)

source("credentials.R") # loads the access credentials
source("dbDownload.R")

### Loading and checking the data ###

ed <- dbDownload(table = "electiondistricts", username = username, password = password, dbname = db.name, host = hostname, port = port)
pb <- dbDownload(table = "pb", username = username, password = password, dbname = db.name, host = hostname, port = port)
rm(password, username, hostname, db.name, port) # if you want to remove the credentials from your environment 

# checking match between pb and ed election district naming
names(pb)
length(unique(pb$ED))
length(intersect(unique(pb$ED), unique(ed$ED)))
setdiff(unique(pb$ED), unique(ed$ED))
# 2 pb$ED not in ed$ED: c("", "Tupper Lake - Ed 006")

## Checking for other Tupper Lake EDs
ed$ED[str_detect(ed$ED, "Tupper")]
## looks like it has EDs 001-005 for Tupper Lake, just not 006. Will disregard this missing ED

### Turnout ###
## Calculating ED turnout among PB voters

## summarize - count by ED, sum votes by ED
pb_turnout <- pb %>% group_by(ED) %>%
  summarise(n = n(),
            G2015 = sum(`2015G` == "Y")/n,
            G2014 = sum(`2014G` == "Y")/n,
            G2013 = sum(`2013G` == "Y")/n,
            G2012 = sum(`2012G` == "Y")/n
            ) %>%
  mutate(type = "pb")
  
  

ed_turnout <- ed %>% group_by(ED) %>%
  summarise(n = sum(Reg),
            G2016 = sum(`2016G`)/n,
            G2015 = sum(`2015G`)/n,
            G2014 = sum(`2014G`)/n,
            G2013 = sum(`2013G`)/n
  ) %>%
  mutate(type = "ed")

turnout <- full_join(pb_turnout, ed_turnout)
nrow(ed_turnout)+nrow(pb_turnout)==nrow(turnout)

turnout <- turnout %>% gather(year, turnout, starts_with("G")) %>%
  mutate(year = as.numeric(str_replace(year, "G", "")))

ggplot(turnout, aes(x = year, y = turnout, color = type)) +
  geom_jitter(alpha = .5)







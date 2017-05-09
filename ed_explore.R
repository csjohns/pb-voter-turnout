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
pb_turnout <- pb %>% group_by(ED, County) %>%
  summarise(n = n(),
            G2015 = sum(`2015G` == "Y")/n,
            G2014 = sum(`2014G` == "Y")/n,
            G2013 = sum(`2013G` == "Y")/n,
            G2012 = sum(`2012G` == "Y")/n
            ) %>%
  mutate(type = "pb",
         County = tolower(County))
  

ed_turnout <- ed %>% group_by(ED, County) %>%
  summarise(n = sum(Reg),
            G2016 = sum(`2016G`)/n,
            G2015 = sum(`2015G`)/n,
            G2014 = sum(`2014G`)/n,
            G2013 = sum(`2013G`)/n
  ) %>%
  mutate(type = "ed",
         County = tolower(County))

#### ALL EDS ####
# Joining PB voter turnout and ED turnout
turnout <- full_join(pb_turnout, ed_turnout)
# Checking full match:
nrow(ed_turnout)+nrow(pb_turnout)==nrow(turnout)

# Reshaping long for plotting
turnout <- turnout %>% gather(year, turnout, starts_with("G")) %>%
  mutate(year = as.numeric(str_replace(year, "G", "")))

# Uninspiring plot
pt <- ggplot(turnout, aes(x = as.factor(year), y = turnout, color = type))
pt + geom_jitter(alpha = 0.5)+ labs(x = "Year", y = "Turnout") + theme_minimal() + facet_wrap(~type)
ggsave("turnout_scatter.pdf")
  
pt + geom_boxplot(notch = TRUE) + labs(x = "Year", y = "Turnout") + theme_minimal()
ggsave("turnout_boxplot.pdf", width = 8, units = "in")

turnout %>%
  filter(County %in% c("kings", "richmond", "queens", "new york", "bronx")) %>%
  ggplot(aes(x = as.factor(year), y = turnout)) +
  geom_jitter(aes(color = as.factor(County)),alpha = 0.5)+ labs(color = "County", x = "Year", y = "Turnout", title = "Turnout in NYC EDs") + 
  theme_minimal() + facet_wrap(~type) +theme(legend.position = "bottom")
ggsave("turnout_scatter_county.pdf", width = 8, units = "in")

turnout %>%
  filter(County %in% c("kings", "richmond", "queens", "new york", "bronx")) %>%
  ggplot(aes(x = as.factor(year), y = turnout)) +
  geom_boxplot(aes(color = as.factor(type)),alpha = 0.5)+ labs(color = "Turnout type", x = "Year", y = "Turnout", title = "Turnout in NYC EDs") + 
  theme_minimal() + facet_wrap(~County) +theme(legend.position = "bottom")
ggsave("turnout_boxplot_county.pdf", width = 10, height = 7, units = "in")


#### PB EDS ONLY ####
### comparing like to like - including only EDs that had PB voters
turnout <- ed_turnout %>% 
  filter(ED %in% pb_turnout$ED) %>%
  full_join(pb_turnout, .)
# check merge
table(turnout$type)

# Reshaping long for plotting
turnout <- turnout %>% gather(year, turnout, starts_with("G")) %>%
  mutate(year = as.numeric(str_replace(year, "G", "")))

# Uninspiring plot
pt <- ggplot(turnout, aes(x = as.factor(year), y = turnout, color = type))
pt + geom_jitter(alpha = 0.5)+ labs(x = "Year", y = "Turnout") + theme_minimal() + facet_wrap(~type)
ggsave("PBonly_turnout_scatter.pdf")

pt + geom_boxplot(notch = TRUE) + labs(x = "Year", y = "Turnout") + theme_minimal()
ggsave("PBonly_turnout_boxplot.pdf", width = 8, units = "in")

turnout %>%
  filter(County %in% c("kings", "richmond", "queens", "new york", "bronx")) %>%
  ggplot(aes(x = as.factor(year), y = turnout)) +
  geom_jitter(aes(color = as.factor(County)),alpha = 0.5)+ labs(color = "County", x = "Year", y = "Turnout", 
                                                                title = "Turnout in NYC EDs with PB voters") + 
  theme_minimal() + facet_wrap(~type) +theme(legend.position = "bottom")
ggsave("PBonly_turnout_scatter_county.pdf", width = 8, units = "in")

turnout %>%
  filter(County %in% c("kings", "richmond", "queens", "new york", "bronx")) %>%
  ggplot(aes(x = as.factor(year), y = turnout)) +
  geom_boxplot(aes(color = as.factor(type)),alpha = 0.5)+ labs(color = "Turnout type", x = "Year", y = "Turnout", 
                                                               title = "Turnout in NYC EDs with PB voters") + 
  theme_minimal() + facet_wrap(~County) +theme(legend.position = "bottom")
ggsave("PBonly_turnout_boxplot_county.pdf", width = 10, height = 7, units = "in")



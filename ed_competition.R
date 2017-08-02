############################################################
###### Computing election district competitiveness    ######
###### Carolina Johnson                               ######
###### 7/10/2017                                      ######
############################################################

########################################################################################################################
### Prelims ###

require(RMySQL)
library(stringr)
library(dplyr)
library(tidyr)
library(ggplot2)

source("credentials.R") # loads the access credentials
source("dbDownload.R")

########################################################################################################################
### Loading and checking the data ###

ed <- ed_orig <- dbDownload(table = "electiondistricts", username = username, password = password, dbname = db.name, host = hostname, port = port)
results <- results_orig <-  dbDownload(table = "electionresults", username = username, password = password, dbname = db.name, host = hostname, port = port)
rm(password, username, hostname, db.name, port) # if you want to remove the credentials from your environment 

## creating unique election-race id
results <- results %>% mutate(elec_id = paste(ED, ElectionYear, ElectionMonth, ElectionDay, RaceName, sep = "-"))

########################################################################################################################
### Filtering to max-voter race ("top ticket" races) to use as competitiveness measure ###

max_race <- results %>% group_by(ED, ElectionYear, ElectionMonth, ElectionDay, RaceName) %>%
  mutate(totvotes = sum(VoteCount, na.rm = TRUE)) %>%
  group_by(ED, ElectionYear, ElectionMonth, ElectionDay) %>%
  filter(totvotes == max(totvotes)) 


########################################################################################################################
### Calculating competitiveness, as margin of victory as percentage of total votes cast in that race in that ED ###

compet <- max_race %>% 
  group_by(ED, ElectionYear, ElectionMonth, ElectionDay, RaceName, Candidate) %>%
  summarize(VoteCount = sum(VoteCount, na.rm = TRUE),
            totvotes = max(totvotes),
            n_ballot_lines = n()) %>% 
  mutate(vote_pct = VoteCount/totvotes) %>%
  group_by(ED, ElectionYear, ElectionMonth, ElectionDay, RaceName) %>%
  arrange(ED, ElectionYear, ElectionMonth, ElectionDay, RaceName, desc(VoteCount)) %>% 
  mutate(runner_up_count = lead(VoteCount),
         vote_diff = VoteCount - runner_up_count,
         vote_diff_cum = VoteCount - sum(runner_up_count, na.rm = TRUE),
         vote_diff_pct = vote_diff/totvotes,
         vote_diff_cum_pct = vote_diff_cum/totvotes) %>% 
  slice(1) # This last line limits the results to only the winners, and their winning percentage (over last )
  
  hist(compet$vote_diff_pct)
  hist(compet$vote_diff_cum_pct)

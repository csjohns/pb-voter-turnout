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

source("credentials_comp.R") # loads the access credentials


########################################################################################################################
### Loading and checking the data ### Loading from the nyc competitiveness app at the moment
con <- dbConnect(dbDriver("PostgreSQL"), user = username, password = password, dbname = db.name, host = hostname, port = port) #establish connection to DB
results_orig <- dbGetQuery(con, "
select * from scraped_BOE 
where electionmonth IN (4,9,11)
and candidate NOT IN ('Absentee/Military', 'Emergency', 'Scattered', 'Yes', 'No', 'Absentee')
and office IN ('President', 'CD', 'SD', 'AD', 'US Senate', 'Governor', 'Lieutenant Governor', 'Mayor', 'Member of the City Council')
                      ")
# source("credentials.R") # loads the access credentials
# results <- results_orig <-  dbDownload(table = "electionresults", username = username, password = password, dbname = db.name, host = hostname, port = port)
dbDisconnect(con) #closes the connection to the database
rm(password, username, hostname, db.name, port) # if you want to remove the credentials from your environment 

# renaming in line with conventions from Sonya's DB and code below
results <- results_orig %>% 
  rename(Office = office, County = county, ED = ed, ElectionYear = electionyear, ElectionMonth = electionmonth,
         ElectionDay = electionday, DistrictNumber = districtnumber, Candidate = candidate, Party = party, VoteCount = votecount)

glimpse(results)


## creating unique election-race id
results <- results %>% mutate(elec_id = paste(ED, ElectionYear, ElectionMonth, ElectionDay, Office, sep = "-"))

########################################################################################################################
### Filtering to max-voter race ("top ticket" races) to use as competitiveness measure ###

max_race <- results %>% group_by(ED, ElectionYear, ElectionMonth, ElectionDay, Office) %>%
  mutate(ed_totvotes = sum(VoteCount, na.rm = TRUE)) %>%
  group_by(ED, ElectionYear, ElectionMonth, ElectionDay) %>% # filter(ED == "Ad 23 - Ed 001") %>% View
  mutate(max_vote = as.numeric(ed_totvotes == max(ed_totvotes))) %>% 
  select(-VoteCount, -Party) %>% distinct


########################################################################################################################
### Calculating competitiveness, as margin of victory as percentage of total votes cast in the most popular race in that ED
##    Margins of victory are calculated at the race-district level, so a generally competitive race 
##    will be considered competetive even if everyone in teh neighborhood votes the same way

compet <- max_race %>% 
  group_by(DistrictNumber, ElectionYear, ElectionMonth, ElectionDay, Office) %>% 
  mutate(dist_totvotes = sum(VoteCount, na.rm = TRUE)) %>% 
  group_by(DistrictNumber, ElectionYear, ElectionMonth, ElectionDay, Office, Candidate) %>%
  summarize(dist_cand_votes = sum(VoteCount, na.rm = TRUE),
            dist_totvotes = unique(dist_totvotes),
            n_ballot_lines = n()) %>% 
  mutate(vote_pct = dist_cand_votes/dist_totvotes) %>% # head(30) %>% View
  group_by() %>% 
  left_join(max_race %>% select(-VoteCount, -Party) %>% distinct, .) %>% #filter(ElectionYear == 2016) %>% arrange(ED, ElectionMonth, Office) %>%  head(100) %>% View
  filter(max_vote == 1) %>% #head(30) %>% View
  group_by(ED, ElectionYear, ElectionMonth, ElectionDay, Office) %>%
  arrange(ED, ElectionYear, ElectionMonth, ElectionDay, Office, desc(dist_cand_votes)) %>% 
  mutate(runner_up_count = lead(dist_cand_votes),
         vote_diff = dist_cand_votes - runner_up_count,
         vote_diff_cum = dist_cand_votes - sum(runner_up_count, na.rm = TRUE),
         vote_diff_pct = vote_diff/dist_totvotes,
         vote_diff_cum_pct = vote_diff_cum/dist_totvotes) %>% 
  mutate_at(vars(ends_with("_pct")), replace_na, 1) %>% 
  slice(1) # This last line limits the results to only the winners, and their winning percentage (over last )

hist(compet$vote_diff_pct)
hist(compet$vote_diff_cum_pct)
ggplot(compet) + 
  geom_point(aes(x = vote_diff_pct, y = vote_diff_cum_pct, color = vote_diff_pct)) + 
  geom_abline(slope = 1, intercept = 0) +
  facet_wrap(~Office)
  
### For Reference: Old code doing all competitiveness scores by ED---------------------------------------------------
  
  ########################################################################################################################
  ### Filtering to max-voter race ("top ticket" races) to use as competitiveness measure ###
  
  max_race <- results %>% group_by(ED, ElectionYear, ElectionMonth, ElectionDay, Office) %>%
    mutate(totvotes = sum(VoteCount, na.rm = TRUE)) %>%
    group_by(ED, ElectionYear, ElectionMonth, ElectionDay) %>%
    filter(totvotes == max(totvotes)) 
  
  
  ########################################################################################################################
  ### Calculating competitiveness, as margin of victory as percentage of total votes cast in that race in that ED ###
  
  compet <- max_race %>% 
    group_by(ED, ElectionYear, ElectionMonth, ElectionDay, Office, Candidate) %>%
    summarize(VoteCount = sum(VoteCount, na.rm = TRUE),
              totvotes = max(totvotes),
              n_ballot_lines = n()) %>% 
    mutate(vote_pct = VoteCount/totvotes) %>%
    group_by(ED, ElectionYear, ElectionMonth, ElectionDay, Office) %>%
    arrange(ED, ElectionYear, ElectionMonth, ElectionDay, Office, desc(VoteCount)) %>% 
    mutate(runner_up_count = lead(VoteCount),
           vote_diff = VoteCount - runner_up_count,
           vote_diff_cum = VoteCount - sum(runner_up_count, na.rm = TRUE),
           vote_diff_pct = vote_diff/totvotes,
           vote_diff_cum_pct = vote_diff_cum/totvotes) %>% 
    slice(1) # This last line limits the results to only the winners, and their winning percentage (over last )
  
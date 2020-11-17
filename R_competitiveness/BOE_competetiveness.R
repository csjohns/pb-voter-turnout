##############################################################################################################################
###
### NYC PB Voters Project 
### Calculating Competetiveness at the political district level (as opposed to ED as in earlier scripts)
### Carolina Johnson
### 02/08/2020
###
### Calculate competitiveness for 4 different types of data:
### 1) Citywide PDFs
### 2) District-level PDFs
### 3) Citywide CSVs
### 4) District-level CSVS
### 
###
##############################################################################################################################

### Load libraries ---------------
library(dplyr)
library(tidyr)
library(stringr)

## Reference dependent processes (these save .rds files and should not be run every time)

# source("R_competitiveness/BOE_pdf_scraper_citywide.R") # returns clean results from citywide pdfs
# source("R_competitiveness/BOE_pdf_scraper.R") # returns clean results from district level pdfs

## Competitiveness function

filter_top_primary <- function(d) {
  d %>% 
    group_by(office, district, year, election, date) %>% 
    filter(totalvotes == max(totalvotes) | !str_detect(election, "Primary"))
}


calc_compet <- function(results) {
  results %>% 
    distinct() %>% # remove lingering duplicates
    filter_top_primary() %>% 
    group_by(candidate, office, district, party, year, election) %>% 
    # calculate how many votes the candidate received, the total votes cast in the race, and the number of ballot lines that candidate ran on:
    summarize(dist_cand_votes = sum(votes, na.rm = TRUE),
              dist_totvotes = unique(totalvotes),
              n_ballot_lines = n()) %>% 
    # calculate candidates vote percent of all votes cast in election:
    mutate(vote_pct = dist_cand_votes/dist_totvotes) %>%  
    # group by race and calculate candidate's relative results:
    group_by(office, district, party, year, election) %>% 
    arrange(desc(vote_pct)) %>% 
    mutate(n_candidates = n(),
           vote_margin_all = dist_cand_votes - (dist_totvotes - dist_cand_votes),
           vote_margin_pct_all = vote_margin_all/dist_totvotes,
           next_up = lead(dist_cand_votes, 1),
           vote_margin_next = dist_cand_votes - next_up,
           vote_margin_pct_next = vote_margin_next/dist_totvotes) %>%
    # filter to only winner:
    slice(1) %>% 
    # replace margin details for uncontested races for comparability:
    mutate(next_up = ifelse(n_candidates == 1 & is.na(next_up), 0, next_up),
           vote_margin_next = ifelse(n_candidates == 1 & is.na(vote_margin_next), dist_cand_votes, vote_margin_next),
           vote_margin_pct_next = ifelse(n_candidates == 1 & is.na(vote_margin_pct_next), 1.0, vote_margin_pct_next))
}


### District PDF results
res_pdf_district <- readRDS("data/cleaned_R_results/res_pdf_district.RDS")
test <- res_pdf_district %>% 
  rename(district = districtnumber) %>% 
  calc_compet()
summary(test)
test %>%  group_by(office, district, party, year, election) %>%   arrange(year, election, office, party, district, desc(vote_pct))  %>% View()

hist(test$vote_margin_pct_all)
test %>% filter(vote_margin_pct_all < 0) %>% View ## confirming all the negative margins are for races with many candidates (ie plurality)

hist(test$vote_margin_pct_next)

## QA: one record with bad winning pct - led to the distinct() in top of funcion call
# test %>% filter(vote_margin_pct_all > 1) %>% glimpse()
# res_pdf_district %>% filter(office == "AD" & districtnumber == "51" & year == "2012" & election == "General Election")
# raw_boe <- readRDS("R_competitiveness/boe_dist_pdf_all.Rds")
# raw2012 <- raw_boe[[4]]
# raw2012 %>% filter(str_detect(office, "Assembly") & str_detect(district, "51") & election == "General Election") %>% 
#   select(-url) 

### City-wide PDF results -----------------------------------------------------------
res_pdf_city <- readRDS("data/cleaned_R_results/res_pdf_city.RDS")
test_city <- res_pdf_city %>% 
  mutate(district = NA) %>% 
  calc_compet()
glimpse(test_city)
summary(test_city)

hist(test_city$vote_margin_pct_all)
hist(test_city$vote_margin_pct_next)


### District ED results
res_csv_district <- readRDS("data/cleaned_R_results/res_csv_district.RDS")
test_csv <- res_csv_district %>% 
  calc_compet()
summary(test_csv)

hist(test_csv$vote_margin_pct_all)
hist(test_csv$vote_margin_pct_next)

table(is.na(test$district))
table(is.na(test_city$district))
table(is.na(test_csv$district))
# districts are all present as they should be

class(test_csv$district)
class(test$district)
table(is.na(as.numeric(test$district)))
test$district <- as.numeric(test$district)
test_city$district <- -1 

all_comp <- bind_rows(test, test_csv, test_city)

all_comp <- all_comp %>% 
  filter(! year %in% c("2008", "2012", "2015", "2016") | (year %in% c("2008", "2012", "2016") & election %in% c("Primary Election 2008",
                                                                                                                "Primary Election 2016",
                                                                                                                "Primary Election")))
saveRDS(all_comp, "data/cleaned_R_results/competitiveness.RDS")
  
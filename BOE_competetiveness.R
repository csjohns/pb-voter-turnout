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

# source(BOE_pdf_scraper_citywide.R) # returns clean results from citywide pdfs
# source(BOE_pdf_scraper.R) # returns clean results from district level pdfs

## Competitiveness function

calc_compet <- function(results) {
  results %>% 
    group_by(candidate, office, district, party, year, election) %>% 
    summarize(dist_cand_votes = sum(votes, na.rm = TRUE),
              dist_totvotes = unique(totalvotes),
              n_ballot_lines = n()) %>% 
    mutate(vote_pct = dist_cand_votes/dist_totvotes) %>% 
    group_by(office, district, party, year, election) %>% 
    arrange(desc(vote_pct)) %>% 
    mutate(vote_margin_all = dist_cand_votes - (dist_totvotes - dist_cand_votes),
           vote_margin_pct_all = vote_margin_all/dist_totvotes,
           next_up = lead(dist_cand_votes, 1),
           vote_margin_next = dist_cand_votes - next_up,
           vote_margin_pct_next = vote_margin_next/dist_totvotes) %>% 
    slice(1)
}

### NEED TO REVIEW THE LOGIC OF THIS COMPETITIVENESS FUNCTION, BUT IT CURRENTLY SEEMS TO BE WORKINGish?
### NOT SURE WHAT'S UP WITH THE SLICING....

### District PDF results
test <- res_comb %>% 
  calc_compet()

### City-wide PDF results -----------------------------------------------------------
res_pdf_city <- readRDS("data/cleaned_R_results/res_pdf_city.RDS")
res_compet <- lapply(res_pdf_city, calc_compet) %>% bind_rows()
View(res_compet)

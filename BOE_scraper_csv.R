##############################################################################################################################
###
### NYC PB Voters Project 
### Scraping all sub-city district level election results from BOE ED-level CSVs - (2014-2017)
### Carolina Johnson
### 02/11/2020
###
### Iteratively download and extract info from all relevant BOE csv links separately (in list) for years 2008-2013
###
##############################################################################################################################

library(tabulizer)
library(rvest)
library(dplyr)
library(tidyr)
library(stringr)

# load custom helper functions
source("BOE_scraper_funs.R")


## download BO results pages for each election of interest (with csvs): ----
urls <- c(r2014 = "http://www.vote.nyc.ny.us/html/results/2014.shtml", 
          r2015 = "http://www.vote.nyc.ny.us/html/results/2015.shtml",
          r2016 = "http://www.vote.nyc.ny.us/html/results/2016.shtml",
          r2017 = "http://www.vote.nyc.ny.us/html/results/2017.shtml")

res_all <- vector("list", length = length(urls))
names(res_all) <- urls

for (u in urls) {
  # scrape and cleanup wayward header/extra cols)
  raw <- scrape_csv(u)
  
    filtered <- lapply(raw, total_ed_votes) # remove administrative lines (not candidate votes)
  cleaned <- bind_rows(filtered) %>% 
    clean_ed_results() # clean and format appropriately
  res_all[[u]] <- aggregate_eds(cleaned) # combine EDs to form district-level results
}

combined <- bind_rows(res_all)
glimpse(combined)

# save file for use
saveRDS(combined, "data/cleaned_R_results/res_csv_district.RDS")




### testing code
# test <- scrape_csv(urls["r2017"])
# 
# test[[1]] %>% glimpse()
# test[[1]] %>% total_ed_votes() %>% 
#   glimpse()
# test2 <- test[[1]] %>% total_ed_votes() 
# table(test2$candidate) %>% sort(decreasing = T) 
# 
# tt <- lapply(test, total_ed_votes)
# testall <- bind_rows(tt)
# glimpse(testall)
# 
# testall <- testall %>% clean_ed_results() 
# 
# testcomb  <- testall %>% aggregate_eds()



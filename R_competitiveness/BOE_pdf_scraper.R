##############################################################################################################################
###
### NYC PB Voters Project 
### Scraping all sub-city district level election results from BOE PDFS - (2008-2013)
### Carolina Johnson
### 01/20/2020
###
### Iteratively download and extract info from all relevant BOE pdf links separately (in list) for years 2008-2013
### 1) Extract data from PDF link
### 2) Extract metadata
### 3) Limit to pages after total header
### 4) Clean and combine all separate race results to on dataframe per year
### 5) Inspect, manually intervene in DQ issues
### 6) Filter to only crossover in cases of multiple crossover sub-result pages
### 7) Iteratively filter to only relevant records, clean election/district names
### 
###
##############################################################################################################################


library(tabulizer)
library(rvest)
library(dplyr)
library(tidyr)
library(stringr)

# load custom helper functions
source("R_competitiveness/BOE_scraper_funs.R")

# demo/area extraction code
# tab <- extract_tables('https://www.vote.nyc.ny.us/downloads/pdf/results/2010/General/20.7NewYork64AssemblyRecap.pdf', pages = 2, columns = 2 )
# head(tab)
# 
# extract_areas('https://www.vote.nyc.ny.us/downloads/pdf/results/2010/General/20.7NewYork64AssemblyRecap.pdf', pages = 2)
# locate_areas(links_rel[p])
# 
# tab <- extract_tables(links_rel[p],
#                       guess = F,
#                       area = list(c(243.84365,54.45601,475.70901,596.90121),
#                       c(79.57003,46.75569,392.71661,485.67428),
#                       c(82.99240,51.03364,736.66450,482.25191),
#                       c(77.85885,50.17805,354.21498,491.66342),
#                       c(81.28122, 47.61128,732.38654,492.51901 ),
#                       c( 80.42562,45.90009,357.63735,483.96309 )))


## download BO results pages for each election of interest: ----
urls <- c(r2008 = "http://www.vote.nyc.ny.us/html/results/2008.shtml", 
          r2009 = "http://www.vote.nyc.ny.us/html/results/2009.shtml",
          r2010 = "http://www.vote.nyc.ny.us/html/results/2010.shtml",
          r2012 = "http://www.vote.nyc.ny.us/html/results/2012.shtml",
          r2013 = "http://www.vote.nyc.ny.us/html/results/2013.shtml")
          
res_all <- vector("list", length = length(urls))
names(res_all) <- urls

alllinks <- vector("list", 5)
names(alllinks) <- urls
# rawlinks <- vector("list", 5)
# names(rawlinks) <- urls

errors <- vector("character")

for (u in urls) {
  year <- names(which(urls == u))
  
  #read html
  res <- read_html(u)
  # extract links
  links <- res %>% 
    html_nodes("a") %>% html_attr("href")
  
  # filter to relevant links and cleanup
  # links_ed <- links[str_detect(links, "EDLevel\\.csv$")] 
  links_rel <- links[str_detect(tolower(links), url_include_string)] ##have visually inspected remainder to ensure all are covered here.
  links_rel <- links_rel[!str_detect(tolower(links_rel), url_exclude_string)] # remove US senators that snuck in from spaces
  links_rel <- str_replace(links_rel, "^../..", "https://vote.nyc.ny.us")
  links_rel[!str_detect(links_rel, "^https://vote.nyc.ny.us")] <- paste0("https://vote.nyc.ny.us", links_rel[!str_detect(links_rel, "^https://vote.nyc.ny.us")])
  alllinks[[u]] <- links_rel
  # rawlinks[[u]] <- links
  
  res_year <- vector("list", length = length(links_rel))
  names(res_year) <- str_split(links_rel,  "20\\d\\d/", simplify = T)[,2]
  
  
  for (p in seq_along(links_rel)){
    #extract tables for each pdf p
    res_year[[p]] <- extract_tables(links_rel[p], 
                                    guess = F, 
                                    area = list(c(70, 40, 747, 493)))
    if (any(sapply(res_year[[p]], function(x){nrow(x) > 1}))){
      #extract metadata
      res_year[[p]][[1]] <- as.matrix(res_year[[p]][[1]][res_year[[p]][[1]][,1] != "", 1])
      election <- str_split(res_year[[p]][[1]][3,1], " - ", simplify = TRUE)[[1,1]]
      date <- str_split(res_year[[p]][[1]][3,1], " - ", simplify = TRUE)[[1,2]]
      scope <- res_year[[p]][[1]][4,1]
      office <-  res_year[[p]][[1]][5,1]
      district <- res_year[[p]][[1]][6,1]
      # for each page table extracted, identify if total count, and split collapsed columns back out
      total <- 0
      for(r in seq_along(res_year[[p]])) {
        total <- total + str_detect(tolower(res_year[[p]][[r]][1,1]), "total")
        if (total == 0) {
          res_year[[p]][[r]] <- "partial"
        } else if (total > 0) {
          res_year[[p]][[r]] <- split_vote_cols(res_year[[p]][[r]])
          res_year[[p]][[r]] <- as.data.frame(res_year[[p]][[r]], stringsAsFactors = F)
          names(res_year[[p]][[r]]) <- c("group", "votes")
        }
      }
      #drop partial results (prior to total)
      res_year[[p]] <- lapply(res_year[[p]], drop_partial)
      # bind rows and remove non-count rows
      res_year[[p]] <- collapse_clean_tabs(res_year[[p]])
      # add metadata to dataframe
      res_year[[p]]$election <- election
      res_year[[p]]$date <- date
      res_year[[p]]$scope <- scope
      res_year[[p]]$office <- office
      res_year[[p]]$district <- district
      res_year[[p]]$url <- links_rel[p]
    } else {
      errors <- c(errors, links_rel[p])
      res_year[[p]] <- NULL
    }
  }
  #collapse all elections that year into one dataframe in the combined results
  res_all[[u]] <- bind_rows(res_year) %>% 
    mutate(year = str_remove(year, "r")) 
}


## checking all results included/errored
allinks <- unlist(alllinks)
reslinks <- lapply(res_all, function(x){x$url}) %>% unlist()
setdiff(allinks, reslinks) == errors

sapply(alllinks, n_distinct)
sapply(res_all, function(x){n_distinct(x$url)})
# the missing records match the errors: Good
errors
# errors[3,5:7] are irrelevant and not worth scraping.

### Data archive/load/backup -----------------------------------------------------------------------------------------------
res_all_backup <- res_all

# saving backup of downloaded raw results
# res_all_backup <- readRDS("R_competitiveness/boe_dist_pdf_all.Rds")
# saveRDS(res_all_backup, "R_competitiveness/boe_dist_pdf_all.Rds")


# loading backup
res_all <- res_all_backup


### Attaching manual data -------------------------------------------------------------
res_manual <- readRDS("R_competitiveness/BOE_manual.rds")
for (i in seq_along(res_manual)) {
  res_manual[[i]]$year <- names(res_manual)[i]
}

## bind manual results to the respective years dataframes
# res_all[["http://www.vote.nyc.ny.us/html/results/2010.shtml"]] <- 
#   res_all[["http://www.vote.nyc.ny.us/html/results/2010.shtml"]] %>% 
#   bind_rows(res_manual[["2010"]] )
# excluding 2010 results 

res_all[["http://www.vote.nyc.ny.us/html/results/2013.shtml"]] <- 
  res_all[["http://www.vote.nyc.ny.us/html/results/2013.shtml"]] %>% 
  bind_rows(res_manual[["2013"]] )


### Cleaning and normalizing formats -----------------------------------------------------------------------------------------------

for (d in seq_along(res_all)) {
  res_all[[d]] <- res_all[[d]] %>% 
    filter_ballot_counts()
  
  res_all[[d]] <- res_all[[d]] %>% 
    group_by(election,date,scope,office,district,year) %>% 
    mutate(totalvotes1 = ifelse(str_detect(toupper(group), "TOTAL VOTES"), votes, NA),
           totalvotes = na.omit(unique(totalvotes1))) %>% 
    ungroup() %>% 
    filter(!str_detect(toupper(res_all[[d]]$group), "TOTAL VOTES")) %>% 
    select(-totalvotes1)
}

## cleaning up candidate/party info
res_all <- lapply(res_all, clean_candidates)
res_all <- lapply(res_all, split_scope)

## confirming crossovers are accurate sums of separate borough results
lapply(res_all, test_crossover_inclusive)

# manual investigation ---------------
res_all[[4]] %>% 
  filter(election == "General Election" & 
           str_detect(office, "State Senator") & 
           district == "26th Senatorial District") %>%
  View()
# manually confirmed against BOE - crossover is good
# res_all[[3]] %>% 
res_all[[3]] %>% 
  filter(election == "Primary Election 2010" & 
         str_detect(office, "Republican State Senator") & 
         district == "28th Senatorial District") %>%
  View()

## ignore the weird 2012 7th congr & 26th senatorial results - they are contractory on BOE site, keep crossover only
## something weird going on - general 13th crossover in twice??  richmond and xover had adjusted results (2 additional votes)
##   It's too small to change the final percents (less than 0.01% change), so removing manual entries to save reformatting trouble

### Filter out crossover redundancies ------------------------------------------------------
filter_xover <- function(results) {
  xovertest <- results %>% 
    group_by(election, party, date, office, district, year) %>% 
    mutate(crossover = check_xover(county)) %>% 
    mutate(isxover = str_detect(tolower(county), "crossover")) %>% 
    filter(!crossover | isxover) %>% 
    select(-crossover, -isxover) %>% 
    ungroup()
  xovertest
}

res_filtered <- lapply(res_all, filter_xover)

### Check and filter to target value set ------------------------------------------------------
## helper functions

check_vals <- function(d) {
  d %>% select(election, date) %>% distinct()
}

check_dists <- function(d) {table(d$district) %>% as.matrix()}
check_office <- function(d){table(d$office) %>% as.matrix()}


## Inspect election/date values
lapply(res_filtered, check_vals)

# fix the 2010 special election on primary day to collapse into primary (so not lost when filter out special)
res_filtered[[2]] <- res_filtered[[2]] %>% 
  mutate(election = ifelse(date == '09/15/2009' & ! stringr::str_detect(election, "Election"), "Primary Special 2009", election))

# remove special elections
res_filtered <- lapply(res_filtered, filter_electiontype)

## check districts/office values
lapply(res_filtered, check_dists)
lapply(res_filtered, check_office)
lapply(res_filtered, function(d){table(d$office, d$election)})

# filter to only relevant offices  ****NEED TO WRITE FUNCTION STILL****
res_filtered <- lapply(res_filtered, filter_office)

### Scope, Party, Office mannagement --------------------------------------------------------------------------------------------
res_comb <- bind_rows(res_filtered)

# cleaning up district & office
# any(str_detect(res_comb$district, "\\d{3}")) # checking no 3 digit districts
# checked crosstabs to original to confirm recording structure correct
res_comb <- res_comb %>% 
  mutate(districtnumber = str_extract(district, "\\d\\d?")) 
res_comb <- res_comb %>% 
  mutate(office = case_when(str_detect(office, "State Senator") ~ "SD",
                            str_detect(office, "Member of the Assembly") ~ "AD",
                            str_detect(office, "City Council") ~ "Member of the City Council",
                            str_detect(office, "Congress") ~ "Representative in Congress",
                            str_detect(office, "Member of the Assembly") ~ "AD",
                            TRUE ~ office))
  
glimpse(res_comb)

res_comb %>% select(-district, -url) %>% saveRDS("data/cleaned_R_results/res_pdf_district.RDS")

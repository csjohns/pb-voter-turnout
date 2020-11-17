##############################################################################################################################
###
### NYC PB Voters Project 
### Scraping all citywide district level election results from BOE PDFS - (2008-2013)
### Carolina Johnson
### 01/20/2020
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
## pre-filtering to only keep dem primary
urls <- tribble(~elections, ~date, ~urls, ~pages,
                "mayor_2009_prim", "09/15/2009", "https://www.vote.nyc.ny.us/downloads/pdf/results/2009/Primary/1.1CrossoverDemMayorRecap.pdf", "7:10",
                "mayor_2009_gen", "11/03/2009", "https://www.vote.nyc.ny.us/downloads/pdf/results/2009/General/1.11CitywideMayorRecap.pdf", "10:15",
                "senate_2010_prim", "09/14/2010", "https://www.vote.nyc.ny.us/downloads/pdf/results/2009/Primary/1.1CrossoverDemMayorRecap.pdf", "7:10",
                "senate_2010_gen", "11/02/2010", "https://www.vote.nyc.ny.us/downloads/pdf/results/2010/General/4.1CitywideUSSenateFullTermRecap.pdf", "11:17",
                "ag_2010_prim", "09/14/2010", "https://www.vote.nyc.ny.us/downloads/pdf/results/2010/Primary/1.1CitywideDemCrossoverAttorneyGeneralRecap.pdf", "12:19",
                "ag_2010_gen", "11/02/2010", "https://www.vote.nyc.ny.us/downloads/pdf/results/2010/General/3.1CitywideAttorneyGeneralRecap.pdf", "11:16",
                "gov_2010_gen", "11/02/2010", "https://www.vote.nyc.ny.us/downloads/pdf/results/2010/General/1.1CitywideGovernorRecap.pdf", "17:27",
                "mayor_2013_prim", "09/10/2013", "https://www.vote.nyc.ny.us/downloads/pdf/results/2013/2013SeptemberPrimaryElection/01011000000Citywide%20Democratic%20Mayor%20Citywide%20Recap.pdf", "8:10",
                "mayor_2013_gen", "11/05/2013", "https://www.vote.nyc.ny.us/downloads/pdf/results/2013/2013GeneralElection/00001100000Citywide%20Mayor%20Citywide%20Recap.pdf" , "21:33",
                "mayor_2017_gen", "11/07/2017", "https://vote.nyc.ny.us/downloads/pdf/election_results/2017/20171107General%20Election/00001100000Citywide%20Mayor%20Citywide%20Recap.pdf", "56:101",
                "mayor_2017_prim", "09/12/2017", "https://vote.nyc.ny.us/downloads/pdf/election_results/2017/20170912Primary%20Election/01001100000Citywide%20Democratic%20Mayor%20Citywide%20Recap.pdf", "25:43",
                "gov_2014_gen", "11/04/2014", "https://vote.nyc.ny.us/downloads/pdf/election_results/2014/20141104General%20Election/00000500000Citywide%20Governor%20Lieutenant%20Governor%20Citywide%20Recap.pdf", "26:44",
                "gov_2014_prim", "09/09/2014", "https://vote.nyc.ny.us/downloads/pdf/election_results/2014/20140909Primary%20Election/01000400000Citywide%20Democratic%20Governor%20Citywide%20Recap.pdf", "13:19",
                "ag_2014_gen", "11/04/2018", "https://vote.nyc.ny.us/downloads/pdf/election_results/2014/20141104General%20Election/00000800000Citywide%20Attorney%20General%20Citywide%20Recap.pdf", "12:18")

# for (u in 1:nrow(urls)) {
#   res_el <- extract_areas(urls$urls[u], pages = eval(parse(text = urls$pages[u])))
#   print(res_el)
#   
# }
res_all <- vector("list", length = nrow(urls))
names(res_all) <- urls$elections

errors <- vector("character")

## download and process each separate elections' file
for (p in seq_along(urls$urls)) {
  
    #extract tables for each pdf p
    res_all[[p]] <- extract_tables(urls$urls[p], 
                                    guess = F, 
                                    pages = eval(parse(text = urls$pages[p])),
                                    area = list(c(70, 40, 747, 493)))
    if (any(sapply(res_all[[p]], function(x){nrow(x) > 1}))){
      
      for(r in seq_along(res_all[[p]])) {
          res_all[[p]][[r]] <- split_vote_cols(res_all[[p]][[r]])
          res_all[[p]][[r]] <- as.data.frame(res_all[[p]][[r]], stringsAsFactors = F)
          names(res_all[[p]][[r]]) <- c("group", "votes")
      }
      
      #drop partial results (prior to total)
      res_all[[p]] <- lapply(res_all[[p]], drop_partial)
      # bind rows and remove non-count rows
      res_all[[p]] <- collapse_clean_tabs(res_all[[p]])
      # add metadata to dataframe
      } else {
      errors <- c(errors, links_rel[p])
      res_all[[p]] <- NULL
    }
  }
res_all_backup <- res_all


res_all <- res_all_backup
## clean up votes - label, exclude write in and ballot classifiers, convert total votes to columnn and cut from rows
for (d in seq_along(res_all)) {
  res_all[[d]]$election <- urls$elections[d]
  res_all[[d]]$date <- urls$date[d]
  
  res_all[[d]] <- res_all[[d]] %>% 
    separate(election, into = c("office", "year", "election"), sep = "_") %>%
    mutate(office = recode(office, "ag" = "Attorney General", "mayor" = "Mayor", "senate" = "US Senator", "gov" = "Governor"),
           election = recode(election, "prim" = "Primary Election", "gen" = "General Election")) %>%
    filter(!str_detect(toupper(group), "WRITE-IN|EMERGENCY|AFFIDAVIT|ABSENTEE|PUBLIC COUNTER|UNRECORDED|TOTAL BALLOTS|FEDERAL|SPECIAL PRESIDENTIAL|APPLICABLE BALLOTS|THEREOF"))
  
  res_all[[d]]$totalvotes = unique(res_all[[d]]$votes[str_detect(toupper(res_all[[d]]$group), "TOTAL VOTES")])
  res_all[[d]] <- res_all[[d]] %>% 
    filter(!str_detect(toupper(res_all[[d]]$group), "TOTAL VOTES"))
}

res_all <- lapply(res_all, clean_candidates)

# combine into one df
res_city <- bind_rows(res_all)

# add party = democratic party for all primary elections
res_city <- res_city %>% 
  mutate(party = if_else(election == "Primary Election", "Democratic Party", NA_character_))

glimpse(res_city)
res_city %>% saveRDS("data/cleaned_R_results/res_pdf_city.RDS")
        
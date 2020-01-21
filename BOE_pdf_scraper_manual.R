library(tabulizer)
library(rvest)
library(dplyr)
library(tidyr)
library(stringr)


split_vote_cols <- function(df){
  splitpat <- "(.* )([^\\s]*$)"
  if (ncol(df) == 1) {
    splits <- str_match(df, splitpat)[,2:3]
    df <- splits
  } else if (!any(df[2:5, 2]!="")){
    df <- df[,1] 
    splits <- str_match(df, splitpat)[,2:3]
    df <- splits
  }
  df
}

drop_partial <- function(x){
  if (!is.data.frame(x) && x == "partial") {
    x <- NULL 
  }
  x
}

collapse_clean_tabs <- function(reslist) {
  out <- bind_rows(reslist)
  out <- out %>% 
    mutate(votes = as.numeric(str_remove_all(votes, ","))) %>% 
    filter(!is.na(votes))
  out
}

# tab <- extract_tables('https://www.vote.nyc.ny.us/downloads/pdf/results/2010/General/20.7NewYork64AssemblyRecap.pdf', pages = 2, columns = 2 )
# head(tab)
# 


extract_areas('https://www.vote.nyc.ny.us/downloads/pdf/results/2010/General/20.7NewYork64AssemblyRecap.pdf', pages = 2)
# locate_areas(links_rel[p])

tab <- extract_tables(links_rel[p],
                      guess = F,
                      area = list(c(243.84365,54.45601,475.70901,596.90121),
                      c(79.57003,46.75569,392.71661,485.67428),
                      c(82.99240,51.03364,736.66450,482.25191),
                      c(77.85885,50.17805,354.21498,491.66342),
                      c(81.28122, 47.61128,732.38654,492.51901 ),
                      c( 80.42562,45.90009,357.63735,483.96309 )))

## download BO results pages for each election of interest: ----
urls <- tribble(~elections, ~urls, ~pages,
                "mayor_2009_prim", "https://www.vote.nyc.ny.us/downloads/pdf/results/2009/Primary/1.1CrossoverDemMayorRecap.pdf", "7:10",
                "mayor_2009_gen",  "https://www.vote.nyc.ny.us/downloads/pdf/results/2009/General/1.11CitywideMayorRecap.pdf", "10:15",
                "senate_2010_prim", "https://www.vote.nyc.ny.us/downloads/pdf/results/2009/Primary/1.1CrossoverDemMayorRecap.pdf", "7:10",
                "senate_2010_gen", "https://www.vote.nyc.ny.us/downloads/pdf/results/2010/General/4.1CitywideUSSenateFullTermRecap.pdf", "11:17",
                "ag_2010_prim", "https://www.vote.nyc.ny.us/downloads/pdf/results/2010/Primary/1.1CitywideDemCrossoverAttorneyGeneralRecap.pdf", "12:19",
                "ag_2010_gen", "https://www.vote.nyc.ny.us/downloads/pdf/results/2010/General/3.1CitywideAttorneyGeneralRecap.pdf", "11:16",
                "gov_2010_gen","https://www.vote.nyc.ny.us/downloads/pdf/results/2010/General/1.1CitywideGovernorRecap.pdf", "17:27",
                "mayor_2013_prim", "https://www.vote.nyc.ny.us/downloads/pdf/results/2013/2013SeptemberPrimaryElection/01011000000Citywide%20Democratic%20Mayor%20Citywide%20Recap.pdf", "8:10",
                "mayor_2013_gen", "https://www.vote.nyc.ny.us/downloads/pdf/results/2013/2013GeneralElection/00001100000Citywide%20Mayor%20Citywide%20Recap.pdf" , "21:33")

res_all <- vector("list", length = nrow(urls))
names(res_all) <- urls$elections

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
## clean up votes - label, exclude write in and ballot classifiers, convert total votes to columnn and cut from rows
for (d in seq_along(res_all)) {
  res_all[[d]]$election <- urls$elections[d]
  
  res_all[[d]] <- res_all[[d]] %>% 
    separate(election, into = c("office", "year", "election"), sep = "_") %>%
    mutate(office = recode(office, "ag" = "attorney general"),
           election = recode(election, "prim" = "primary", "gen" = "general")) %>%
    filter(!str_detect(toupper(group), "WRITE-IN|EMERGENCY|AFFIDAVIT|ABSENTEE|PUBLIC COUNTER|UNRECORDED|TOTAL BALLOTS|FEDERAL|SPECIAL PRESIDENTIAL|APPLICABLE BALLOTS"))
  
  res_all[[d]]$totalvotes = unique(res_all[[d]]$votes[str_detect(toupper(res_all[[d]]$group), "TOTAL VOTES")])
  res_all[[d]] <- res_all[[d]] %>% 
    filter(!str_detect(toupper(res_all[[d]]$group), "TOTAL VOTES"))
}

clean_candidates <- function(df) {
  df %>% separate(group, c("candidate", "party"), sep = " \\(") %>%
    mutate(party = str_replace(party, "\\)", "")) %>%
    mutate_at(vars(candidate, party), str_trim) 
}

calc_compet <- function(results) {
  results %>% 
    group_by(candidate, office, year, election) %>% 
    summarize(dist_cand_votes = sum(votes, na.rm = TRUE),
              dist_totvotes = unique(totalvotes),
              n_ballot_lines = n()) %>% 
    mutate(vote_pct = dist_cand_votes/dist_totvotes) %>% 
    ungroup() %>% 
    arrange(desc(vote_pct)) %>% 
    mutate(vote_margin_all = dist_cand_votes - (dist_totvotes - dist_cand_votes),
           vote_margin_pct_all = vote_margin_all/dist_totvotes,
           next_up = lead(dist_cand_votes, 1),
           vote_margin_next = dist_cand_votes - next_up,
           vote_margin_pct_next = vote_margin_next/dist_totvotes) %>% 
    slice(1)
}

res_all <- lapply(res_all, clean_candidates)

res_compet <- lapply(res_all, calc_compet) %>% bind_rows()
View(res_compet
     )

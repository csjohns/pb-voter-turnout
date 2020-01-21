library(tabulizer)

# tab <- extract_tables('https://www.vote.nyc.ny.us/downloads/pdf/results/2010/General/20.7NewYork64AssemblyRecap.pdf', pages = 2, columns = 2 )
# head(tab)
# 
# extract_areas('https://www.vote.nyc.ny.us/downloads/pdf/results/2010/General/20.7NewYork64AssemblyRecap.pdf', pages = 2)
# locate_areas(links_rel[p])

tab <- extract_tables(links_rel[p],
                      guess = F,
                      area = list(c(243.84365,54.45601,475.70901,596.90121),
                      c(79.57003,46.75569,392.71661,485.67428),
                      c(82.99240,51.03364,736.66450,482.25191),
                      c(77.85885,50.17805,354.21498,491.66342),
                      c(81.28122, 47.61128,732.38654,492.51901 ),
                      c( 80.42562,45.90009,357.63735,483.96309 )))

### scrape BOE for electronic results for specified elections
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

## download BO results pages for each election of interest: ----
urls <- c(r2008 = "http://www.vote.nyc.ny.us/html/results/2008.shtml", 
          r2009 = "http://www.vote.nyc.ny.us/html/results/2009.shtml",
          r2010 = "http://www.vote.nyc.ny.us/html/results/2010.shtml",
          r2012 = "http://www.vote.nyc.ny.us/html/results/2012.shtml",
          r2013 = "http://www.vote.nyc.ny.us/html/results/2013.shtml")
          
res_all <- vector("list", length = length(urls))
names(res_all) <- urls

errors <- vector("character")

for (u in urls[4:5]) {
  year <- names(which(urls == u))
  
  #read html
  res <- read_html(u)
  # extract links
  links <- res %>% 
    html_nodes("a") %>% html_attr("href")
  
  # filter to relevant links and cleanup
  # links_ed <- links[str_detect(links, "EDLevel\\.csv$")] 
  links_rel <- links[str_detect(links, "Council|Congress|State Senat|Assembly")]
  links_rel <- str_replace(links_rel, "^../..", "https://vote.nyc.ny.us")
  links_rel[!str_detect(links_rel, "^https://vote.nyc.ny.us")] <- paste0("https://vote.nyc.ny.us", links_rel[!str_detect(links_rel, "^https://vote.nyc.ny.us")])
  
  res_year <- vector("list", length = length(links_rel))
  names(res_year) <- str_split(links_rel,  "20\\d\\d/", simplify = T)[,2]
  
  
  for (p in seq_along(links_rel)){
    #extract tables for each pdf p
    res_year[[p]] <- extract_tables(links_rel[p], 
                                    guess = F, 
                                    area = list(c(70, 40, 747, 493)))
    if (any(sapply(res_year[[p]], function(x){nrow(x) > 1}))){
      #extract metadata
      res_year[[p]][[1]] <- as.matrix(res_year[[p]][[1]][res_year[[p]][[1]] != ""])
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
    } else {
      errors <- c(errors, links_rel[p])
      res_year[[p]] <- NULL
    }
  }
  #collapse all elections that year into one dataframe in the combined results
  res_all[[u]] <- bind_rows(res_year) %>% 
    mutate(year = str_remove(year, "r")) 
}


###### START HERE! NOT HANDLING CROSSOVER RESULTS CORRECTLY - THE CROSSOVER RESULTS INCLUDE THE TOTAL OF BOTH AREAS. JUST USE CROSSOVER IF EXISTS.
res_all_backup <- res_all
res_all <- res_all_backup
### cleaning up
for (d in seq_along(res_all)) {
  res_all[[d]] <- res_all[[d]] %>% 
    filter(!str_detect(toupper(group), "WRITE-IN|EMERGENCY|AFFIDAVIT|ABSENTEE|PUBLIC COUNTER|UNRECORDED|TOTAL BALLOTS|FEDERAL|SPECIAL PRESIDENTIAL|APPLICABLE BALLOTS"))
  
  res_all[[d]] <- res_all[[d]] %>% 
    group_by(election,date,scope,office,district,year) %>% 
    mutate(totalvotes1 = ifelse(str_detect(toupper(group), "TOTAL VOTES"), votes, NA),
           totalvotes = na.omit(unique(totalvotes1))) %>% 
    ungroup() %>% 
    filter(!str_detect(toupper(res_all[[d]]$group), "TOTAL VOTES")) %>% 
    select(-totalvotes1)
}

clean_candidates <- function(df) {
  df %>% separate(group, c("candidate", "party"), sep = " \\(") %>%
    mutate(party = str_replace(party, "\\)", "")) %>%
    mutate_at(vars(candidate, party), str_trim) 
}

calc_compet <- function(results) {
  results %>% 
    group_by(election, date, office, district, year) %>% 
    mutate(totalvotes_allscope = sum(totalvotes)) %>% View()
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
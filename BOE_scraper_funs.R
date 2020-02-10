##############################################################################################################################
###
### NYC PB Voters Project 
### Helper functions for various BOE scraping tasks
### Carolina Johnson
### 02/08/2020
###
### 
##############################################################################################################################


### Functions for PDF scraping ----------------------------------------------------

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

clean_candidates <- function(df) {
  df %>% separate(group, c("candidate", "cand_party"), sep = " \\(") %>%
    mutate(cand_party = str_replace(cand_party, "\\)", "")) %>%
    mutate_at(vars(candidate, cand_party), str_trim) 
}

split_scope <- function(df) {
  # split scope into election geography and party (if any)
  df %>% 
    separate(scope, into = c("county", "party"), sep = " - ") %>% 
    mutate(party = ifelse(str_detect(party,"All"),NA,party))
}

check_xover <- function(x) {
  any(str_detect(tolower(x), "crossover"))
}

test_single_tot <- function(x) {
  n_distinct(x) == 1
}

test_crossover_inclusive <- function(results) {
  xovertest <- results %>% 
    group_by(election, date, office, district, year) %>% 
    mutate(crossover = check_xover(scope)) %>% #View()
    mutate(isxover = str_detect(tolower(scope), "crossover")) %>% 
    group_by(election, date, office, district, year, candidate, cand_party, isxover) %>% #grouping by all but scope in order to only sum across non-xover scopes
    mutate(totalvotes_xoversplit = sum(totalvotes)) #%>% View()
  xoversum <- xovertest %>% 
    group_by(election, date, office, district, year) %>% 
    summarize(allequal = test_single_tot(totalvotes_xoversplit)) 
  if (any(!xoversum$allequal)) {
    res <- xoversum %>% filter(!allequal)
  } else if (!any(!xoversum$allequal)) {
    res <- TRUE
  }
  res
}


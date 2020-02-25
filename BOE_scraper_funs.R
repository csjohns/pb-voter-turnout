##############################################################################################################################
###
### NYC PB Voters Project 
### Helper functions for various BOE scraping tasks
### Carolina Johnson
### 02/08/2020
###
### 
##############################################################################################################################

### Common functions/resources ------------------------------------------------------------------------

url_include_string <- "council|congress|cong\\d|state(%20| )?senat|senate(%20| )?\\d|\\d+senat|assembly"
url_exclude_string <- "us%20senat|states%20senat"
ballot_exclusions <- "WRITE-IN|EMERGENCY|AFFIDAVIT|ABSENTEE|PUBLIC COUNTER|UNRECORDED|TOTAL BALLOTS|FEDERAL|SPECIAL PRESIDENTIAL|APPLICABLE BALLOTS|BLANK|INVALID|OVERVOTED"


filter_ballot_counts <- function(df) {
  df %>% 
    filter(!str_detect(toupper(group), ballot_exclusions))
}

clean_candidates <- function(df) {
  df %>% separate(group, c("candidate", "cand_party"), sep = " \\(") %>%
    mutate(cand_party = str_replace(cand_party, "\\)", "")) %>%
    mutate_at(vars(candidate, cand_party), str_trim) 
}

filter_electiontype <- function(d) {
  filter(d, str_detect(tolower(election), "general") | str_detect(tolower(election), "primary"))
}


filter_office <- function(d) {
  filter(d, str_detect(tolower(office), "city council|mayor|senator|cong|member of the assembly"))
}


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
    mutate(crossover = check_xover(county)) %>% #View()
    mutate(isxover = str_detect(tolower(county), "crossover")) %>% 
    group_by(election, party, date, office, district, year, candidate, cand_party, isxover) %>% #grouping by all but scope in order to only sum across non-xover scopes
    mutate(totalvotes_xoversplit = sum(totalvotes)) #%>% View()
  xoversum <- xovertest %>% 
    group_by(election, party, date, office, district, year) %>% 
    summarize(allequal = test_single_tot(totalvotes_xoversplit)) 
  if (any(!xoversum$allequal)) {
    res <- xoversum %>% filter(!allequal)
  } else if (!any(!xoversum$allequal)) {
    res <- TRUE
  }
  res
}

### Functions for CSV ED level scraping-------------------------------
## function to scrape individual csv 
scrape_csv <- function(url) {
  res <- read_html(url)
  # extract links
  links <- res %>% 
    html_nodes("a") %>% html_attr("href")
  
  # filter to relevant links and cleanup
  links <- links[str_detect(links, "EDLevel\\.csv$")] 
  links_rel <- links[str_detect(tolower(links), url_include_string)] ##have visually inspected remainder to ensure all are covered here.
  links_rel <- links_rel[!str_detect(tolower(links_rel), url_exclude_string)] # remove US senators that snuck in from spaces
  links_rel <- str_replace(links_rel, "^../..", "https://vote.nyc.ny.us")
  links_rel[!str_detect(links_rel, "^https://vote.nyc.ny.us")] <- paste0("https://vote.nyc.ny.us", links_rel[!str_detect(links_rel, "^https://vote.nyc.ny.us")])
  
  res_year <- vector("list", length = length(links_rel))
  names(res_year) <- str_split(links_rel,  "20\\d\\d/", simplify = T)[,2]
  
  for (p in seq_along(links_rel)){
    raw <- try(read.csv(links_rel[p], as.is = T))
    # replace bad csvs with new pull, trim leading 11 cols and replace with header from first row
    if (ncol(raw) == 22) {
      raw <- read.csv(links_rel[p], as.is = T, header = FALSE)
      header <- raw[1,1:11]
      header <- str_replace_all(header, "/| ", ".")
      raw <- raw[, 12:22]
      names(raw) <- header
      
    } 
    raw$url <- links_rel[p]
    raw$year <- str_remove(names(which(urls == url)), "r") # bad form -depends on object (urls) expected in global environment :p
    res_year[[p]] <- raw
  }
  res_year
}


total_ed_votes <- function(df) {
  if(!is.numeric(df$Tally)) {
    df$Tally <- str_remove_all(df$Tally, ",")
    df$Tally <- as.numeric(df$Tally)
    message(paste0("File ", df$url, " has non-numeric Tally. Coercing to Numeric. ", sum(is.na(df$Tally)), " records coerced to NA"))
  }
  df %>% 
    filter(!str_detect(toupper(Unit.Name), ballot_exclusions)) %>% 
    group_by(AD, ED) %>% 
    mutate(totalvotes = sum(Tally)) %>% 
    filter(!str_detect(toupper(Unit.Name), "SCATTERED")) %>% 
    ungroup()
}


clean_ed_results <- function(df) {
  df %>% 
    rename(group = Unit.Name,
           district = District.Key,
           office = Office.Position.Title,
           votes = Tally,
           party = Party.Independent.Body, 
           county = County) %>% 
    mutate(ed  = paste(AD, str_pad(ED, width = 3, side = "left", pad = 0), sep = "-")) %>%
    separate(Event, c("election", "date"), sep = " - ") %>%
    clean_candidates() %>% 
    select(candidate, cand_party, votes, election, date, county, party, office, year, totalvotes, district, ed) %>% 
    distinct() # to remove eds duplicated by crossovers
}

aggregate_eds <- function(df){
  df %>% 
    distinct() %>% 
    group_by(candidate, cand_party, election, date, party, office, year, district) %>% # leaving out county as otherwise have to sum up again to => crossover
    summarize(votes = sum(votes),
              totalvotes = sum(totalvotes)) %>% 
    ungroup()
}
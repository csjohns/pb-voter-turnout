###
### scrape BOE for electronic ED results for specified elections (currently 2017)
###
## THIS NEEDS ADJUSTING TO ACCOUNT FOR THE ELECTION PARTY (PARTY/INDEPENDENT BODY)

library(rvest)
library(dplyr)
library(tidyr)

## install rvest
## download BO results pages for each election of interest:
url <- "http://www.vote.nyc.ny.us/html/results/2017.shtml"

# read html
r2017 <- read_html(url)

# extract links
links <- r2017 %>% 
  html_nodes("a") %>% html_attr("href")

# filter to relevant links and cleanup
links_ed <- links[str_detect(links, "EDLevel\\.csv$")] 
links_rel <- links_ed[str_detect(links_ed, "City Council|Mayor|Senator|Cong|Member of the Assembly") & !str_detect(links_ed, "[0-9]Citywide")]
links_rel <- str_replace(links_rel, "^../..", "https://vote.nyc.ny.us")
links_rel[!str_detect(links_rel, "^https://vote.nyc.ny.us")] <- paste0("https://vote.nyc.ny.us", links_rel[!str_detect(links_rel, "^https://vote.nyc.ny.us")])

# download csvs to list
raw <-  vector("list", length(links_rel))
names(raw) <- links_rel

for (l in links_rel) {
  raw[[l]] <- read.csv(l, as.is = T)
}

# find the csvs with the ridiculous bad BOE formatting
badcsv <- which(sapply(raw, ncol)!=11)

# replace bad csvs with new pull, trim leading 11 cols and replace with header from good file
for (l in names(badcsv)) {
  raw[[l]] <- read.csv(l, as.is = T, header = FALSE)
  raw[[l]] <- raw[[l]][, 12:22]
  names(raw[[l]]) <- names(raw[[1]])
}

table(sapply(raw, ncol)!=11)

# format individuals results dfs

clean_results <- function(df) {
  df %>% 
    filter(!Unit.Name %in% c("Affidavit", "Absentee/Military", "Manually Counted Emergency", "Absentee / Military", "Federal", "Public Counter", "Scattered", "Emergency")) %>%
    rename(edraw = ED) %>%
    mutate(ed  = paste("Ad ", AD, " - Ed ", str_pad(edraw, width = 3, side = "left", pad = 0), sep = "")) %>%
    separate(Event, c("election_type", "date"), sep = " - ") %>%
    separate(date, c("electionmonth", "electionday", "electionyear"), sep = "/") %>%
    mutate_at(vars(electionmonth, electionday, electionyear), as.numeric) %>%
    separate(Unit.Name, c("candidate", "party"), sep = " \\(") %>%
    mutate(party = str_replace(party, "\\)", "")) %>%
    mutate_at(vars(candidate, party), str_trim) %>% 
    rename(districtnumber = District.Key) %>%
    mutate(districtnumber = as.character(districtnumber)) %>% 
    rename(office = Office.Position.Title,
           votecount = Tally) %>%
    select(office, county = County, ed, electionyear, electionmonth, electionday, districtnumber, candidate, party, votecount)
}

rawt <- lapply(raw[3:5], clean_results)
rawtt <- bind_rows(rawt)

raw <- lapply(raw, clean_results)
sapply(raw, is.data.frame) %>% table()

comb <- bind_rows(raw)
table(comb$party, useNA = 'ifany')
table(comb$office, useNA = 'ifany')

glimpse(comb)

comb <- comb %>% 
  mutate(office = case_when(office == "State Senator" ~ "SD",
                            office == "Member of the Assembly" ~ "AD",
                            TRUE ~ office))
saveRDS(comb, "combined2017res.Rds")
## extract all a href tags
## filter to containing "*EDLevel.csv"
# then loop through load & r bind each of those csv urls

comb17 <- readRDS("combined2017res.Rds")

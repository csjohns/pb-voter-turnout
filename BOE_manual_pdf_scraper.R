## BOE manual file fixing
## code copied from BOE_pdf_scraper.R

source("BOE_scraper_funs.R")

manual <- readr::read_csv("HJC_example_BOE_total.csv") %>% 
  select(-X1)
names(manual) <- c("group", "votes", "district", "scope", "year", "election", "date")
manual <- manual %>% 
  mutate(office = case_when(district == "13 Congressional District" ~ "Representative in Congress",
                            district == "36 City Council District" ~ "Democratic Member of the City Council",
                            TRUE ~ NA_character_),
         election = paste0(election, " Election ", year))
manual <- manual %>% 
  filter(!is.na(district )) %>% 
  select(year, group, votes, election, date, scope, office, district)

res_year <- vector("list", 2)
names(res_year) <- c("2010", "2013")
res_year[["2010"]] <- filter(manual, year == 2010) %>% select(-year)
res_year[["2013"]] <- filter(manual, year == 2013) %>% select(-year)

res_year_clean <- lapply(res_year, collapse_clean_tabs)
for (p in seq_along(res_year)){
  res_year[[p]] <- collapse_clean_tabs(res_year[[p]]) 
}

## confirming no other weird rows/records
res_year[["2010"]] %>%   filter(!str_detect(toupper(group), "WRITE-IN|EMERGENCY|AFFIDAVIT|ABSENTEE|PUBLIC COUNTER|UNRECORDED|TOTAL BALLOTS|FEDERAL|SPECIAL PRESIDENTIAL|APPLICABLE BALLOTS")) %>% 
  .$group %>% table() %>% sort(decreasing = TRUE) %>% as.matrix()
res_year[["2013"]] %>%   filter(!str_detect(toupper(group), "WRITE-IN|EMERGENCY|AFFIDAVIT|ABSENTEE|PUBLIC COUNTER|UNRECORDED|TOTAL BALLOTS|FEDERAL|SPECIAL PRESIDENTIAL|APPLICABLE BALLOTS|BLANK|INVALID")) %>% 
  .$group %>% table() %>% sort(decreasing = TRUE) %>% as.matrix()

# no total votes for 2013!  Adding manually
tot_votes2013 <- res_year[["2013"]] %>%   
  filter(!str_detect(toupper(group), "WRITE-IN|EMERGENCY|AFFIDAVIT|ABSENTEE|PUBLIC COUNTER|UNRECORDED|TOTAL BALLOTS|FEDERAL|SPECIAL PRESIDENTIAL|APPLICABLE BALLOTS|BLANK|INVALID")) %>% 
  summarize(votes = sum(votes)) %>% 
  mutate(group = "Total Votes") %>% 
  select(group, votes)

tot_votes2013 <- res_year[["2013"]] %>% select(-group, -votes) %>% distinct() %>% 
  bind_cols(tot_votes2013, .)

res_year[["2013"]] <- bind_rows(res_year[["2013"]], tot_votes2013)

## clean up district names
res_year[["2010"]]$district <- "13th Congressional District"
res_year[["2013"]]$district <- "36th City Council District"

saveRDS(res_year,"BOE_manual.rds")


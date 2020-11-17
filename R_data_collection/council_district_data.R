### council district data selected and combined
library(dplyr)
library(stringr)
library(tidyr)

diststats <- read.csv("data/ccdata.csv", as.is = T)
glimpse(diststats)
ccmstats <- read.csv("data/council_district_summary.csv", as.is =T) 

diststats <- diststats %>% 
  select(-X) %>% 
  mutate(CD = as.numeric(str_remove_all(CD, "CD"))) %>% 
  mutate_at(vars(-CD, -pop, -medhhinc), ~./pop) %>% 
  select(-pop) %>% 
  rename_at(vars(age18, white, college), ~paste0(., "_pct"))

ccmstats <- ccmstats %>% 
  select(district, year, incumbent, competitivejenks,vote_margin_pct_next) %>% 
  filter(year %in% c(2009,2013,2017)) %>% 
  pivot_wider(names_from = year, values_from = c("incumbent", "competitivejenks", "vote_margin_pct_next")) #%>%
  # filter(is.na(incumbent_2013)|is.na(incumbent_2009)|is.na(incumbent_2017))
                     
ccdist_combined <- full_join(ccmstats, diststats, by = c("district" = "CD"))

glimpse(ccdist_combined)

saveRDS(ccdist_combined, "data/cleaned_R_results/council_districts_wmargin.rds")

### exploratory jenks
BAMMtools::getJenksBreaks(diststats$age18_pct,3)
hist(diststats$age18_pct)
# 0.08590616 0.19842293 0.32915168

BAMMtools::getJenksBreaks(diststats$white_pct,4)
hist(diststats$white_pct)
# 0.01281779 0.19836858 0.49529454 0.81920630

BAMMtools::getJenksBreaks(diststats$college_pct,4 )
hist(diststats$college_pct)
#  0.07188765 0.21320295 0.37701862 0.65609417
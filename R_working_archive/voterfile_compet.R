##############################################################################################################################
###
### NYC PB Voters Project 
### Attaching competitiveness to voters
### Carolina Johnson
### 02/12/2020
###
### 1) Reformat voter file/district ids so that it can join to competitiveness 
### 2) Join compet in by office, district, year, election
### 3) Collapse voterfile to most competitive election
### 
###
##############################################################################################################################

### Load libraries ---------------
library(dplyr)
library(tidyr)
library(stringr)
library(data.table)

## load voterfile 
voterfile <- fread("PersonFile20180426-11056504994/PersonFile20180426-11056504994.txt")
voterfile <- voterfile[,c("Voter File VANID", "CD", "SD", "CityCouncilName", "HD")]
voterfile <- voterfile %>% 
  rename("VANID" = "Voter File VANID",
         Congress12 = "CD",
         Council12 = "CityCouncilName",
         AD12 = "HD",
         SD12 = "SD")

## load vf with geodata

voterfile_sf <- readRDS( "data/cleaned_R_results/voterfile_oldgeo.RDS")
glimpse(voterfile_sf)

voterfile_sf <- voterfile_sf %>% 
  rename(Congress02 = "CongDist",
         Council02 = "CounDist",
         AD02 = "AssemDist",
         SD02 = "StSenDist")

voterfile_sf <- as.data.table(voterfile_sf)

## join old and current district geographies - only keeping voters who we can attach to districts
voterfile <- inner_join(voterfile, voterfile_sf, by = "VANID")
## sample voterfile by 10% for remaining testing
# voterfile_samp <- sample_frac(voterfile, 0.1)
# rm(voterfile)
# voterfile <- voterfile_samp
# rm(voterfile_samp)

rm(voterfile_sf)

# add citywide offices 
voterfile <- voterfile %>% 
  mutate("AG02" = -1,
         "AG12" = -1,
         "Governor02" = -1,
         "Governor12" = -1,
         "Mayor02" = -1,
         "Mayor12" = -1,
         "Senator02" = -1)

## load competitiveness data
compet <- readRDS("data/cleaned_R_results/competitiveness.RDS")
unique(compet$office)

# recode to match with voterfile
compet <- compet %>% 
  ungroup() %>% 
  mutate(office = recode(office, "Member of the City Council" = "Council", "Representative in Congress" = "Congress",
                         "Member of the Assembly" = "AD", "State Senator" = "SD", "Attorney General" = "AG", "US Senator" = "Senator")) 
competgo <- compet %>% 
  mutate(census = ifelse(year < "2013", "02", "12")) %>% 
  unite(office, office, census, sep = "") %>% 
  mutate(election = case_when(str_detect(tolower(election), "federal") ~ "federal",
                              str_detect(tolower(election), "primary") ~ "primary",
                              str_detect(tolower(election), "general") ~ "general",
                              TRUE ~ NA_character_)) %>% 
  select(office, district, year, election, margin = vote_margin_pct_next)
  
## reshape voterfile long to be able to join with competgo
## split voterfile into 10 sets for this


attach_filter_compet <- function(df) {
  df  %>% 
    gather(office, district, -VANID) %>% 
    mutate(district = as.numeric(district)) %>% 
    full_join(competgo, by = c("office", "district")) %>% 
    mutate(office = str_remove_all(office, "\\d+")) %>% 
    group_by(VANID, election, year) %>% 
    filter(margin == min(margin, na.rm = T)) %>% 
    ungroup()
}


nsplits <- 10
voterfile$group <- sample(1:nsplits, size = nrow(voterfile), replace = T)
filenames <- sapply(1:nsplits, function(x)paste0("data/temp/compet_split_", x, ".rds"))

for (i in 1:nsplits) {
  voterfile %>% 
    filter(group == i) %>% 
    saveRDS(file = filenames[i])
}

rm(voterfile)
gc()

processed <- vector("list", length = nsplits)
names(processed) <- filenames

for (f in filenames) {
  temp <- readRDS(f)
  processed[[f]] <- attach_filter_compet(temp)
}

gc()
saveRDS(processed, file = "data/temp/processed_compet.rds")

## clear environ & restart R
processed <- readRDS("data/temp/processed_compet.rds")
processed <- bind_rows(processed)
gc()
saveRDS(processed, "data/cleaned_R_results/long_compet.rds")
gc()

table(processed$office) %>% prop.table()
table(processed$office, processed$year) %>% prop.table(1)
# 
# processed <- processed %>%
#   filter(!is.na(VANID)) %>% 
#   select(-office, -district) %>% 
#   unite(col = "election", year, election ) %>% 
#   mutate(election = stringr::str_replace(election, "federal", "pp")) %>% 
#   spread(election, margin, fill = NA)
# 
# # 

## Final reshape steps were too memory intensive - switched to desktop with 32G RAM (not sure whether 16G would have done it).

### Some additional exploratory analysis ----
library(dplyr)
library(tidyr)
library(ggplot2)

vf_compet <- readRDS("data/cleaned_R_results/wide_compet.rds")

compet_plot <- function(var) {
  hist(vf_compet[[var]], main = var)
}
par(mfrow = c(4,4))
lapply(names(vf_compet)[-1], compet_plot)
summary(vf_compet) ## weird behavior with the cols with nas (all just 0,1 in summary...)
summarystats <- lapply(vf_compet, na.omit) %>% sapply(summary) %>% t() %>% as_tibble(rownames = "election") %>% filter(election != "VANID")

summarystats %>% 
  gather("statistic", "value", -election) %>% 
  mutate(statistic = factor(statistic, levels = c("Min.", "1st Qu.", "Median", "Mean", "3rd Qu.", "Max."))) %>% 
  ggplot(aes(y = election, x = value, color = statistic)) +
  geom_jitter(height = .3, size =3, alpha = .75)

lapply(vf_compet[,-1], na.omit) %>% sapply(n_distinct)
lapply(vf_compet[,-1], na.omit) %>% sapply(as.character) %>% barplot()

compet_box <- function(var) {
  boxplot(vf_compet[[var]], main = var)
}
par(mfrow = c(4,4))
lapply(names(vf_compet)[c(13,15)], compet_box)


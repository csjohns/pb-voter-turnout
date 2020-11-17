##############################################################################################################################
###
### NYC PB Voters Project 
### Exploring competitiveness PB/non-PB
### Carolina Johnson
### 02/25/2020
###
### 1) read in PB data
### 2) flag vf_compet if PB
### 3) descriptive stats
### 
###
##############################################################################################################################

library(dplyr)
library(tidyr)
library(ggplot2)

pb <- readRDS("data/cleaned_R_results/pb.rds")

vf_compet <- readRDS("data/cleaned_R_results/wide_compet.rds")

vf_compet <- vf_compet %>% 
  mutate(pb = VANID %in% pb$VANID)

vf_sample <- sample_n(vf_compet, 1000000)
vf_sample <- vf_compet %>% 
  filter(pb) %>% 
  full_join(vf_sample)

table(vf_sample$pb) %>% prop.table()

vf_long <- vf_sample %>% 
  gather(key = "election", value = "compet", starts_with("20"))

ggplot(vf_long) +
  geom_boxplot(aes(x = election, y = compet, fill = pb)) 

# looking for common support in competetivieness between pb/non-pb voters.  There is a substantial overlap for all years,
# with the 2016 primary show the most divergency (but still within body of distribution)
# years with no apparent variation to be excluded from matching: 
# - 2009_general, 2010_primary, 2013_general, 2013_primary, 2017_general


vflong <- voterfile %>% 
  select(starts_with("comp"), VANID, pb) %>% 
  gather("election", "compet", starts_with("comp")) %>% 
  mutate_all(replace_na, 1) %>% 
  mutate(pb = as.factor(pb))

library(ggplot2)
ggplot(vflong) +
  geom_boxplot(aes(x= election, y = compet, fill = pb))


##############################################################################################################################
###
### NYC PB Voters Project 
### Processing presidential competitiveness numbers
### Carolina Johnson
###
### Uses statewide recap result data from state BOE: https://www.elections.ny.gov/
### 
### 1. Load presidential 
### 2. Process to calculate margin
### 3. Create function to attache presidential values
###
##############################################################################################################################

# Load libraries
library(dplyr)
library(tidyr)

# load presidential
pres <- read.csv("data/pres_elec_res.csv", as.is = TRUE)

pres <- pres %>% 
  mutate(margin = (winner.votes - runner.up.votes)/total.votes) 
pres_wide <- pres %>% 
  select(-ends_with("votes")) %>% 
  unite(election, year, election) %>% 
  # mutate(election = paste0("comp_", election)) %>% 
  filter(!is.na(margin)) %>% 
  spread(key = election, value = margin) 

attach_pres <- function(df, pres_wide){
  for (c in seq_along(pres_wide)){
    df[[names(pres_wide)[c]]] <- pres_wide[1,c]
  }
  df
}

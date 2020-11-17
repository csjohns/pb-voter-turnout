# Script to read in City Council District data, downloaded from http://www.infoshare.org
# 2012-2016 5-yr ACS estimates

library(tidyverse)
setwd("/Users/jakecarlson/Box Sync/PBP/VoterMatching/Data/CityCouncil")

files <- dir(pattern = "*.csv")

cleanfiles <- function(file){
  cdnum <- str_extract(file, "\\d{2}")
  
  cd <- paste("CD", cdnum, sep = "")
  cd <- enquo(cd)
  
  df <- read_csv(file) %>%
    t() %>% as_data_frame()
  
  colnames(df) <- df[1,]
  
  vars <- c("Total Population", "Persons under 5 years", "Persons 5-9 years", 
            "Persons 10-14 years", "Persons 15-17 years", 
            "Non-Hispanic white alone persons",
            "Number of persons 25+ yrs with bachelor's degree",  "Number of persons 25+ yrs with master's degree",
            "Number of persons 25+ yrs with professional school degree", "Number of persons 25+ yrs with doctorate degree",
            "Median household income ($2016)")
  
  df <- df %>% 
    slice(2) %>%
    select(one_of(vars)) %>%
    rename(pop = "Total Population",
           age5 = "Persons under 5 years",
           age9 = "Persons 5-9 years",
           age14 = "Persons 10-14 years",
           age17 = "Persons 15-17 years",
           white = "Non-Hispanic white alone persons",
           bachelors = "Number of persons 25+ yrs with bachelor's degree",
           masters = "Number of persons 25+ yrs with master's degree",
           profdeg = "Number of persons 25+ yrs with professional school degree",
           doctor = "Number of persons 25+ yrs with doctorate degree",
           medhhinc = "Median household income ($2016)") %>%
    mutate_all(as.numeric) %>%
    mutate(age18 = age5 + age9 + age14 + age17,
           college = bachelors + masters + profdeg + doctor,
           CD = !! cd) %>%
    select(pop, age18, white, college, medhhinc, CD)
    
  
}

ccdata <- files %>%
  map_df(cleanfiles)

ccdata <- ccdata %>% # Combines CDs 8, 22, and 34, which cross boroughs
  filter(medhhinc != 0) %>% # this drops Rikers Island from CD22
  group_by(CD) %>%
  summarize(pop = sum(pop),
            age18 = sum(age18),
            white = sum(white),
            college = sum(college),
            medhhinc = mean(medhhinc)) #note, taking average of the med HH incomes

write.csv(ccdata, file = "ccdata.csv")

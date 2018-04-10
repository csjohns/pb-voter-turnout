##### 
### Matching PB to non-PB voters

library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)
library(data.table)
library(MatchIt)
library(cem)
library(lme4)
library(simcf)

source("credentials.R") # loads the access credentials
source("dbDownload.R")

stringNAs <- function(x){
  ifelse(x, "", NA)
}

### Load PB data ### -------------------------------------------------------------------------------------

pb <- dbDownload(table = "pb", username = username, password = password, dbname = db.name, host = hostname, port = port)
rm(password, username, hostname, db.name, port) # if you want to remove the credentials from your environment 
# load("pb_orig.Rdata")
#pb <- pb_orig 
#rm(pb_orig)
pb <- pb %>% select(-DWID) %>% 
  filter(DoR != "" & !is.na(DoR))
pb <- pb %>% mutate(DoB = mdy(DoB),
                    pb = 1)


### Load full voterfile data ### -------------------------------------------------------------------------------------
### Limit it to only non-PB districts and VANIDS


load("pbdistricts.Rdata")
pbdistricts <- na.omit(pbdistricts)

## loading full voter file
voterfile <- fread("personfileFULL20170731-15112428081/personfileFULL20170731-15112428081.txt")

## filtering voterfile to only actual registered non-pb voters and 
voterfile <- voterfile[!CityCouncilName %in% pbdistricts & !`Voter File VANID` %in% pb$VANID & !is.na(CityCouncilName) & RegistrationStatusName != "Applicant"]

## Renaming/recoding to match with the pb table on the DB
voterfile <- as.data.frame(voterfile)

voterfile <- voterfile  %>%
  rename(Ethnicity = EthnicCatalistName,
         DoB = DOB,
         DoR = DateReg,
         RegStatus = RegistrationStatusName,
         County = CountyName,
         #City = CityName,
         Zip = Zip5,
         Lat = Latitude,
         Long = Longitude,
         NYCCD = CityCouncilName,
         CensusTract = CensusTractName,
         VANID = `Voter File VANID`,
         ED = PrecinctName)

voterfile <- voterfile  %>%
  mutate(County = recode(County, Bronx = "BRONX" , Kings = "KINGS", `New York` = "NEW YORK", Queens = "QUEENS", Richmond = "RICHMOND")) %>% 
  mutate(DoB = mdy(DoB))
  
  names(voterfile) <- names(voterfile) %>% 
    str_replace("General", "g_20") %>%
    str_replace("PresidentialPrimary", "pp_20") %>%
    str_replace("Primary", "p_20")
  
  voterfile <- voterfile %>%
    select(-starts_with("Special"), -ends_with("Party")) %>%
    select(-Lat, -Long, -starts_with("Street"), -starts_with("Apt"), - VHHID, -StateFileID, -DWID, 
           -starts_with("Reported"))
  voterfile <- voterfile %>% 
    filter(DoR != "")
  
  ### Load compare structure of the two data frames ### -------------------------------------------------------------------------------------
  
  setdiff(names(pb), names(voterfile))
  setdiff(names(voterfile), names(pb))

  ### Join to voter file ### -------------------------------------------------------------------------------------
  ## this code works by basically appending the voters from the districts of interest to the non-pb voterfile
  
  voterfile <- pb %>% 
    filter(pbdistrict %in% c(23, 39)) %>% ## this is filtering to only districts we have full/near full data for
    bind_rows(voterfile)
    
  ## recoding vote tallies to a binary voted/not voted indicator
  convLogicVote <- function(x){as.numeric(x != "")}   #function to create binary for any case with any non-empty string
  
  voterfile <- voterfile %>% mutate(pb = ifelse(is.na(pb), 0, pb))
  voterfile <- voterfile %>% 
    mutate_at(vars(starts_with("p_")), funs(convLogicVote)) %>% 
    mutate_at(vars(starts_with("g_")), funs(convLogicVote)) %>% 
    mutate_at(vars(starts_with("pp_")), funs(convLogicVote)) 
  
  ## Creating new variables for age (in years), and voting rates for years 2000-2007 (increasing granularity to aid matching/analysis)
  voterfile <- voterfile %>% 
    rowwise() %>% 
    mutate(age = year(Sys.Date()) - year(DoB),
           g_early = sum(g_2000, g_2001, g_2002, g_2003, g_2004, g_2005, g_2006, g_2007, na.rm = TRUE),
           p_early = sum(p_2000, p_2001, p_2002, p_2003, p_2004, p_2005, p_2006, p_2007, na.rm = TRUE)) %>% 
    group_by()
  
### Including census data ### ----------------------------------------------------------------------------------------------------------------------------
  source("censustables.R")
  #load("census.Rdata")
  voterfile <- voterfile %>% filter(County %in% c("BRONX", "KINGS", "NEW YORK", "QUEENS", "RICHMOND")) %>% 
    mutate(countycode = recode(County, BRONX = "005", KINGS = "047", `NEW YORK` = "061", QUEENS = "081", RICHMOND = "085"),
           tract = paste0(countycode, str_pad(CensusTract, 6, "left", "0")))

  gc()
  voterfile <- voterfile %>% 
    left_join(educ) %>%
    left_join(inc) %>% 
    left_join(race)
  
  voterfile <- voterfile %>% 
    mutate(agegroup = cut(age, breaks = c(0, 20, 30, 40, 50, 60,70,80,Inf)))
    
### Implementing Matching, starting with exact ###-----------------------------------------------------------------------------------------------------------------------------------------------  # 
  
## Exact matching to narrow field of possibility
  
  summary(voterfile)
  
  exact_df <- voterfile %>% 
    select(VANID, pb,  Race, agegroup, Sex, g_early, g_2008, g_2009, g_2010, g_2011, p_early, p_2008, p_2009, p_2010, pp_2004, pp_2008) %>% 
    na.omit()
  
  m.exact <- matchit(pb ~ Race +agegroup +Sex + g_early + g_2008 + g_2009 + g_2010 + g_2011 + p_early + p_2008 + p_2009 + p_2010 + pp_2004 + pp_2008, data = exact_df, method = "exact")
  
  treat_sub <- unique(m.exact$subclass[m.exact$treat == TRUE])
  table(m.exact$treat, is.na(m.exact$subclass))
  ## filter to rows with classes
  matchable_vans <- exact_df$VANID[!is.na(m.exact$subclass)]
  
  rm(m.exact)
  gc()
  
### Creating matching dataframe based on the potential matches from m.exact --------------------------------------------------------------------------------
  matching_df <- voterfile %>%
    filter(VANID %in% matchable_vans) %>% 
    mutate(agegroup = cut(age, breaks = c(0, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60,65, 70,75, 80,85, 90, Inf))) %>% 
    select(VANID, pb, Race, agegroup, Sex, g_early, g_2008, g_2009, g_2010, g_2011, p_early, p_2008, p_2009, p_2010, pp_2004, pp_2008, white, college, medhhinc) %>% 
    na.omit()
  
### Implementing CEM - defining cutpoints for continuous variables  --------------------------------------------------------------------------------
  
df_cutpoints <- list(
  white = quantile(matching_df$white, c(0,.2,.4,.6,.8,1)),
  #college = quantile(matching_df$college, c(0,.5,1)),
  medhhinc = quantile(matching_df$medhhinc, c(0,.2,.4,.6,.8,1))
)

  
  c.out <- matching_df %>% select(-VANID, -college) %>% 
    mutate_at(vars(starts_with("g_"), starts_with(("p_"))), as.factor) %>% 
    mutate(pp_2004 = as.factor(pp_2004),
           pp_2008 = as.factor(pp_2008),
           Race = as.factor(Race),
           Sex = as.factor(Sex)) %>% 
    cem(treatment = "pb", data = .,  
        grouping = list(
          g_early = list("0",c("1,2"), c("3", "4", "5"), c("6", "7,", "8")), 
          p_early = list("0",c("1,2"), c("3", "4", "5"), c("6", "7,", "8"))), 
        cutpoints = df_cutpoints,
        verbose = 1) ### DON'T USE K2K - TOO MUCH MEMORY DEMAND. RANDOMLY DRAW FROM STRATA AFTER THE FACT

    c.out
    
### Creating the pairwise k2k match including one random sampled control for every pb voter  --------------------------------------------------------------------------------
    c.match <- data.frame(VANID = matching_df$VANID, pb = matching_df$pb, cem_group = c.out$strata, race =matching_df$Race)
    
    c.match <- c.match %>% 
      group_by(cem_group) %>% 
      mutate(n_treat = sum(pb),
             n_control = sum(pb==0)) %>% 
      filter(n_treat > 0 & n_control > 0)
    
    c.treat <- c.match %>% filter(pb == 1 & n_control > 0)
    
    c.control <- c.match %>% filter(pb == 0 & n_treat > 0)
    
    c.control <- c.control %>% 
      group_by(cem_group) %>%
      sample_frac(1) %>% 
      slice(1:unique(n_treat))
    
    c.treat <- c.treat %>% 
      mutate(n_sample = ifelse(n_treat <= n_control, n_treat, n_control )) %>%
      group_by(cem_group) %>% 
      sample_frac(1) %>% 
      slice(1:unique(n_sample)) 
    
    c.match <- c.treat %>% 
      select(-n_sample) %>% 
      bind_rows(c.control)
    
### creating analysis DF by joining voterfile to the CEM match output - effectively returns voterfile info filtered to matched dataset -----
    vf_analysis <- c.match %>% select(-n_treat, -n_control) %>% 
      left_join(voterfile)
    
    vf_analysis %>% filter(pb == 1) %>% select(Race, Sex, medhhinc, college, white, g_early, p_early, age) %>% summary()
    vf_analysis %>% filter(pb == 0) %>% select(Race, Sex, medhhinc, college, white, g_early, p_early, age)  %>% summary()
    
    rm(voterfile)
    gc()

#### Saving matched datafile as R object for future use  --------------------------------------------------------------------------------
save(vf_analysis, file = "vf_analysis.Rdata")

    
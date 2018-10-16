##### 
### Matching PB to non-PB voters

library(dplyr)
library(tidyr)
library(glue)
library(lubridate)
library(stringr)
library(data.table)
library(MatchIt)
library(cem)

source("credentials.R") # loads the access credentials
source("dbDownload.R")

stringNAs <- function(x){
  ifelse(x, "", NA)
}
# 
# conv19c <- function(s, ft = "%m/%d/%y"){
#   as.Date(format(as.Date(s,format=ft), "19%y%m%d"), "%Y%m%d")
# }
### Load PB data ### -------------------------------------------------------------------------------------

pb <- dbDownload(table = "pb", username = username, password = password, dbname = db.name, host = hostname, port = port)

# load("pb_orig.Rdata")
#pb <- pb_orig 
#rm(pb_orig)
pb <- pb %>% select(-DWID) %>% 
  filter(DoR != "" & !is.na(DoR)) %>% 
  mutate_at(vars(starts_with("pb_2")), replace_na, 0) # this line is to deal with the fact taht the row appended dist 23 voters (who didn't otherwise exist)
pb <- pb %>% mutate(DoB = mdy(DoB),
                    pb = 1)

# limit to only 23/39 and 2016 districts
pbnyc <- read.csv(file = "pbnyc_district_votes.csv", as.is = TRUE)
pb2016 <- pbnyc %>% filter(districtCycle == 1 & voteYear == 2016) 

### Load full voterfile data ### -------------------------------------------------------------------------------------
### Limit it to only non-PB districts and VANIDS

pbdistricts <- unique(pbnyc$district)
rm(pbnyc)

## loading full voter file
con <- dbConnect(MySQL(), username = username, password = password, dbname = db.name, host = hostname, port = port) #establish connection to DB
voterfile <- glue_sql("SELECT * FROM voterfile52018 
                      WHERE RegistrationStatusName NOT IN ('Applicant', 'Dropped', 'Unregistered')
                      AND DateReg <> ''
                      AND (CityCouncilName IS NULL 
                      OR CityCouncilName NOT IN ({nyccds*}))",
                      nyccds = pbdistricts,
                      .con = con) %>% 
  dbGetQuery(con, .)
dbDisconnect(con)
rm(password, username, hostname, db.name, port) # if you want to remove the credentials from your environment 
save(voterfile, file = "voterfile_noPB.Rdata ")
voterfile <- voterfile <- voterfile %>% 
  filter(DateReg != "" & !`Voter File VANID` %in% pb$VANID) 

## adding missing district info
source("vf_gis_nyccdmatch.R")
source("pb_cleanup_addnyccd_foranalysis.R")

## remove districts in pbdistricts
voterfile <- voterfile %>% filter(!CityCouncilName %in% pbdistricts & !is.na(CityCouncilName))
save(voterfile, file = "voterfile_noPB_gis.Rdata")

# voterfile <- fread("PersonFile20180426-11056504994/PersonFile20180426-11056504994.txt")

# ## filtering voterfile to only actual registered non-pb voters and 
# voterfile <- voterfile[!CityCouncilName %in% pbdistricts & !`Voter File VANID` %in% pb$VANID & !is.na(CityCouncilName) & RegistrationStatusName != "Applicant"]
# 
# ## Renaming/recoding to match with the pb table on the DB
# voterfile <- as.data.frame(voterfile)

# renaming columns - modified 5/15 for the 2018 voterfile column name changes
voterfile <- voterfile  %>%
  rename(Ethnicity = EthnicCatalistName,
         DoB = DOB,
         DoR = DateReg,
         RegStatus = RegistrationStatusName,
         County = CountyName,
         City = CityName,
         #Zip = Zip5,
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
           -starts_with("Reported"), -CounDist)
  
  ### Load compare structure of the two data frames ### -------------------------------------------------------------------------------------
  
  setdiff(names(pb), names(voterfile))
  setdiff(names(voterfile), names(pb))

  ### Join to voter file ### -------------------------------------------------------------------------------------
  ## this code works by basically appending the voters from the districts of interest to the non-pb voterfile
  
  voterfile <- pb %>% 
    filter(pbdistrict %in% c(23, 39, pb2016$district) | pb_2012 == 1) %>% ## this is filtering to only districts we have full/near full data for
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
  gc()
  
### Including census data ### ----------------------------------------------------------------------------------------------------------------------------
  source("censustables.R")
  # load("census.Rdata")
  voterfile <- voterfile %>% filter(County %in% c("BRONX", "KINGS", "NEW YORK", "QUEENS", "RICHMOND")) %>% 
    mutate(countycode = recode(County, BRONX = "005", KINGS = "047", `NEW YORK` = "061", QUEENS = "081", RICHMOND = "085"),
           tract = paste0(countycode, str_pad(CensusTract, 6, "left", "0")))

  gc()
  voterfile <- voterfile %>% 
    left_join(educ) %>%
    left_join(inc) %>% 
    left_join(race)
  
  ## Add indicator for if voter's race matches majority race of tract
  voterfile <- voterfile %>% 
    mutate(majmatch = Race == majority)
  
  ##7657 PB voters in the voter file
  
### Including competitiveness ----------------------------------------------------------------------
load("compet.Rdata")  

voterfile <- compet_select %>% 
  select(-County) %>% 
  left_join(voterfile, .)
  
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
    rename(comp_g_2016 = g_2016_comp, comp_g_2014 = g_2014_comp, comp_p_2014 = p_2014_comp, comp_pp_2016 = pp_2016_comp) %>% 
    mutate(agegroup = cut(age, breaks = c(0, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60,65, 70,75, 80,85, 90, Inf))) %>% 
    select(VANID, pb, Race, agegroup, Sex, 
           g_early, g_2008, g_2009, g_2010, g_2011, p_early, p_2008, p_2009, p_2010, pp_2004, pp_2008, 
           white, college, medhhinc , majmatch 
           , starts_with("comp")
           # ,g_2014_comp, g_2016_comp, p_2014_comp, pp_2016_comp
           ) %>% 
    na.omit()
  
  
### Implementing CEM - defining cutpoints for continuous variables  --------------------------------------------------------------------------------
  
df_cutpoints <- list(
  white = quantile(matching_df$white, c(0,.2,.4,.6,.8,1)),
  #college = quantile(matching_df$college, c(0,.5,1)),
  medhhinc = quantile(matching_df$medhhinc, c(0,.2,.4,.6,.8,1))
  , comp_g_2014 = quantile(compet_select$g_2014_comp, probs = c(0, .25, .75, 1), na.rm = TRUE),
  comp_g_2016 = quantile(compet_select$g_2016_comp, probs = c(0, .25, .75, 1), na.rm = TRUE),
  comp_p_2014 = quantile(compet_select$p_2014_comp, probs = c(0, .25, .75, 1), na.rm = TRUE),
  comp_pp_2016 = quantile(compet_select$pp_2016_comp, probs = c(0, .25, .75, 1), na.rm = TRUE)
)

  
  c.out <- matching_df %>% select(-VANID, -college) %>% 
    mutate_at(vars(starts_with("g_"), starts_with(("p_"))), as.factor) %>% 
    mutate(pp_2004 = as.factor(pp_2004),
           pp_2008 = as.factor(pp_2008),
           Race = as.factor(Race),
           Sex = as.factor(Sex)) %>% 
    cem(treatment = "pb", data = .,  
        grouping = list(
          g_early = list("0",c("1,2"), c("3", "4"), c("5", "6"), c("7,", "8")), 
          p_early = list("0",c("1,2"),  c("3", "4"), c("5", "6"), c("7,", "8"))
          ), 
        cutpoints = df_cutpoints,
        verbose = 1) ### DON'T USE K2K - TOO MUCH MEMORY DEMAND. RANDOMLY DRAW FROM STRATA AFTER THE FACT

    c.out
    #4707 matched, 2158 not = 69% match rate -- matching on outer, inner quantile splits == 6330/6204 (59%)
    
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
    vf_analysis <- c.match %>% dplyr::select(-n_treat, -n_control) %>% 
      left_join(voterfile) %>% 
      rename(comp_g_2016 = g_2016_comp, comp_g_2014 = g_2014_comp, comp_p_2014 = p_2014_comp, comp_pp_2016 = pp_2016_comp) 
    
    vf_analysis %>% filter(pb == 1) %>% select(Race, Sex, medhhinc, college, white, g_early, p_early, age, majmatch) %>% summary()
    vf_analysis %>% filter(pb == 0) %>% select(Race, Sex, medhhinc, college, white, g_early, p_early, age, majmatch)  %>% summary()
    
    rm(voterfile)
    gc()

#### Saving matched datafile as R object for future use  --------------------------------------------------------------------------------
save(vf_analysis, file = "vf_analysis.Rdata")

    
## Creating population level cutoffs for matching

library(dplyr)
library(tidyr)
library(glue)
library(lubridate)
library(stringr)
library(data.table)

stringNAs <- function(x){
  ifelse(x, "", NA)
}

### Load full voterfile data ### -------------------------------------------------------------------------------------

voterfile <- fread("PersonFile20180426-11056504994/PersonFile20180426-11056504994.txt")
voterfile <- voterfile[! RegistrationStatusName %in% c('Applicant', 'Dropped', 'Unregistered') & DateReg != ""]

set.seed(92018)
source("vf_gis_nyccdmatch.R")

### standard voterfile column name processing etc -------------------------------
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

### Downstream processing, adding auxiliary info - pulled out to standardize across different match_specs -------------
source("rr_vf_aux_processing.R")

median_excl_99  <-  function(x) {
  median(x[x != -99], na.rm = TRUE)
}

breaks_from_jenks <- function(x, k = 4) {
  c(0, BAMMtools::getJenksBreaks(sample(x, 10000), k = k)[2:(k-1)], 1)
}
  

# note: cutpoints are inclusive on the upper end
fine_cuts <- list(
  white = quantile(voterfile$white, c(0,.2,.4,.6,.8,1), na.rm = T),
  college = quantile(voterfile$college, c(0,.5,1), na.rm = T),
  medhhinc = quantile(voterfile$medhhinc, c(0,.2,.4,.6,.8,1), na.rm = T)
  , dist_white = breaks_from_jenks(voterfile$dist_white)
  , dist_college = breaks_from_jenks(voterfile$dist_college)
  , dist_medhhinc = breaks_from_jenks(voterfile$dist_medhhinc)
  , dist_age18 = breaks_from_jenks(voterfile$dist_age18)
  , comp_2008_primary = c(-99, 0, median_excl_99(voterfile$comp_2008_primary) - 0.001, 1)
  , comp_2009_primary = c(-99, 0, median_excl_99(voterfile$comp_2009_primary) - 0.001, 1)
  , comp_2010_general = c(-99, 0, median_excl_99(voterfile$comp_2010_general) - 0.001, 1)
  , comp_2012_primary = c(-99, 0, median_excl_99(voterfile$comp_2012_primary) - 0.001, 1)
  , comp_2014_general = c(-99, 0, median_excl_99(voterfile$comp_2014_general) - 0.001, 1)
  , comp_2014_primary = c(-99, 0, median_excl_99(voterfile$comp_2014_primary) - 0.001, 1)
  , comp_2016_primary = c(-99, 0, median_excl_99(voterfile$comp_2016_primary) - 0.001, 1)
  , comp_2017_primary = c(-99, 0, median_excl_99(voterfile$comp_2017_primary) - 0.001, 1)
)

coarse_cuts <- list(
  white = quantile(voterfile$white, c(0, 0.5, 1), na.rm = T),
  college = quantile(voterfile$college, c(0,.5,1), na.rm = T),
  medhhinc = quantile(voterfile$medhhinc, c(0,.5,1), na.rm = T)
  , dist_white = quantile(voterfile$dist_white, c(0,.5,1), na.rm = T)
  , dist_college = quantile(voterfile$dist_college, c(0,.5,1), na.rm = T)
  , dist_medhhinc = quantile(voterfile$dist_medhhinc, c(0,.5,1), na.rm = T)
  , dist_age18 = quantile(voterfile$dist_age18, c(0,.5,1), na.rm = T)
  , comp_2008_primary = c(-99, 0, median_excl_99(voterfile$comp_2008_primary), 1)
  , comp_2009_primary = c(-99, 0, median_excl_99(voterfile$comp_2009_primary), 1)
  , comp_2010_general = c(-99, 0, median_excl_99(voterfile$comp_2010_general), 1)
  , comp_2012_primary = c(-99, 0, median_excl_99(voterfile$comp_2012_primary), 1)
  , comp_2014_general = c(-99, 0, median_excl_99(voterfile$comp_2014_general), 1)
  , comp_2014_primary = c(-99, 0, median_excl_99(voterfile$comp_2014_primary), 1)
  , comp_2016_primary = c(-99, 0, median_excl_99(voterfile$comp_2016_primary), 1)
  , comp_2017_primary = c(-99, 0, median_excl_99(voterfile$comp_2017_primary), 1)
)



save(fine_cuts, coarse_cuts, file = "data/cleaned_R_results/cutpoints.Rdata")

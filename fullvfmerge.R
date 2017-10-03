##Pre-2012 votes

library(RColorBrewer)
library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)
library(margins)


source("credentials.R") # loads the access credentials
source("dbDownload.R")

stringNAs <- function(x){
  ifelse(x, "", NA)
}

### Loading and checking the data ###

pb_orig <- dbDownload(table = "pb", username = username, password = password, dbname = db.name, host = hostname, port = port)
rm(password, username, hostname, db.name, port) # if you want to remove the credentials from your environment 
pb <- pb_orig

fullvf <- read.delim("personfileFULL20170731-15112428081/personfileFULL20170731-15112428081.txt", stringsAsFactors = FALSE)

pb <- fullvf %>% select(General08, General09, General10, General11, Primary08, Primary09, Primary10, Primary12, PresidentialPrimary16, Voter.File.VANID) %>% 
  mutate(VANID = as.character(Voter.File.VANID)) %>%
  left_join(pb, .)

pb <- pb %>%  select(-`2008G`, -`2009G`, -`2010G`, -`2011G`, -`2016PP`) %>%
  rename(g_2008 = General08,
         g_2009 = General09,
         g_2010 = General10,
         g_2011 = General11,
         p_2008 = Primary08,
         p_2009 = Primary09,
         p_2010 = Primary10,
         pp_2016 = PresidentialPrimary16
         )
pb_orig <- pb

## renaming/recoding variables----
pb <- pb %>%
  rename(g_2012 = `2012G`,
         g_2013 = `2013G`,
         g_2014 = `2014G`,
         g_2015 = `2015G`,
         pp_2008 = `2008PP`,
         p_2012 = `2012P`,
         p_2013 = `2013P`,
         p_2014 = `2014P`,
         p_2015 = `2015P`,
         p_2016 = `2016P`,
         g_2016 = `2016G`
  ) 

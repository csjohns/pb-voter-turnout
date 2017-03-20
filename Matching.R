### Packages ###
library(dplyr)
library(car)
library(data.table)
library(chron)
library(lubridate)

## Load Data ##
source("/Users/jakecarlson/Box Sync/PBP/VoterMatching/Analysis/VoterTurnout/pb_querydownload.R") # loads the access credentials

### Data cleaning ###

## Recoding ##

# Rename general elections (Can't start variable w/ a number)
setnames(data, 
         old = c('2012G', '2013G', '2014G', '2015G'),
         new = c('G2012', 'G2013', 'G2014', 'G2015'))

# Rename primaries
setnames(data, 
         old = c('2012P', '2013P', '2014P', '2015P'),
         new = c('P2012', 'P2013', 'P2014', 'P2015'))

## Variable types ##
# Dates, need extra code, because as.Date uses "68" as cutoff, so you had DoBs as "2051"
data$DoB <- as.Date(data$DoB, "%m/%d/%y")
data$DoB <- as.Date(ifelse(data$DoB > "2016-12-31", format(data$DoB, "19%y-%m-%d"), format(data$DoB)))

data$DoR <- as.Date(data$DoR, "%m/%d/%y")
data$DoR <- as.Date(ifelse(data$DoR > "2016-12-31", format(data$DoR, "19%y-%m-%d"), format(data$DoR)))


# Turning election year variables to numeric, changing "Y" to 1, class to "numeric"
data$G2012 <- recode(data$G2012, "'Y'=1; else=0", as.numeric.result = TRUE)
data$G2013 <- recode(data$G2013, "'Y'=1; else=0", as.numeric.result = TRUE)
data$G2014 <- recode(data$G2014, "'Y'=1; else=0", as.numeric.result = TRUE)
data$G2015 <- recode(data$G2015, "'Y'=1; else=0", as.numeric.result = TRUE)

## New Variables ## 
# Number of General elections voted in
data <- mutate(data, genelec = G2012 + G2013 + G2014 + G2015)

# Average general election turnout
data$turnoutRate <- data$genelec/4


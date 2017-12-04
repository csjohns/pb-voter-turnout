library(acs)
library(stringr)
library(dplyr)

nyc <-  geo.make(state = "NY", county = c(5, 47, 61, 81, 85), tract = "*")

## Median income ##

# Table B19013 is for median income, also median income by race
census <- acs.fetch(2015, span = 5, geography = nyc,
                    table.name = "B19013", col.names = "pretty")

medIncome<-data.frame(boro=geography(census)[[1]],
                            data.frame(paste0(str_pad(census@geography$state, 2, "left", pad="0"), 
                                              str_pad(census@geography$county, 3, "left", pad="0"), 
                                              str_pad(census@geography$tract, 6, "left", pad="0")), 
                                       census@estimate[,"B19013. Median Household Income in the Past 12 Months (in 2015 Inflation-Adjusted Dollars): Median household income in the past 12 months (in 2015 Inflation-adjusted dollars)"],
                                       census@standard.error[,"B19013. Median Household Income in the Past 12 Months (in 2015 Inflation-Adjusted Dollars): Median household income in the past 12 months (in 2015 Inflation-adjusted dollars)"]
                                       ))
medIncome <- select(medIncome, 1:4)
rownames(medIncome)<-1:nrow(medIncome)
names(medIncome)<-c("name", "GEOID","median", "se")

## Education ##

## Race ##
census <- acs.fetch(2015, span = 5, geography = nyc,
                    table.number = "B02001", col.names = "pretty", case.sensitive = FALSE)

## Still need to fix, could also do Table B03002, which includes hispanic
race<-data.frame(boro=geography(census)[[1]],
                      data.frame(paste0(str_pad(census@geography$state, 2, "left", pad="0"), 
                                        str_pad(census@geography$county, 3, "left", pad="0"), 
                                        str_pad(census@geography$tract, 6, "left", pad="0")), 
                                 census@estimate[,"B19013. Median Household Income in the Past 12 Months (in 2015 Inflation-Adjusted Dollars): Median household income in the past 12 months (in 2015 Inflation-adjusted dollars)"],
                                 census@standard.error[,"B19013. Median Household Income in the Past 12 Months (in 2015 Inflation-Adjusted Dollars): Median household income in the past 12 months (in 2015 Inflation-adjusted dollars)"]
                      ))
medIncome <- select(medIncome, 1:4)
rownames(medIncome)<-1:nrow(medIncome)
names(medIncome)<-c("name", "GEOID","median", "se")


library(tidyverse)
library(data.table)
library(lubridate)
library(gridExtra)

voterfile <- fread("PersonFile20180426-11056504994.txt")

pbdistricts <- c(3, 5, 6, 7, 8, 10, 11, 12, 15, 16, 17, 19, 20, 21, 22, 23, 24, 26, 27, 29, 30,
                31, 32, 33, 34, 35, 36, 38, 39, 40, 41, 44, 45, 47, 49)

pbsampledists <- c(39, 23, 30, 35, 36, 40)

# Retain only "Registered Active" and "Registered Inactive"
voterfile <- voterfile[RegistrationStatusName == "Registered Active" | RegistrationStatusName == "Registered Inactive"]

voterfile <- voterfile  %>%
  rename(Ethnicity = EthnicCatalistName,
         DoB = DOB,
         DoR = DateReg,
         RegStatus = RegistrationStatusName,
         County = CountyName,
         #City = CityName,
         Lat = Latitude,
         Long = Longitude,
         NYCCD = CityCouncilName,
         CensusTract = CensusTractName,
         VANID = `Voter File VANID`,
         ED = PrecinctName)

voterfile <- voterfile  %>%
  mutate(County = recode(County, Bronx = "BRONX" , Kings = "KINGS", `New York` = "NEW YORK", Queens = "QUEENS", Richmond = "RICHMOND")) %>% 
  mutate(DoB = mdy(DoB))

voterfile <- voterfile %>% 
  mutate(pbdist = NYCCD %in% pbdistricts)

names(voterfile) <- names(voterfile) %>% 
  str_replace("General", "g_20") %>%
  str_replace("PresidentialPrimary", "pp_20") %>%
  str_replace("Primary", "p_20")

## recoding vote tallies to a binary voted/not voted indicator
convLogicVote <- function(x){as.numeric(x != "")}   #function to create binary for any case with any non-empty string

voterfile <- voterfile %>% 
  mutate_at(vars(starts_with("p_")), funs(convLogicVote)) %>% 
  mutate_at(vars(starts_with("g_")), funs(convLogicVote)) %>% 
  mutate_at(vars(starts_with("pp_")), funs(convLogicVote)) 

voterfile <- voterfile %>% 
    mutate(age = year(Sys.Date()) - year(DoB)) %>% 
  as.data.table()

# Total registered voters == 4,946,176
voterfile <- as.data.table(voterfile)


#### Race descripitves, registered voters ####

### NYC
race <- voterfile[, .N, by = Race] %>% 
  rename(NYC = N)

### D23
race1 <- voterfile[NYCCD == 23, .N, by = Race]

race$D23 <- race1$N

### D39
race1 <- voterfile[NYCCD == 39, .N, by = Race]

race$D39 <- race1$N

race <- race %>% 
  gather(Area, Population, 2:4)

race <- race %>% 
  group_by(Area) %>% 
  mutate(tot = sum(Population, na.rm = TRUE),
         Proportion = Population/tot)

rm(race1)
### Histogram
ggplot(race, aes(x = Race, y = Proportion, fill = Area)) + 
  geom_bar(stat = "identity", position = "dodge") +
  ggtitle("Race, Registered Voters") +
  theme_minimal() + 
  ggsave("raceRegistered.png")


# Race      NYC     23     39  
# W     1952876  33194  76586
# A      397442  28726   6687
# B     1370046  11359  11016
# U      116091  10881   4631
# H     1109530   4118   2789
# N         191      5      5



#### Age Descriptives Registered Voters ####
# For D23

ggplot(look, aes(x = age, y = tot)) + 
  geom_histogram(stat = "identity") +
#  ylim(0, 80000) + 
  ggtitle("District 23")

#For D39
look <- voterfile[NYCCD == 39, .N, by = Race]

look <- voterfile %>% 
  filter(NYCCD == 39) %>% 
  group_by(age) %>% 
  summarize(tot = n())

ggplot(look, aes(x = age, y = tot)) + 
  geom_histogram(stat = "identity") +
  #  ylim(0, 80000) + 
  ggtitle("District 39")

# For NYC

age <- voterfile %>% 
  group_by(age) %>% 
  summarize(tot = n()) %>% 
  ungroup() %>% 
  mutate(gtot = sum(tot, na.rm = TRUE),
         Proportion = tot/gtot) %>% 
  rename(NYC = Proportion) %>% 
  select(age, NYC)


age <- voterfile %>% 
  filter(NYCCD == 23) %>% 
  group_by(age) %>%  
  summarize(tot = n()) %>% 
  ungroup() %>% 
  mutate(gtot = sum(tot, na.rm = TRUE),
         Proportion = tot/gtot) %>% 
  rename(D23 = Proportion) %>% 
  select(age, D23) %>% 
  right_join(age, by = "age")

age <- voterfile %>% 
  filter(NYCCD == 39) %>% 
  group_by(age) %>%  
  summarize(tot = n()) %>% 
  ungroup() %>% 
  mutate(gtot = sum(tot, na.rm = TRUE),
         Proportion = tot/gtot) %>% 
  rename(D39 = Proportion) %>% 
  select(age, D39) %>% 
  right_join(age, by = "age") %>% 
  gather(Area, Proportion, 2:4)
  
ggplot(age, aes(x = age, y = Proportion, fill = Area, color = Area)) + 
  geom_density(alpha = .4, position = "identity", stat = "identity") +
  #  ylim(0, 80000) + 
  ggtitle("Age, Registered Voters") +
  theme_minimal() +
  ggsave("ageRegistered.png")


#### Turnout Registered Voters ####

## All NYC
turnout <- voterfile %>% 
  summarize(tot = n(),
            g_2017 = sum(g_2017, na.rm = TRUE)/tot,
            g_2016 = sum(g_2016, na.rm = TRUE)/tot,
            g_2015 = sum(g_2015, na.rm = TRUE)/tot,
            g_2014 = sum(g_2014, na.rm = TRUE)/tot,
            g_2013 = sum(g_2013, na.rm = TRUE)/tot,
            g_2012 = sum(g_2012, na.rm = TRUE)/tot,
            g_2011 = sum(g_2011, na.rm = TRUE)/tot,
            g_2010 = sum(g_2010, na.rm = TRUE)/tot,
            g_2009 = sum(g_2009, na.rm = TRUE)/tot,
            g_2008 = sum(g_2008, na.rm = TRUE)/tot,
            g_2007 = sum(g_2007, na.rm = TRUE)/tot,
            g_2006 = sum(g_2006, na.rm = TRUE)/tot,
            g_2005 = sum(g_2005, na.rm = TRUE)/tot,
            g_2004 = sum(g_2004, na.rm = TRUE)/tot,
            g_2003 = sum(g_2003, na.rm = TRUE)/tot,
            g_2002 = sum(g_2002, na.rm = TRUE)/tot,
            g_2001 = sum(g_2001, na.rm = TRUE)/tot,
            g_2000 = sum(g_2000, na.rm = TRUE)/tot,
            p_2016 = sum(p_2016, na.rm = TRUE)/tot,
            p_2015 = sum(p_2015, na.rm = TRUE)/tot,
            p_2014 = sum(p_2014, na.rm = TRUE)/tot,
            p_2013 = sum(p_2013, na.rm = TRUE)/tot,
            p_2012 = sum(p_2012, na.rm = TRUE)/tot,
            p_2011 = sum(p_2011, na.rm = TRUE)/tot,
            p_2010 = sum(p_2010, na.rm = TRUE)/tot,
            p_2009 = sum(p_2009, na.rm = TRUE)/tot,
            p_2008 = sum(p_2008, na.rm = TRUE)/tot,
            p_2007 = sum(p_2007, na.rm = TRUE)/tot,
            p_2006 = sum(p_2006, na.rm = TRUE)/tot,
            p_2005 = sum(p_2005, na.rm = TRUE)/tot,
            p_2004 = sum(p_2004, na.rm = TRUE)/tot,
            p_2003 = sum(p_2003, na.rm = TRUE)/tot,
            p_2002 = sum(p_2002, na.rm = TRUE)/tot,
            p_2001 = sum(p_2001, na.rm = TRUE)/tot,
            p_2000 = sum(p_2000, na.rm = TRUE)/tot
  ) %>% 
  mutate(Area = "NYC") %>% 
  gather(elect, Turnout, 2:36) %>% 
  mutate(Year = as.Date(str_extract(elect, "\\d{4}"), "%Y"),
        Election = str_extract(elect, "[:alpha:]")) %>% 
  select(-elect, -tot)

### all NYCDDs

turnout <- voterfile %>% 
  group_by(NYCCD) %>%  
  summarize(tot = n(),
            g_2017 = sum(g_2017, na.rm = TRUE)/tot,
            g_2016 = sum(g_2016, na.rm = TRUE)/tot,
            g_2015 = sum(g_2015, na.rm = TRUE)/tot,
            g_2014 = sum(g_2014, na.rm = TRUE)/tot,
            g_2013 = sum(g_2013, na.rm = TRUE)/tot,
            g_2012 = sum(g_2012, na.rm = TRUE)/tot,
            g_2011 = sum(g_2011, na.rm = TRUE)/tot,
            g_2010 = sum(g_2010, na.rm = TRUE)/tot,
            g_2009 = sum(g_2009, na.rm = TRUE)/tot,
            g_2008 = sum(g_2008, na.rm = TRUE)/tot,
            g_2007 = sum(g_2007, na.rm = TRUE)/tot,
            g_2006 = sum(g_2006, na.rm = TRUE)/tot,
            g_2005 = sum(g_2005, na.rm = TRUE)/tot,
            g_2004 = sum(g_2004, na.rm = TRUE)/tot,
            g_2003 = sum(g_2003, na.rm = TRUE)/tot,
            g_2002 = sum(g_2002, na.rm = TRUE)/tot,
            g_2001 = sum(g_2001, na.rm = TRUE)/tot,
            g_2000 = sum(g_2000, na.rm = TRUE)/tot,
            p_2016 = sum(p_2016, na.rm = TRUE)/tot,
            p_2015 = sum(p_2015, na.rm = TRUE)/tot,
            p_2014 = sum(p_2014, na.rm = TRUE)/tot,
            p_2013 = sum(p_2013, na.rm = TRUE)/tot,
            p_2012 = sum(p_2012, na.rm = TRUE)/tot,
            p_2011 = sum(p_2011, na.rm = TRUE)/tot,
            p_2010 = sum(p_2010, na.rm = TRUE)/tot,
            p_2009 = sum(p_2009, na.rm = TRUE)/tot,
            p_2008 = sum(p_2008, na.rm = TRUE)/tot,
            p_2007 = sum(p_2007, na.rm = TRUE)/tot,
            p_2006 = sum(p_2006, na.rm = TRUE)/tot,
            p_2005 = sum(p_2005, na.rm = TRUE)/tot,
            p_2004 = sum(p_2004, na.rm = TRUE)/tot,
            p_2003 = sum(p_2003, na.rm = TRUE)/tot,
            p_2002 = sum(p_2002, na.rm = TRUE)/tot,
            p_2001 = sum(p_2001, na.rm = TRUE)/tot,
            p_2000 = sum(p_2000, na.rm = TRUE)/tot
  ) %>% 
  mutate(Area = as.character(NYCCD)) %>%
  filter(!is.na(NYCCD)) %>% 
  gather(elect, Turnout, 3:37) %>% 
  mutate(Year = as.Date(str_extract(elect, "\\d{4}"), "%Y"),
         Election = str_extract(elect, "[:alpha:]")) %>% 
  select(-elect, -tot, -NYCCD) %>% 
  bind_rows(turnout)

###### 

presidential <- c(2016, 2012, 2008, 2004, 2000)
midterm <- c(2018, 2014, 2010, 2006, 2002)
citycouncil <- c(2017, 2013, 2009, 2005, 2001)
off <- c(2015, 2011, 2007, 2003)

turnout <- turnout %>% 
  mutate(Office = ifelse(year(Year) %in% presidential, "Presidential", 
                         ifelse(year(Year) %in% midterm, "Midterm", 
                                ifelse(year(Year) %in% citycouncil, "City Council", "Off Year"))))

p_turnout <- ggplot(subset(turnout, Area %in% pbsampledists & Office != "Off Year"), 
            aes(x = Year, y = Turnout, group = interaction(Election, Office))) +
  geom_line(aes(linetype = factor(Election, labels = c("General", "Primary")))) +
  labs(linetype = "Election",
       x = "Office",
       y = "District") +
  facet_grid(Area ~ Office) +
  theme_minimal() +
  theme(strip.placement = "outside",
        legend.margin = margin(r = 0.1, l = 0.01),
        axis.title.y.right = element_text(size = 9),
        plot.margin = margin(r = 0.1, l = 0.1, t = 0.1, b = 0.1)) +
  scale_x_date(date_labels = "%y") +
  scale_y_continuous(position = "right")


#### Demographics #####

source("censustables.R")

census <- race %>% 
  left_join(inc, by = "tract") %>% 
  left_join(educ, by = "tract")

voterfile <- voterfile %>% mutate(countycode = recode(County, BRONX = "005", KINGS = "047", `NEW YORK` = "061", QUEENS = "081", RICHMOND = "085")) %>%
  mutate(countycode = ifelse(countycode %in% c("005", "047", "061", "081", "085"), countycode, NA),
         tract = paste0(countycode, str_pad(CensusTract, 6, "left", "0")))

voterfile <- voterfile %>% 
  left_join(census, by = "tract")


p_hhinc <- ggplot(subset(voterfile, NYCCD %in% pbsampledists), 
                              aes(x = medhhinc, group = NYCCD)) +
  geom_histogram(bins = 15) +
  facet_grid(NYCCD ~ .) +
  labs(x = "Med HH Inc, tract ($)",
       y = "",
       title = " ") +
  ylim(0, 35000) +
  theme_minimal() + 
  theme(strip.text.y = element_blank(),
        axis.text.y = element_blank(),
        plot.margin = margin(r = 0.1, l = 0.1, t = 0.1, b = 0.1))

p_college <- ggplot(subset(voterfile, NYCCD %in% pbsampledists), 
                 aes(x = college/100, group = NYCCD)) +
  geom_histogram(bins = 15) +
  facet_grid(NYCCD ~ .) +
  labs(x = "College degree, tract (%)",
       y = "",
       title = " ") +
  ylim(0, 35000) +
  theme_minimal() +
  theme(strip.text.y = element_blank(),
        axis.text.y = element_blank())

p_age <- ggplot(subset(voterfile, NYCCD %in% pbsampledists), 
                    aes(x = age, group = NYCCD)) +
  geom_histogram(bins = 15) +
  facet_grid(NYCCD ~ .) +
  labs(x = "Age",
       y = "Individuals",
       title = " ") +
  ylim(0, 35000) +
  theme_minimal() +
  theme(strip.text.y = element_blank())

p_race <- voterfile %>% 
  filter(NYCCD %in% pbsampledists) %>% 
  filter(!Race %in% c("N", "U")) %>% 
  ggplot(aes(x = Race, group = NYCCD)) +
  geom_bar() +
  facet_grid(NYCCD ~ .) +
  labs(x = "Estimated Race",
       y = "",
       title = " ") +
  ylim(0, 35000) +
  scale_x_discrete(labels = c("A" = "Asian", "B" = "Black", "H" = "Hispanic", "W" = "White")) +
  theme_minimal() +
  theme(strip.text.y = element_blank(),
        axis.text.y = element_blank())


library(ggpubr)

p <- ggarrange(plotlist = list(p_age, p_college, p_race, p_hhinc, p_turnout), 
               nrow = 1, ncol = 5,
               widths = c(.8, .8, .8, .8, 1.4))
ggsave(file = "descriptives.png", p, width = 12, height = 8)
p
turnout <- voterfile %>% 
  group_by(NYCCD) %>% 
  summarize(tot = n(),
            turnout = )
  ?unit
## PB v. Non-PB districts



# increase district presidential elections

test <- voterfile %>%
  split(.$NYCCD)%>%
  mutate(desc = map(summary))

test1 <- test %>% 
  unnest()
test$`1`

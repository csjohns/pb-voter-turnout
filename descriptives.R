library(tidyverse)
library(lubridate)
library(gridExtra)
library(grid)
library(glue)
library(scales)

#### Load full voterfile data
# voterfile <- readRDS(paste0("data/cleaned_R_results/voterfile_for_matching.rds"))
voterfile <- readRDS("data/cleaned_R_results/voterfile_full_clean.rds")

# filter to appropriate population
compare_districts <- c(23, 39, 30,35,36,40)
pbdistricts <- readRDS("data/cleaned_R_results/pbdistricts.rds")

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

#### PB Districts

turnout <- voterfile %>% 
  filter(NYCCD %in% pbdistricts) %>% 
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
  mutate(Area = "PB Districts") %>% 
  gather(elect, Turnout, 2:36) %>% 
  mutate(Year = as.Date(str_extract(elect, "\\d{4}"), "%Y"),
         Election = str_extract(elect, "[:alpha:]")) %>% 
  select(-elect, -tot) %>% 
  bind_rows(turnout)

### PB Voters


turnout <- voterfile %>% 
  filter(pb ==1) %>% 
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
  mutate(Area = "PB Voters") %>% 
  gather(elect, Turnout, 2:36) %>% 
  mutate(Year = as.Date(str_extract(elect, "\\d{4}"), "%Y"),
         Election = str_extract(elect, "[:alpha:]")) %>% 
  select(-elect, -tot) %>% 
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

p_turnout <- ggplot(subset(turnout, Office != "Off Year"), 
            aes(x = Year, y = Turnout, group = interaction(Area, Office))) +
  geom_line(aes(linetype = Area, color = Area)) +
  labs(color = "Voter group",
       linetype = "Voter group",
       x = "Year",
       y = "Turnout") +
  facet_grid(factor(Election, labels = c("General", "Primary")) ~ Office) +
  theme_minimal() +
  theme(legend.position = "right",
        panel.spacing = unit(1, "lines")) +
  scale_x_date(date_labels = "%y") +
  scale_y_continuous(position = "left")

pdf(file = "Paper_text/Figs/turnout.pdf", height = 4, width = 7)
p_turnout
dev.off()

#plot.margin = margin(r = 0.4, l = 0.4, t = 0.1, b = 0.1) taken from above plot from pre-sizing adjustments

#### Demographics #####

## Age

p1 <- ggplot(voterfile, 
                aes(x = age)) +
  geom_histogram(bins = 15, fill = "#619CFF") +
  labs(x = "",
       y = "",
       title = " ") +
  coord_cartesian(xlim = c(0, 110),
                  ylim = c(0, 2000000)) +
  scale_y_continuous(labels = c(0, 500, 1000, 1500, 2000)) +
  scale_x_continuous(breaks = c(0, 15, 30, 45, 60, 75, 90, 105)) +
  theme_minimal() +
  theme(strip.text.y = element_blank(),
        strip.text.x = element_blank(),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

p2 <- voterfile %>% 
  filter(NYCCD %in% pbdistricts) %>% 
  ggplot(aes(x = age)) +
  geom_histogram(bins = 15, fill = "#00BA38") +
  labs(x = "",
       y = "",
       title = " ") +
  coord_cartesian(xlim = c(0, 110),
                  ylim = c(0, 1200000)) +
  scale_y_continuous(labels = c(0, 250, 500, 750, 1000, 1250)) +
  scale_x_continuous(breaks = c(0, 15, 30, 45, 60, 75, 90, 105)) +
  theme_minimal() +
  theme(strip.text.y = element_blank(),
        strip.text.x = element_blank(),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

p3 <- voterfile %>% filter(pb == 1) %>% 
  ggplot(aes(x = age)) +
  geom_histogram(bins = 15, fill = "#F8766D") +
  labs(x = "Age",
       y = "",
       title = " ") +
  coord_cartesian(xlim = c(0, 110),
                  ylim = c(0, 8000)) +
  scale_y_continuous(labels = c(0, 2, 4, 6, 8)) +
  scale_x_continuous(breaks = c(0, 15, 30, 45, 60, 75, 90, 105)) +
  theme_minimal() +
  theme(strip.text.y = element_blank())


pdf(file = "Paper_text/Figs/age2.pdf", height = 3.5, width = 2.75)

# grid.newpage()
grid.arrange(rbind(ggplotGrob(p1), ggplotGrob(p2), ggplotGrob(p3), size = "last"), left = "Persons (in 1,000s)")
# grid.draw(rbind(ggplotGrob(p1), ggplotGrob(p2), ggplotGrob(p3), size = "last"))
dev.off()

## College degree
p1 <- ggplot(voterfile, 
                    aes(x = college/100)) +
  geom_histogram(bins = 15, fill = "#619CFF") +
  labs(x = "College degree, tract (%)",
       y = "",
       title = " ") +
  coord_cartesian(xlim = c(0, 1),
                  ylim = c(0, 2000000)) +
  scale_y_continuous(labels = comma) +
  theme_minimal() +
  theme(strip.text.y = element_blank(),
        strip.text.x = element_blank(),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

p2 <- voterfile %>% 
  filter(NYCCD %in% pbdistricts) %>% 
  ggplot(aes(x = college/100)) +
  geom_histogram(bins = 15, fill = "#00BA38") +
  labs(x = "College degree, tract (%)",
       y = "",
       title = " ") +
  coord_cartesian(xlim = c(0,1),
                  ylim = c(0,1200000)) +
  scale_y_continuous(labels = comma) +
  theme_minimal() +
  theme(strip.text.y = element_blank(),
        strip.text.x = element_blank(),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

p3 <-  voterfile %>% filter(pb == 1) %>% 
  ggplot(aes(x = college/100)) +
  geom_histogram(bins = 15, fill = "#F8766D") +
  labs(x = "College degree, tract (%)",
       y = "",
       title = " ") +
  coord_cartesian(xlim = c(0,1),
                  ylim = c(0,8000)) +
  scale_y_continuous(labels = comma) +
  #scale_x_continuous(breaks = c(0, 15, 30, 45, 60, 75, 90, 105)) +
  theme_minimal() +
  theme(strip.text.y = element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

pdf(file = "Paper_text/Figs/college2.pdf", height = 3.5, width = 2.75)

grid.newpage()
grid.draw(rbind(ggplotGrob(p1), ggplotGrob(p2), ggplotGrob(p3), size = "last"))
dev.off()

## Household Income

p1 <- ggplot(voterfile, aes(x = medhhinc)) +
  geom_histogram(bins = 15, fill = "#619CFF") +
  labs(x = "",
       y = "",
       title = " ") +
  coord_cartesian(xlim = c(0, 250000),
                  ylim = c(0,2000000)) +
  scale_y_continuous(labels = comma) +
  theme_minimal() +
  theme(strip.text.y = element_blank(),
        strip.text.x = element_blank(),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

p2 <- voterfile %>% 
  filter(NYCCD %in% pbdistricts) %>% 
  ggplot(aes(x = medhhinc)) +
  geom_histogram(bins = 15, fill = "#00BA38") +
  labs(x = "",
       y = "",
       title = " ") +
  coord_cartesian(xlim = c(0,250000),
                  ylim = c(0,1200000)) +
  scale_y_continuous(labels = comma) +
  theme_minimal() +
  theme(strip.text.y = element_blank(),
        strip.text.x = element_blank(),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

p3 <-  voterfile %>% filter(pb == 1) %>% 
  ggplot(aes(x = medhhinc)) +
  geom_histogram(bins = 15, fill = "#F8766D") +
  labs(x = "Median Household Income ($)",
       y = "",
       title = " ") +
  coord_cartesian(xlim = c(0, 250000),
                  ylim = c(0,8000)) +
  scale_y_continuous(labels = comma) +
  #scale_x_continuous(breaks = c(0, 15, 30, 45, 60, 75, 90, 105)) +
  theme_minimal() +
  theme(strip.text.y = element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

pdf(file = "Paper_text/Figs/hhinc2.pdf", height = 3.5, width = 2.75)

grid.newpage()
grid.draw(rbind(ggplotGrob(p1), ggplotGrob(p2), ggplotGrob(p3), size = "last"))
dev.off()

## Race


p1 <- voterfile %>% 
  filter(Race != "N") %>% 
  ggplot(aes(x = Race)) +
  geom_bar(fill = "#619CFF") +
  labs(x = "",
       y = "NYC",
       title = " ") +
  coord_cartesian(ylim = c(0,2000000)) +
  scale_y_continuous(labels = comma, position = "right") +
  scale_x_discrete(labels = c("A" = "Asian", "B" = "Black", "H" = "Hisp.", "U" = "Unk.", "W" = "White")) +
  theme_minimal() +
  theme(strip.text.y = element_blank(),
        strip.text.x = element_blank(),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

p2 <- voterfile %>% 
  filter(NYCCD %in% pbdistricts) %>% 
  filter(Race != "N") %>% 
  ggplot(aes(x = Race)) +
  geom_bar(fill = "#00BA38") +
  labs(x = "Estimated Race",
       y = "PB Districts",
       title = " ") +
  coord_cartesian(ylim = c(0,1200000)) +
  scale_y_continuous(labels = comma, position = "right") +
  scale_x_discrete(labels = c("A" = "Asian", "B" = "Black", "H" = "Hisp.", "U" = "Unk.", "W" = "White")) +
  theme_minimal() +
  theme(strip.text.y = element_blank(),
        strip.text.x = element_blank(),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

p3 <- voterfile %>% filter(pb == 1) %>% 
  filter(Race != "N") %>% 
  ggplot(aes(x = Race)) +
  geom_bar(fill = "#F8766D") +
  labs(x = "Estimated Race",
       y = "PB Voters",
       title = " ") +
  coord_cartesian(ylim = c(0,8000)) +
  scale_y_continuous(labels = comma, position = "right") +
  scale_x_discrete(labels = c("A" = "Asian", "B" = "Black", "H" = "Hisp.", "U" = "Unk.", "W" = "White")) +
  theme_minimal() +
  theme(strip.text.y = element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

pdf(file = "Paper_text/Figs/race2.pdf", height = 3.5, width = 2.75)

grid.newpage()
grid.draw(rbind(ggplotGrob(p1), ggplotGrob(p2), ggplotGrob(p3), size = "last"))
dev.off()

# library(ggpubr)
# 
# 
# p <- ggarrange(plotlist = list(p_age, p_college, p_race, p_hhinc, p_turnout), 
#                nrow = 1, ncol = 5,
#                widths = c(.8, .8, .8, .8, 1.4))
# ggsave(file = "descriptives.png", p, width = 12, height = 8)

#### PB VOTE COUNTS ####

pbvotes <- read_csv("pbnyc_district_votes.csv")

pbvotes <- pbvotes %>% filter(voters != 0)

p <- ggplot(pbvotes, aes(x = as.factor(voteYear), y = voters)) +
  geom_point(alpha = 0.35) +
  labs(x = "Vote Year",
       y = "District PB Vote Count") +
  scale_y_continuous(labels = comma) +
  theme_minimal()

pdf(file = "Paper_text/Figs/districtvotes.pdf", height = 3.5, width = 4.5)
p
dev.off()

pbvotes %>% group_by(pbnycCycle) %>% summarize(avg = mean(voters, na.rm = TRUE),
                                               tot = n())

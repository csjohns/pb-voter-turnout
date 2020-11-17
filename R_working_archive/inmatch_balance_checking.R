#################################################################################################################################################
### 
### PB Voter Turnout Analysis
### Comparing matched to non-matched & matched PB to Non-PB
###
### Created by: Carolina Johnson
### Created date: 10/16/2018
###
#################################################################################################################################################
library(dplyr)
library(tidyr)
library(ggplot2)
library(cobalt)
library(stringr)

load("matchcompare_gis.RData")



all_pb <- voterfile %>% 
  filter(pb == 1) %>% 
  filter(!is.na(g_2014_comp) & !is.na(g_2016_comp) & !is.na(p_2014_comp) & !is.na(pp_2016_comp) & !is.na(high_school) & !is.na(majmatch)) %>% 
  left_join(., dplyr::select(c.college, VANID, pb_match = pb, cem_group)) %>% 
  mutate(inmatch = as.numeric(!is.na(cem_group))) %>% 
  dplyr::select(-cem_group, -pb_match, -agegroup, -DoR, -Ethnicity, -ED, -County, -countycode, -tract, -City, -State, 
                -CensusTract, -RegStatus, -starts_with("pb_2"), -Zip, -CD, -SD, -HD, -DoB,
                -black, -asian, -pacislander, -latinx, -mixed, -other,
                -majority, -pbdistrict, -p_2011, -ends_with("_2012"), -ends_with("_2013"), -ends_with("_2014"), -ends_with("_2015"), -ends_with("_2016"), -ends_with("_2017"))
# all_pb %>% filter(VANID %in% all_pb$VANID[duplicated(all_pb$VANID)]) %>% dplyr:::select(VANID, pb, cem_group, pb_match, race, n_treat, n_control) %>% arrange(VANID) %>% View
# voterfile %>% filter(VANID %in% voterfile$VANID[duplicated(voterfile$VANID)]) %>% dplyr:::select(VANID, pb) %>% arrange(VANID) %>% View

vnames <- data.frame(old = names(all_pb), new = names(all_pb)) %>% 
  mutate(new = str_replace_all(new, pattern = "g_", replacement = "General "),
         new = str_replace_all(new, pattern = "pp_", replacement = "Presidential primary "),
         new = str_replace_all(new, pattern = "p_", replacement = "Primary "),
         new = ifelse(new == "g_early", "General elections 2000-2007", new),
         new = ifelse(new == "p_early", "Primary elections 2000-2007", new),
         new = ifelse(new == "high_school", "% High school diploma", new),
         new = ifelse(new == "college", "% College degree", new),
         new = ifelse(new == "medhhinc", "% Median household income", new),
         new = ifelse(new == "white", "% White", new),
         # new = ifelse(new == "black", "% Black", new),
         # new = ifelse(new == "asian", "% Asian", new),
         # new = ifelse(new == "pacislander", "% Pacific Islander", new),
         # new = ifelse(new == "latinx", "% Latinx", new),
         # new = ifelse(new == "mixed", "% Multiple races", new),
         # new = ifelse(new == "other", "% Other race", new),
         new = ifelse(new == "majmatch", "Majority race alignment", new),
         new = ifelse(new == "age", "Age", new),
         new = ifelse(new == "comp_g_2014", "Competitiveness 2014 general", new),
         new = ifelse(new == "comp_g_2016", "Competitiveness 2016 general", new),
         new = ifelse(new == "comp_p_2014", "Competitiveness 2014 primary", new),
         new = ifelse(new == "comp_pp_2016", "Competitiveness 2016 pres. primary", new)
  )


cbtout <- dplyr::select(all_pb, -VANID, -NYCCD, -pb, -inmatch) %>% 
  mutate(Sex = as.factor(Sex),
         Race = as.factor(Race)) %>% 
  bal.tab(treat = "inmatch", data = all_pb)
bal.plot(dplyr::select(all_pb,  -VANID, -NYCCD, -pb, -inmatch), treat = "inmatch", data = all_pb)

love.plot(cbtout, threshold = .1, abs = FALSE, var.names = vnames, title = "") + theme(legend.position = "none")       
ggsave("Paper/Figs/pb_match_compare.pdf", width = 5, height = 7, units = "in")

covo <- dplyr::select(all_pb, -VANID, -NYCCD, -pb)
for (varname in names(covo)){
  print(bal.plot(covo, treat = "inmatch", data = all_pb, var.name = varname))
}

ggplot(all_pb) + geom_bar(aes(x = Sex, fill = as.factor(inmatch)), position = "dodge")
ggplot(all_pb) + geom_bar(aes(x = as.factor(NYCCD), fill = as.factor(inmatch)), position = "dodge")

#looks like we have a harder time matching older, non-white/minority race historically more frequent voters, 


### comparing match patterns among specific demographics of pb voters: ------------------------------------------------------
extractGroupBal <- function(cv, tt = "inmatch",  group, df){
  cv[[tt]] <- NULL
  cv[[group]] <- NULL
  print(unique(df[[group]]))
  print(names(df))
  bt <- bal.tab(cv, treat = tt, data = df)
  bt$Balance %>% 
    dplyr::select(Diff.Un) %>% #M.0.Un, M.1.Un, Diff.Un, M.0.Adj, M.1.Adj, Diff.Adj) %>% 
    tibble::rownames_to_column("variable")
}
all_pb %>% filter(Race == "H") %>% 
  summary

cbtout_byrace <- dplyr::select(all_pb, -VANID, -NYCCD, -pb) %>% 
  filter(Race %in% c("A", "B", "W")) %>% 
  mutate(Sex = as.factor(Sex),
         Race = as.factor(Race)) %>% 
  group_by(Race) %>% 
  do(extractGroupBal(cv = ., tt = "inmatch", group = "Race", df = .)) 

ggplot(cbtout_byrace, aes(x = Diff.Un, y = variable, color = Race)) + geom_point() +
  geom_segment(aes())# + facet_wrap(~Race)

all_pb %>% 
  ggplot() + geom_bar(aes(x = g_early, fill = as.factor(inmatch)), position = "dodge") + facet_wrap(~Race)
voterfile%>% 
  ggplot() + geom_density(aes(x = g_early, fill = as.factor(pb)), position = "dodge") + facet_grid(pb~Race, scales = "free")
all_pb %>% 
  ggplot() + geom_bar(aes(x = p_early, fill = as.factor(inmatch)), position = "dodge") + facet_wrap(~Race)
voterfile%>% 
  ggplot() + geom_density(aes(x = p_early, fill = as.factor(pb)), position = "dodge") + facet_grid(pb~Race, scales = "free")

### Observations about the match - basically, the proportion of established voters is higher among PB voters of all races
## than among non-PB voters. This probem is particularly acute for non-white voters, where the population of potential 
##  matches for established voters is much smaller, making it harder to identify an exact match for every non-white 
## PB voter.  In effect, new or less-experienced voters are overrepresesnted in our analytic sample, especialy for non-white
## voters.  This effect is amplified for primary votes.  It is echoed a bit by the fact that PB voters tend to be slightly
## older than the general population of registered voters.


all_pb %>% 
  ggplot() + geom_density(aes(x = age, fill = as.factor(inmatch)), alpha = .5) + facet_wrap(~Race)
voterfile%>% 
  ggplot() + geom_density(aes(x = age, fill = as.factor(pb)), alpha = .5) + facet_grid(pb~Race, scales = "free")
voterfile %>% 
  group_by(Race, pb) %>% summarize(meanage = median(age, na.rm = T))
all_pb %>% 
  group_by(Race, inmatch) %>% summarize(meanage = median(age, na.rm = T))

raceres <- data.frame()
for (r in c("A", "B", "W")) {
  ronly <- all_pb %>% 
    filter(Race == r) %>% 
    dplyr::select(-Race)
  matchbal <- ronly %>% 
    dplyr::select(-VANID, -NYCCD, -pb, -inmatch, -Sex) %>% 
    bal.tab(treat = "inmatch", data = ronly)
  raceres <- matchbal$Balance %>% 
    tibble::rownames_to_column("variable") %>% 
    select(variable, M.0.Un, M.1.Un, Diff.Un) %>% 
    bind_rows(raceres, .)
}
  
  
  bal.tabtTreat = "inmatch", data = .)
  head(., 2)
  
  extractBal()
  
  group_by(Race) %>% 
  do(extractBal(., "inmatch"))
  
  bal.tab(treat = "inmatch", data = all_pb)
bal.plot(dplyr::select(all_pb,  -VANID, -NYCCD, -pb, -inmatch), treat = "inmatch", data = all_pb)



#### Looking at pb vs non-PB post  matching:------------------------------------------------------------------
vf_matched <- c.college %>% #dplyr::select(-n_treat, -n_control) %>% 
  # mutate(insample = 1) %>% 
  left_join(voterfile) %>%
  group_by() %>% 
  # mutate(insample = replace_na(insample, 0)) %>% 
  group_by() %>% 
  # rename(comp_g_2016 = g_2016_comp, comp_g_2014 = g_2014_comp, comp_p_2014 = p_2014_comp, comp_pp_2016 = pp_2016_comp) %>% 
  dplyr::select(-cem_group, -pb_match, -agegroup, -DoR, -Ethnicity, -ED, -County, -countycode, -tract, -City, -State, 
                -CensusTract, -countycodeupdate, -CensusTractUpdate, -tractcode, -RegStatus, -starts_with("pb_2"), -Zip, -CD, -SD, -HD, -DoB,
                -black, -asian, -pacislander, -latinx, -mixed, -other,
                -pbdistrict, -p_2011)%>% 
  dplyr::select(-VANID, -NYCCD) %>% 
  mutate(Sex = as.factor(Sex),
         Race = as.factor(Race)) 
gc()

system.time(vfout <- bal.tab(vf_matched, treat = "pb",  data = vf_matched))
bal.plot(vf_matched, "comp_2017_general", treat = "pb", data = vf_matched, mirror = T, na.rm = T)
bal.plot(vf_matched, "comp_2014_general", treat = "pb", data = vf_matched, mirror = T, na.rm = T)
bal.plot(vf_matched, "comp_2014_primary", treat = "pb", data = vf_matched, mirror = T, na.rm = T)
bal.plot(vf_matched, "comp_2013_primary", treat = "pb", data = vf_matched, mirror = T, na.rm = T)
bal.plot(vf_matched, "comp_2013_general", treat = "pb", data = vf_matched, mirror = T, na.rm = T)
bal.plot(vf_matched, "high_school", treat = "pb", data = vf_matched, mirror = T)
bal.plot(vf_matched, "college", treat = "pb", data = vf_matched, mirror = T)
bal.plot(vf_matched, "medhhinc", treat = "pb", data = vf_matched, mirror = T)
bal.plot(vf_matched, "latinx", treat = "pb", data = vf_matched, mirror = T)
bal.plot(vf_matched, "incumbent_2017", treat = "pb", data = vf_matched, mirror =)

love.plot(vfout, threshold = .1, abs = FALSE)     




#### Looking at pb vs non-PB pre  matching:--------------------------------------------------------------

test <- c.college %>% dplyr::select(-n_treat, -n_control) %>% 
  mutate(insample = 1) %>% 
  full_join(voterfile) %>%
  group_by() %>% 
  mutate(insample = replace_na(insample, 0)) %>% 
  group_by() %>% 
  rename(comp_g_2016 = g_2016_comp, comp_g_2014 = g_2014_comp, comp_p_2014 = p_2014_comp, comp_pp_2016 = pp_2016_comp) %>% 
  dplyr::select(-DoR, -Ethnicity, -ED, -County, -countycode, -tract, -City, -State, 
                -CensusTract, -RegStatus, -starts_with("pb_2"), -Zip, -CD, -SD, -HD, -DoB,
                -black, -asian, -pacislander, -latinx, -mixed, -other,
                -majority, -pbdistrict, -g_2012, -g_2013, -g_2014, -g_2015, -g_2016, -g_2017, 
                -p_2012, -p_2013, -p_2014, -p_2015, -p_2016, -p_2017, -pp_2012, -pp_2016, -cem_group, -agegroup,
                -race) %>% 
  dplyr::select(-VANID, -NYCCD) %>% 
  mutate(Sex = as.factor(Sex),
         Race = as.factor(Race)) 
gc()
test <- test[c(sample(which(test$pb != 1 | test$insample != 1), 100000), which(test$pb == 1 | test$insample == 1)), ] %>% na.omit()

system.time(unout <- test %>% 
              select(-pb, -insample, -p_2011) %>% 
              bal.tab(treat = "pb",  data = test, weights = "insample", method = "matching", disp.means = TRUE, un = TRUE, quick = TRUE))

bal.plot(test, "comp_g_2016", treat = "pb", data = test, mirror = T,weights = "insample", method = "matching", which= "both")
bal.plot(test, "comp_g_2014", treat = "pb", data = test, mirror = T, weights = "insample", method = "matching", which= "both")
bal.plot(test, "comp_p_2014", treat = "pb", data = test, mirror = T, weights = "insample", method = "matching", which= "both")
bal.plot(test, "comp_pp_2016", treat = "pb", data = test, mirror = T, weights = "insample", method = "matching", which= "both")
bal.plot(test, "high_school", treat = "pb", data = test, mirror = T, weights = "insample", method = "matching", which= "both")
bal.plot(test, "college", treat = "pb", data = test, mirror = T,weights = "insample", method = "matching", which= "both", na.rm = T)
bal.plot(test, "medhhinc", treat = "pb", data = test, mirror = T,weights = "insample", method = "matching", which= "both")
bal.plot(test, "latinx", treat = "pb", data = test, mirror = T,weights = "insample", method = "matching", which= "both")
  
love.plot(unout, threshold = .1, abs = FALSE, var.names = vnames, title = "", line = T) +theme(legend.position = "bottom")
ggsave("Paper/Figs/match_balance.pdf", width = 6, height = 7.5, units = "in")     

extractGroupBal <- function(cv, tt = "inmatch",  group, df){
  cv[[tt]] <- NULL
  cv[[group]] <- NULL
  print(unique(df[[group]]))
  print(names(df))
  bt <- bal.tab(cv, treat = tt, data = df)
  bt$Balance %>% 
    dplyr::select(M.0.Un, M.1.Un, Diff.Un) %>% 
    tibble::rownames_to_column("variable")
  }
  
unout_byrace <- test %>% filter(insample == 1) %>% 
  filter(Race %in% c("A", "B", "W")) %>% 
  mutate(Sex = as.factor(Sex),
         Race = as.factor(Race)) %>% 
  group_by(Race) %>% 
  do(extractGroupBal(cv = ., tt = "pb", group = "Race", df = .))

ggplot(cbtout_byrace) + geom_point(aes(x = Diff.Un, y = variable, color = Race))# + facet_wrap(~Race)

raceout <- list()
for (r in c("A" ,"B", "W")){

  raceout[[r]] <- test %>% filter(Race == r) %>% 
    select(  -p_2011)  %>% 
    bal.tab(., treat = "pb",  data = ., weights = "insample", method = "matching", disp.means = TRUE, un = TRUE, quick = TRUE)
  
  love.plot(raceout[[r]], threshold = .1, abs = FALSE, var.names = vnames, title = r, line = T) +theme(legend.position = "bottom")
}

love.plot(raceout[["A"]], threshold = .1, abs = FALSE, var.names = vnames, title = "A", line = T) +theme(legend.position = "bottom")
love.plot(raceout[["B"]], threshold = .1, abs = FALSE, var.names = vnames, title = "B", line = T) +theme(legend.position = "bottom")
love.plot(raceout[["W"]], threshold = .1, abs = FALSE, var.names = vnames, title = "W", line = T) +theme(legend.position = "bottom")


## competitiveness balance
library(gridExtra)
p1 <- bal.plot(test, "comp_g_2016", treat = "pb", data = test, mirror = T,weights = "insample", method = "matching", which= "both") + labs(x = "2016 General Election Margin of Victory", title = "") + theme_bw() + theme(legend.position = "none") 
p2 <- bal.plot(test, "comp_g_2014", treat = "pb", data = test, mirror = T, weights = "insample", method = "matching", which= "both") + labs(x = "2014 General Election Margin of Victory", title = "") + theme_bw() +  theme(legend.position = "none")
p3 <- bal.plot(test, "comp_p_2014", treat = "pb", data = test, mirror = T, weights = "insample", method = "matching", which= "both") + labs(x = "2016 Presidential Primary Margin of Victory", title = "") + theme_bw() +  theme(legend.position = "none")
p4 <- bal.plot(test, "comp_pp_2016", treat = "pb", data = test, mirror = T, weights = "insample", method = "matching", which= "both")+ labs(x = "2014 Primary Margin of Victory", title = "") + theme_bw() +  theme(legend.position = "none")
allcomp <- grid.arrange(p1, p2, p3, p4, nrow = 4)
ggsave("Paper/Figs/competitiveness.pdf", allcomp, width = 6, height = 8)

voterfile %>% group_by(pb) %>% summarize_at(vars(ends_with("_comp")), mean, na.rm = T)
vf_matched %>% group_by(pb) %>% summarize_at(vars(starts_with("comp_")), mean, na.rm = T)

voterfile %>% select(ends_with("_comp")) %>% summary()

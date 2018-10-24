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
library(cobalt)

load("matchcompare_gis.RData")

all_pb <- voterfile %>% 
  filter(pb == 1) %>% 
  filter(!is.na(g_2014_comp) & !is.na(g_2016_comp) & !is.na(p_2014_comp) & !is.na(pp_2016_comp)) %>% 
  left_join(., dplyr::select(c.college, VANID, pb_match = pb, cem_group)) %>% 
  mutate(inmatch = as.numeric(!is.na(cem_group))) %>% 
  dplyr::select(-cem_group, -pb_match, -agegroup, -DoR, -Ethnicity, -ED, -County, -countycode, -tract, -City, -State, 
                -CensusTract, -RegStatus, -starts_with("pb_2"), -Zip, -CD, -SD, -HD, -DoB,
                -majority, -pbdistrict)
# all_pb %>% filter(VANID %in% all_pb$VANID[duplicated(all_pb$VANID)]) %>% dplyr:::select(VANID, pb, cem_group, pb_match, race, n_treat, n_control) %>% arrange(VANID) %>% View
# voterfile %>% filter(VANID %in% voterfile$VANID[duplicated(voterfile$VANID)]) %>% dplyr:::select(VANID, pb) %>% arrange(VANID) %>% View



cbtout <- dplyr::select(all_pb, -VANID, -NYCCD, -pb) %>% 
  mutate(Sex = as.factor(Sex),
         Race = as.factor(Race)) %>% 
  bal.tab(treat = "inmatch", data = all_pb)
bal.plot(dplyr::select(all_pb,  -VANID, -NYCCD, -pb), treat = "inmatch", data = all_pb)

love.plot(cbtout, threshold = .1, abs = FALSE)         

covo <- dplyr::select(all_pb, -VANID, -NYCCD, -pb)
for (varname in names(covo)){
  print(bal.plot(covo, treat = "inmatch", data = all_pb, var.name = varname))
}

ggplot(all_pb) + geom_bar(aes(x = Sex, fill = as.factor(inmatch)), position = "dodge")
ggplot(all_pb) + geom_bar(aes(x = as.factor(NYCCD), fill = as.factor(inmatch)), position = "dodge")

#looks like we have a harder time matching older, non-white/minority race historically more frequent voters, 
  

#### Looking at pb vs non-PB post  matching:------------------------------------------------------------------
vf_matched <- c.college %>% dplyr::select(-n_treat, -n_control) %>% 
  # mutate(insample = 1) %>% 
  left_join(voterfile) %>%
  group_by() %>% 
  # mutate(insample = replace_na(insample, 0)) %>% 
  group_by() %>% 
  rename(comp_g_2016 = g_2016_comp, comp_g_2014 = g_2014_comp, comp_p_2014 = p_2014_comp, comp_pp_2016 = pp_2016_comp) %>% 
  dplyr::select(-DoR, -Ethnicity, -ED, -County, -countycode, -tract, -City, -State, 
                -CensusTract, -RegStatus, -starts_with("pb_2"), -Zip, -CD, -SD, -HD, -DoB,
                -majority, -pbdistrict, -g_2012, -g_2013, -g_2014, -g_2015, -g_2016, -g_2017, 
                -p_2012, -p_2013, -p_2014, -p_2015, -p_2016, -p_2017, -pp_2012, -pp_2016, -cem_group, -agegroup,
                -race) %>% 
  dplyr::select(-VANID, -NYCCD) %>% 
  mutate(Sex = as.factor(Sex),
         Race = as.factor(Race)) 
gc()

system.time(vfout <- bal.tab(vf_matched, treat = "pb",  data = vf_matched))
bal.plot(vf_matched, "comp_g_2016", treat = "pb", data = vf_matched, mirror = T)
bal.plot(vf_matched, "comp_g_2014", treat = "pb", data = vf_matched, mirror = T)
bal.plot(vf_matched, "comp_p_2014", treat = "pb", data = vf_matched, mirror = T)
bal.plot(vf_matched, "comp_pp_2016", treat = "pb", data = vf_matched, mirror = T)
bal.plot(vf_matched, "high_school", treat = "pb", data = vf_matched, mirror = T)
bal.plot(vf_matched, "college", treat = "pb", data = vf_matched, mirror = T)
bal.plot(vf_matched, "medhhinc", treat = "pb", data = vf_matched, mirror = T)
bal.plot(vf_matched, "latinx", treat = "pb", data = vf_matched, mirror = T)

love.plot(vfout, threshold = .1, abs = FALSE)     




#### Looking at pb vs non-PB pre  matching:--------------------------------------------------------------

test <- c.granular %>% dplyr::select(-n_treat, -n_control) %>% 
  mutate(insample = 1) %>% 
  full_join(voterfile) %>%
  group_by() %>% 
  mutate(insample = replace_na(insample, 0)) %>% 
  group_by() %>% 
  rename(comp_g_2016 = g_2016_comp, comp_g_2014 = g_2014_comp, comp_p_2014 = p_2014_comp, comp_pp_2016 = pp_2016_comp) %>% 
  dplyr::select(-DoR, -Ethnicity, -ED, -County, -countycode, -tract, -City, -State, 
                -CensusTract, -RegStatus, -starts_with("pb_2"), -Zip, -CD, -SD, -HD, -DoB,
                -majority, -pbdistrict, -g_2012, -g_2013, -g_2014, -g_2015, -g_2016, -g_2017, 
                -p_2012, -p_2013, -p_2014, -p_2015, -p_2016, -p_2017, -pp_2012, -pp_2016, -cem_group, -agegroup,
                -race) %>% 
  dplyr::select(-VANID, -NYCCD) %>% 
  mutate(Sex = as.factor(Sex),
         Race = as.factor(Race)) 
gc()
test <- test[c(sample(which(test$pb != 1 | test$insample != 1), 50000), which(test$pb == 1 | test$insample == 1)), ] %>% na.omit()

system.time(unout <- bal.tab(test, treat = "pb",  data = test, weights = "insample", method = "matching", disp.means = TRUE, un = TRUE, quick = TRUE))
bal.plot(test, "comp_g_2016", treat = "pb", data = test, mirror = T )
bal.plot(test, "comp_g_2014", treat = "pb", data = test, mirror = T, weights = "insample", method = "matching", which= "both")
bal.plot(test, "comp_p_2014", treat = "pb", data = test, mirror = T, weights = "insample", method = "matching", which= "both")
bal.plot(test, "comp_pp_2016", treat = "pb", data = test, mirror = T, weights = "insample", method = "matching", which= "both")
bal.plot(test, "high_school", treat = "pb", data = test, mirror = T)
bal.plot(test, "college", treat = "pb", data = test, mirror = T)
bal.plot(test, "medhhinc", treat = "pb", data = test, mirror = T)
bal.plot(test, "latinx", treat = "pb", data = test, mirror = T)
  
love.plot(unout, threshold = .1, abs = FALSE)     


### function to do this myself:


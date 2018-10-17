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
  left_join(., dplyr::select(c.college, VANID, pb_match = pb, cem_group)) %>% 
  mutate(inmatch = as.numeric(!is.na(cem_group))) %>% 
  dplyr::select(-cem_group, -pb_match, -agegroup, -DoR, -Ethnicity, -ED, -County, -countycode, -tract, -City, -State, 
                -CensusTract, -RegStatus, -starts_with("pb_2"), -Zip, -CD, -SD, -HD, -DoB)
# all_pb %>% filter(VANID %in% all_pb$VANID[duplicated(all_pb$VANID)]) %>% dplyr:::select(VANID, pb, cem_group, pb_match, race, n_treat, n_control) %>% arrange(VANID) %>% View
# voterfile %>% filter(VANID %in% voterfile$VANID[duplicated(voterfile$VANID)]) %>% dplyr:::select(VANID, pb) %>% arrange(VANID) %>% View


cbtout <- bal.tab(dplyr::select(all_pb, -Sex, -Race, -majority, -VANID, -NYCCD, -pb), treat = "inmatch", data = all_pb)
bal.plot(dplyr::select(all_pb, -Sex, -Race, -majority, -VANID, -NYCCD, -pb), treat = "inmatch", data = all_pb)

love.plot(cbtout, threshold = .1)         

covo <- dplyr::select(all_pb, -Sex, -Race, -majority, -VANID, -NYCCD, -pb, -pbdistrict)
for (varname in names(covo)){
  print(bal.plot(covo, treat = "inmatch", data = all_pb, var.name = varname))
}

  ggplot(all_pb) + geom_bar(aes(x = Sex, fill = as.factor(inmatch)), position = "dodge")
  ggplot(all_pb) + geom_bar(aes(x = as.factor(NYCCD), fill = as.factor(inmatch)), position = "dodge")

#looks like we have a harder time matching older, non-white/minority race historically more frequent voters, 
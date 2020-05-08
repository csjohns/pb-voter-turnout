## Adding districts back in to the constructed pb table 


nyccds <- read.csv("ed-nyccd-map.csv", as.is = TRUE)
nyccds <- nyccds %>% 
  mutate(ED = paste0("Ad ", str_sub(ElectDist, 1, 2), " - Ed ", str_sub(ElectDist, 3,5)))

pb <- pb %>% 
  filter(County %in% c("BRONX", "KINGS", "NEW YORK", "QUEENS", "RICHMOND")) %>% 
  mutate(NYCCD = ifelse(NYCCD == "", NA, NYCCD),
         ED = ifelse(ED == "", NA, ED))

nyccds <- full_join(nyccds, data.frame(ED = as.vector(na.omit(unique(pb$ED))), stringsAsFactors = FALSE))

for (i in 1:nrow(pb)){
  if(is.na(pb$NYCCD[i])){
    pb$NYCCD[i] <- sample(nyccds$CounDist[nyccds$ED == pb$ED[i]], 1)
  }
}

pbnyc_history <- read.csv(file = "pbnyc_district_votes.csv", as.is = TRUE) %>% 
  rename(pbdistrict = district) %>%
  group_by(pbdistrict) %>%
  summarize(start_year = min(voteYear)) %>% 
  filter(start_year < 2017)

pb <- pb %>% 
  mutate(first_pb = ifelse(pb_2012 == 1, 2012, 
                           ifelse(pb_2013 == 1, 2013,
                                  ifelse(pb_2014 == 1, 2014,
                                         ifelse(pb_2015 == 1, 2015,
                                                ifelse(pb_2016 == 1, 2016, NA))))))

## replace NA pbdistricts wtih NYCCD, then filter back to NA if start data for that district is 
pb_imputed <- pb %>% 
  mutate(NYCCD = ifelse(is.na(NYCCD), pbdistrict, NYCCD),
         pbdistrict_imputed = ifelse(is.na(pbdistrict) & NYCCD %in% pbnyc_history$pbdistrict, NYCCD, pbdistrict),
         imputed = ifelse(is.na(pbdistrict) & NYCCD %in% pbnyc_history$pbdistrict, 1, 0))
pb_imputed <- pb_imputed %>% 
  left_join(pbnyc_history, by = c("pbdistrict_imputed" = "pbdistrict")) %>% 
  mutate(pbdistrict_imputed = ifelse(imputed == 1 & start_year > first_pb, NA, pbdistrict_imputed)) %>% 
  select(-imputed, -first_pb, -start_year, -pbdistrict) %>% 
  rename(pbdistrict = pbdistrict_imputed)
pb <- pb_imputed
rm(pb_imputed)

## impute remaining missing pbdistrict for 2012 voters only - using proportional allocation based on unaccounted for voters
pb2012 <- read.csv(file = "pbnyc_district_votes.csv", as.is = TRUE) %>% 
  filter(voteYear == 2012) %>% 
  select(district, voters) 
pb2012 <- pb %>% filter(pb_2012 == 1 & !is.na(pbdistrict)) %>% count(pbdistrict) %>% 
  left_join(pb2012, ., by = c(district = "pbdistrict")) %>% 
  mutate(pct_missing = (voters-n)/sum(voters-n)) %>% 
  select(-voters, -n)
pb$pbdistrict[pb$pb_2012 == 1 & is.na(pb$pbdistrict)] <- sample(c(pb2012$district),
                                                                size = sum(pb$pb_2012 == 1 & is.na(pb$pbdistrict)), 
                                                                replace = T, prob=c(pb2012$pct_missing)) 

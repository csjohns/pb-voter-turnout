create_pb_long <- function(analysis_df){
  # Dealing with DoB, calculating total PB votes (note that these will usually be off by 1 because 2014 has too many voters (everyone who came before))
  
  pb <- analysis_df %>% 
    ungroup() %>%
    mutate(DoB = ymd(DoB)) %>% 
    mutate(college_pct = college/100,
           high_school_pct = high_school/100,
           medhhinc_10k = medhhinc/10000)
  
  
  ### Reshaping long for regression -------------------------------------------------------------------------------------------------------------------------
  ## This is a multi-step process, creating the long pb votes, and the long regular votes separately, then joining.
  
  pb_long <- pb %>% dplyr::select(-pp_2012, -p_2011) %>%
    gather(election, turned_out, starts_with("p_2"), starts_with("g_2"), starts_with("pp_")) %>%
    separate(election, c("election_type", "year")) %>%
    ungroup() %>% 
    mutate(year = as.numeric(year))
  # 
  # pb_long <- pb %>% filter(pb == 1) %>% dplyr::select(VANID, totpb, starts_with("pb_")) %>% 
  #   gather( year, pb, starts_with("pb_")) %>% 
  #   mutate(year = as.numeric(str_replace(year, "pb_", "")),
  #          pb = as.numeric(pb)) %>% 
  #   full_join(filter(elec_long, pb == 1) %>% dplyr::select(VANID, year, totpb) %>% distinct())
  # 
  # # this code calculates pb start year - not that this code is good even though 2014 is a wacky error year since voters who didn't vote before 2014 will indeed have their start year be 2014
  # pb_long <- pb_long %>% group_by(VANID) %>%
  #   arrange(VANID, year) %>%
  #   mutate(pbyear  = ifelse(pb == 1, year, NA),
  #          pb_start = min(pbyear, na.rm = TRUE),
  #          pb_start = ifelse(pb_start == Inf, NA, pb_start)
  #   )
  
  # pb_long <- pb_long %>% dplyr::select(-pb) %>% full_join(elec_long) 
  
  pb_long <- pb_long %>%  filter(year >= 2008)
  pb_long <- pb_long %>%
    # group_by(VANID) %>% 
    #mutate(pb_start_group = min(pb_start, na.rm = TRUE)) %>% filter(pb == 1 & year %in% c(2016,2017)) %>% arrange(VANID, year) %>% View
    # group_by() %>% 
    mutate(after_pb = ifelse(pb == 1 & year >= pbyear, 1, 0),
           # after_pb = as.numeric(year >= pb_start),
           # after_pb = ifelse(is.na(after_pb), 0, after_pb),
           # repeater = totpb > 1, removing this because errors in 2014 means every early voter is a repeater, which isn't correct
           age_at_vote = year - year(DoB) ,
           Female = ifelse(Sex == "F", 1, 0))
  ## I think there are some nonsense ages in here and I need to investigate DoB coding more
  
  pb_long  
}
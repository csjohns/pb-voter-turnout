# Helper functions for voterfile missing data clean up

random_mode <- function(x) {
  m <- modeest::mlv(x, method = "mfv", na.rm = TRUE)
  if (length(m) > 1) {
    m <- sample(m, 1)
  }
  m
}

replace_na_compet <- function(voterfile, compet_var) {
  cv <- compet_var
  print(cv)
  compet_var <- ensym(compet_var)
  voterfile <- voterfile %>% 
    group_by(ED) %>% 
    mutate(mode_compet = random_mode(!!compet_var)) %>%  # calculate mode for that ED for this column (randomly break ties)
    ungroup() %>% 
    mutate(mode_compet = ifelse(is.na(ED) | ED == "", NA, mode_compet)) %>% # replace all unknown EDs with NA mode
    mutate(!!compet_var := if_else(is.na(!!compet_var), mode_compet, !!compet_var)) # replace NA values of column w/the mode
  # voterfile[[cv]] <- ifelse(is.na(voterfile[[cv]]), voterfile$mode_compet, voterfile[[cv]])
   
  select(voterfile, -mode_compet)
}

comp_structural_na_1 <- function(voterfile, structural_vars = c("comp_2008_primary", "comp_2012_primary", "comp_2016_primary")) {
  voterfile %>% 
    mutate_at(vars(structural_vars), ~ifelse(!is.na(comp_2009_primary) & is.na(.), -99, .)) 
}

# voterfile %>%
#   # mutate_at(vars(comp_2008_primary, comp_2012_primary, comp_2016_primary), ~ifelse(!is.na(comp_2009_primary) & is.na(.), 1, .)) %>%
#   comp_structural_na_1() %>%
#   select(starts_with("comp_")) %>% purrr::map_dbl(~ sum(is.na(.))/length(.))

##############################################################################################################################
###
### NYC PB Voters Project 
### Matching pb/non-pb voters
### Carolina Johnson
### 
### General matching function
### 
### 2. Create matching dataframe
### 3. Define CEM match thresholds
### 4. Run CEM
### 5. Create analysis dataframe from 1:1 match
### 6. Save matched dataframe to RDS
### 7. Run match balance checking
### 
### Input: Requires a loaded matching df and voterfile; arguments defining matching fields and criteria
### Output: save match counts and
###
##############################################################################################################################
# 
# matching_fields <- c("Race", "age", "Sex", 
#                      "g_early", "g_2008", "g_2009", "g_2010", "p_early", "p_2008", "p_2009", "p_2010", "pp_2004", "pp_2008", 
#                      "white", "college",
#                      "medhhinc" , "majmatch" 
#                      , starts_with("comp")
#                      , white_pct, college_pct)
# 
# cutpoints <-  list()
# 
# grouping <-  list()


custom_cem <- function(df, fields, cutpoints, grouping, outfile = NULL) {
  res <- vector("list")
  df <- df %>% 
    select_at(.vars = c("VANID", "pb", fields)) %>% 
    drop_na()
  print(names(df))
  print(paste0("vars exact matching: ", setdiff(names(df), c(names(cutpoints), names(grouping)))))
  
  c.out <- df %>% 
    select(-VANID) %>% 
    mutate_at(vars(matches("^p_|^g_|^pp_|Race|Sex")), as.factor) %>%
    cem::cem(treatment = "pb",
             data = .,
             cutpoints = cutpoints,
             grouping = grouping,
             verbose = 1)
  
  print(c.out)
  res$summary <- summary(c.out)
  ### Creating the pairwise k2k match including one random sampled control for every pb voter  --------------------------------------------------------------------------------
  c.match <- data.frame(VANID = df$VANID, pb = df$pb, cem_group = c.out$strata, race =df$Race)
  
  c.match <- c.match %>% 
    group_by(cem_group) %>% 
    mutate(n_treat = sum(pb),
           n_control = sum(pb==0)) %>% 
    filter(n_treat > 0 & n_control > 0)
  
  c.treat <- c.match %>% filter(pb == 1 & n_control > 0)
  
  c.control <- c.match %>% filter(pb == 0 & n_treat > 0)
  
  c.control <- c.control %>% 
    group_by(cem_group) %>%
    sample_frac(1) %>% 
    slice(1:unique(n_treat))
  
  c.treat <- c.treat %>% 
    mutate(n_sample = ifelse(n_treat <= n_control, n_treat, n_control )) %>%
    group_by(cem_group) %>% 
    sample_frac(1) %>% 
    slice(1:unique(n_sample)) 
  
  c.match <- c.treat %>% 
    select(-n_sample) %>% 
    bind_rows(c.control)
  
  res$df <- c.match %>% 
    dplyr::select(-n_treat, -n_control) %>% 
    left_join(df)
if (!is.null(outfile)) {saveRDS(res, file = outfile)}
  res$df
}

                     
                     
                     
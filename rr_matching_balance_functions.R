
#### var names -------------------

vnames <- data.frame(old = varlist, new = varlist) %>% 
  mutate(new = str_replace_all(new, pattern = "^g_", replacement = "General "),
         new = str_replace_all(new, pattern = "^pp_", replacement = "Presidential primary "),
         new = str_replace_all(new, pattern = "^p_", replacement = "Primary "),
         new = ifelse(new == "g_early", "General elections 2000-2007", new),
         new = ifelse(new == "p_early", "Primary elections 2000-2007", new),
         new = ifelse(new == "high_school", "Tract: % High school diploma", new),
         new = ifelse(new == "college", "Tract: % College degree", new),
         new = ifelse(new == "medhhinc", "Tract: Median household income", new),
         new = ifelse(new == "white", "Tract: % White", new),
         # new = ifelse(new == "black", "% Black", new),
         # new = ifelse(new == "asian", "% Asian", new),
         # new = ifelse(new == "pacislander", "% Pacific Islander", new),
         # new = ifelse(new == "latinx", "% Latinx", new),
         # new = ifelse(new == "mixed", "% Multiple races", new),
         # new = ifelse(new == "other", "% Other race", new),
         new = ifelse(new == "majmatch", "Majority race alignment", new),
         new = ifelse(new == "age", "Age", new),
         new = str_replace_all(new, pattern = "^comp_", "Competitiveness " ),
         new = str_replace_all(new, pattern = "_general", " General"),
         new = str_replace_all(new, pattern = "_primary", " Primary"),
         new = str_replace_all(new, "match_group_", "District group"),
         new = str_replace_all(new, "incumbent_", "Incumbent "),
         new = str_replace_all(new, "dist_medhhinc", "CD: Median HH income"),
         new = str_replace_all(new, "dist_college", "CD: % College degree"),
         new = str_replace_all(new, "dist_white", "CD: % White"),
         new = str_replace_all(new, "competitivejenks_2017", "CD: Competitive race ")
  )



### matching functions  ------------------
calc_pb_balance <- function(voterfile = voterfile, matched, fields) {
  df <- voterfile %>% 
    select(VANID, pb, fields) %>% 
    filter(pb == 1) %>% 
    left_join(., (filter(matched, pb == 1) %>%  select(VANID, cem_group)), by = "VANID")  %>% 
    mutate(inmatch = as.numeric(!is.na(cem_group))) %>%
    select(-cem_group, -VANID, -pb, -agegroup) %>% 
    drop_na()
  
  if("Sex" %in% names(df)) {
    df <- df %>% 
      mutate(Sex = as.factor(Sex),
             Race = as.factor(Race),
             match_group = as.factor(match_group))
  }
  if("NYCCD" %in% names(df)) {
    df <- df %>% 
      mutate(NYCCD = as.factor(NYCCD))
  }
  out <- df %>% 
    bal.tab(treat = "inmatch", data = df)
  out
}

pb_balance_plot  <- function(testout, match_type) {
  p <- love.plot(testout, threshold = .1, abs = TRUE, var.names = vnames, 
                 line = T, stars = "std") +
    theme(legend.position = "bottom") +
    labs(x = "Mean differences PB - not PB", color = "", shape = "",
         title = match_type)
  p
}


calc_control_balance <- function(voterfile = voterfile, matched, fields) {
  df <- voterfile %>% 
    select(VANID, pb, fields) %>% 
    mutate(match_group = as.factor(match_group)) %>% 
    left_join(., (select(matched, VANID, cem_group)), by = "VANID")  %>% 
    mutate(inmatch = as.numeric(!is.na(cem_group))) %>% 
    select(-cem_group, -VANID, -agegroup) %>% 
    drop_na()
  
  out <- df %>% 
    select(-pb, -inmatch) %>% 
    bal.tab(treat = "pb",  data = df, weights = "inmatch", 
            method = "matching", disp.means = TRUE, un = TRUE, quick = TRUE)
  
  out
}


cc_balance_plot  <- function(testout, plotlabel) {
  # tt <- testout
  
  p <- love.plot(testout, threshold = .1, abs = FALSE, title = plotlabel, var.names = vnames, 
                 line = T, sample.names = c("Before Matching", "After Matching"),
                 stars = "std") +
    theme(legend.position = "bottom") +
    labs(x = "Mean differences PB - not PB", color = "", shape = "", linetype = "", size = "", stroke = "")
  p
}

extract_balance_df <- function(tab, label) {
  tab$Balance %>% 
    dplyr::select(Diff.Un) %>% 
    tibble::rownames_to_column("variable") %>% 
    mutate(model = label)
}


match_name_lookup <- tribble(
  ~match_type, ~match_name, ~`Individual \nDemographics & past voting`, ~`Census tract`, ~`Council District`, ~`Competitiveness`,
  "Only exact", "Medium indiv. vars only", "Medium granularity", "", "", "",
  "Tract super", "Fine + tract (*selected model*)", "Fine granularity", "Standard: Income (quintile), % white (quintile), % college educated (median) + majority race indicator", "", "",
  "Tract fine", "Med. + tract", "Medium granularity: larger bins for pre-2008 votes and age group", "as above", "", "", 
  "Tract coarse", "Coarse + coarse tract",   "Coarse granularity: any or no pre-2008 votes, age by <26, 26-65, 65+", "Coarse: Split at median only + majority race indicator", "", "",
  "Dist comp", "CD Competition + tract", "Medium granularity", "Standard", "High/low competition indicators for city council elections 2009, 2013, 2017", "",
  "Dist white", "CD % white + tract",  "Medium", "Standard",  "% white", "",
  "Dist medhhinc", "CD median HH inc. + tract", "Medium", "Standard",  "Median household income", "",
  "Dist college", "CD % college + tract", "Medium", "Standard",  "% college educated", "",
  "incumb. 13", "CD Incumbent 2013 + tract", "Medium", "Standard",  "Council member 2013 incumbent", "",
  "incumb. 17", "CD Incumbent 2017 + tract", "Medium", "Standard",  "Council member 2017 incumbent", "",
  "Group dist", "CD group cluster", "Medium", "",  "Multivariate district cluster ID", "",
  "Dist + tract", "CD group cluster + tract",  "Medium", "Standard", "Multivariate district cluster ID", "",
  "Dist + compet", "CD group cluster + competition","Medium", "", "Multivariate district cluster ID", "Standard: Individual high/low competition indicators for elections with city-wide variability",
  "Compet", "Election competitiveness", "Medium", "", "", "Standard",
  "Compet + tract, fine", "Competitiveness + tract", "Medium", "Standard", "", "Standard",
  "Compet + tract, coarse", "Competitiveness + coarse tract", "Coarse", "Coarse", "", "Standard",
  "All, coarse", "All variables, coarse", "Coarse", "Coarse", "Cluster ID", "Standard",
  "All, fine", "All variables, standard", "Medium", "Standard", "Cluster ID", "Standard"
  )

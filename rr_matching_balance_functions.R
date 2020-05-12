### matching functions
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
  p <- love.plot(testout, threshold = .1, abs = TRUE, #var.names = vnames, 
                 title = match_type, line = T) +
    theme(legend.position = "bottom") +
    labs(x = "Mean differences PB - not PB", color = "", shape = "",
         title = match_type)
  p
}


calc_control_balance <- function(voterfile = voterfile, matched, fields) {
  df <- voterfile %>% 
    select(VANID, pb, fields) %>% 
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
  
  p <- love.plot(testout, threshold = .1, abs = FALSE, title = plotlabel, #var.names = vnames, 
                 line = T, sample.names = c("Before Matching", "After Matching")) +
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
## comparing the with and without district incumbency values


# Load libraries
library(dplyr)
library(tidyr)
library(purrr)
library(broom)
library(ggplot2)
library(lubridate)
library(stringr)
library(cobalt)


### functions and formats ------------
calc_pb_balance <- function(voterfile = voterfile, matched, fields) {
  df <- voterfile %>% 
    select(VANID, pb, age, fields) %>% 
    filter(pb == 1) %>% 
    left_join(., (filter(matched, pb == 1) %>%  select(VANID, cem_group)), by = "VANID")  %>% 
    mutate(inmatch = as.numeric(!is.na(cem_group))) %>% 
    select(-cem_group, -VANID, -pb, -agegroup) %>% 
    drop_na()
    
  out <- df %>% 
    mutate(Sex = as.factor(Sex),
           Race = as.factor(Race)) %>% 
    bal.tab(treat = "inmatch", data = df)
  
  out
}

calc_control_balance <- function(voterfile = voterfile, matched, fields) {
  df <- voterfile %>% 
    select(VANID, pb, age, fields) %>% 
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
  

extract_balance_df <- function(tab, label) {
  tab$Balance %>% 
    dplyr::select(Diff.Un) %>% 
    tibble::rownames_to_column("variable") %>% 
    mutate(model = label)
}
 
# vnames <- data.frame(old = names(all_pb), new = names(all_pb)) %>% 
#   mutate(new = str_replace_all(new, pattern = "g_", replacement = "General "),
#          new = str_replace_all(new, pattern = "pp_", replacement = "Presidential primary "),
#          new = str_replace_all(new, pattern = "p_", replacement = "Primary "),
#          new = ifelse(new == "g_early", "General elections 2000-2007", new),
#          new = ifelse(new == "p_early", "Primary elections 2000-2007", new),
#          new = ifelse(new == "high_school", "% High school diploma", new),
#          new = ifelse(new == "college", "% College degree", new),
#          new = ifelse(new == "medhhinc", "Median household income", new),
#          new = ifelse(new == "white", "% White", new),
#          # new = ifelse(new == "black", "% Black", new),
#          # new = ifelse(new == "asian", "% Asian", new),
#          # new = ifelse(new == "pacislander", "% Pacific Islander", new),
#          # new = ifelse(new == "latinx", "% Latinx", new),
#          # new = ifelse(new == "mixed", "% Multiple races", new),
#          # new = ifelse(new == "other", "% Other race", new),
#          new = ifelse(new == "majmatch", "Majority race alignment", new),
#          new = ifelse(new == "age", "Age", new),
#          new = ifelse(new == "comp_g_2014", "Competitiveness 2014 general", new),
#          new = ifelse(new == "comp_g_2016", "Competitiveness 2016 general", new),
#          new = ifelse(new == "comp_p_2014", "Competitiveness 2014 primary", new),
#          new = ifelse(new == "comp_pp_2016", "Competitiveness 2016 pres. primary", new)
#   )

### load data ------------------
suffix <- ""
voterfile <- readRDS(paste0("data/cleaned_R_results/voterfile_for_matching", suffix, ".rds"))
allout <- readRDS(paste0("data/cleaned_R_results/matching_res", suffix, ".RDS")) 


cbtout <- calc_pb_balance(voterfile, allout$outdf[[1]], allout$matching_fields[[1]])

love.plot(cbtout, threshold = .1, abs = FALSE,  title = "") + 
  theme(legend.position = "none")+     
  xlab("Mean differences matched-unmatched")

allout <- allout %>% 
  mutate(balance_pb = map2(outdf, match_type,
                           .f = ~extract_balance_df(calc_pb_balance(voterfile = voterfile, 
                                                                    matched = .x, 
                                                                    fields = allout$matching_fields[[1]]), 
                                                    label = .y)))

p1 <- allout %>% #slice(1:3) %>% 
  pluck("balance_pb") %>% 
  bind_rows() %>% 
  filter(variable != "inmatch") %>% 
  mutate(variable = as.factor(variable)) %>% 
  mutate(variable = forcats::fct_reorder(variable, Diff.Un, .desc = T)) %>% 
  # allout$balance_pb[[1]] %>% 
  mutate(startpoint = 0) %>% 
  ggplot(aes(y = Diff.Un, x = variable, color = model, shape = model, group = model)) +
  geom_line() +
  geom_point() +
  geom_hline(aes(yintercept = 0), alpha = .6) +
  coord_flip() +
  labs(y = "Difference matched - unmatched") +
  theme_minimal()

plotly::ggplotly(p1)


#### balance checking pb/non-pb
testout <- calc_control_balance(voterfile, allout$outdf[[1]], fields = allout$matching_fields[[1]])

love.plot(testout, threshold = .1, abs = FALSE, #var.names = vnames, 
          title = "", line = T, 
          sample.names = c("Before Matching", "After Matching")) +
  theme(legend.position = "bottom") +
  labs(x = "Mean differences PB-nonPB", color = "", shape = "")

for (i in 1:nrow(allout)) {
  testout <- calc_control_balance(voterfile, allout$outdf[[i]], fields = allout$matching_fields[[1]])
  
  p <- love.plot(testout, threshold = .1, abs = FALSE, #var.names = vnames, 
            title = "", line = T, 
            sample.names = c("Before Matching", "After Matching")) +
    theme(legend.position = "bottom") +
    labs(x = "Mean differences PB-nonPB", color = "", shape = "",
         title = allout$match_type[[i]])
  print(p)
}


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

cbtout_byrace <- all_pb %>% #dplyr::select(all_pb) %>% #, -VANID, -NYCCD, -pb) %>% 
  filter(Race %in% c("A", "B", "W")) %>% 
  mutate(Sex = as.factor(Sex),
         Race = as.factor(Race)) %>% 
  group_by(Race) %>% 
  do(extractGroupBal(cv = ., tt = "inmatch", group = "Race", df = .)) %>% 
  mutate(end = 0)

ggplot(cbtout_byrace, aes(x = Diff.Un, y = variable, color = Race)) + geom_point() +
  geom_segment(aes(xend = end, yend = variable))# + facet_wrap(~Race)


all_pb %>% 
  ggplot() + geom_bar(aes(x = g_early, fill = as.factor(inmatch)), position = "dodge") + facet_wrap(~Race)
voterfile%>% 
  ggplot() + geom_density(aes(x = g_early, fill = as.factor(pb)), position = "dodge") + facet_grid(pb~Race, scales = "free")
all_pb %>% 
  ggplot() + geom_bar(aes(x = p_early, fill = as.factor(inmatch)), position = "dodge") + facet_wrap(~Race)
voterfile%>% 
  ggplot() + geom_density(aes(x = p_early, fill = as.factor(pb)), position = "dodge") + facet_grid(pb~Race, scales = "free")

## comparing the with and without district incumbency values


# Load libraries
library(dplyr)
library(tidyr)
library(purrr)
library(broom)
library(ggplot2)
library(lubridate)
library(stringr)
library(lme4)
library(margins)
library(simcf)


# create summary for plotting

robust_sum <- function(df) {
  if(!"tidyresult" %in% names(df)) {
    df <- df %>% 
      mutate(tidyresult = map(result, broom::tidy, conf.int = TRUE),
             AIC = map_dbl(result, AIC),
             BIC = map_dbl(result, BIC))
  }
  df %>%  
    select(match_type, model_name, tidyresult) %>% 
    unnest(cols = tidyresult)
} 

robust_plot <- function(df) {
  df %>% 
    filter(group == "fixed" & term != "I(age_at_vote < 18)TRUE" & !str_detect(term, "year") & term != "(Intercept)") %>% 
    ggplot(aes(x = term, y = estimate, ymin = conf.low, ymax = conf.high, color = match_type)) +
    geom_pointrange() +
    geom_point() +
    geom_hline(aes(yintercept = 0)) +
    facet_wrap(~model_name) +
    # ylim(-5, 5) +
    coord_flip()
}

# without any district/incumbency measure
suffix <- ""
allout <- readRDS(paste0("data/cleaned_R_results/matching_res", suffix, ".RDS")) %>% 
  filter(match_type %in% c("All vars, fine", "All vars, coarse", "Excl compet"))
 regres <- readRDS("data/cleaned_R_results/model_match_lmers.rds") #%>% 
  # filter(match_type %in% c("All vars, fine", "All vars, coarse", "Excl compet"))

p <- regres %>% 
  robust_sum() %>% 
  robust_plot()

#

# 
# with any district/incumbency measure
suffix <- "incumb17"
allout_inc <- readRDS("data/cleaned_R_results/matching_resw_incumb.RDS") %>% 
  filter(match_type %in% c("All vars, fine", "All vars, coarse", "Excl compet"))
regres_inc <- readRDS(paste0("data/cleaned_R_results/iter_regress_check", suffix, ".RDS")) %>% 
  filter(match_type %in% c("All vars, fine", "All vars, coarse", "Excl compet"))

regres_inc %>% 
  robust_sum() %>% 
  robust_plot()

regresall <- regres_inc %>% 
  mutate(match_type = paste0("inc_", match_type)) %>% 
  bind_rows(regres)

all <- regresall %>% 
  robust_sum() %>% 
  robust_plot()

plotly::ggplotly(all)

### comparing match balances
# source("pub_balance_checking.R")
library(cobalt)

suffix <- "_within_dist"
voterfile <- readRDS(paste0("data/cleaned_R_results/voterfile_for_matching", suffix, ".rds"))
allout <- readRDS(paste0("data/cleaned_R_results/matching_res", suffix, ".RDS")) 

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


cbtout <- calc_pb_balance(voterfile, allout$outdf[[1]], allout$matching_fields[[1]])

love.plot(cbtout, threshold = .1, abs = FALSE,  title = "") + 
  theme(legend.position = "none")+     
  xlab("Mean differences matched-unmatched")

for (i in 1:nrow(allout)) {
  print(allout$match_type[[i]])
  cbtout <- calc_pb_balance(voterfile, allout$outdf[[i]], c(allout$matching_fields[[11]], "age"))
  p <- love.plot(cbtout, threshold = .1, abs = FALSE,  title = paste0("Match: ", allout$match_type[[i]])) + 
    theme(legend.position = "none")+     
    xlab("Mean differences matched-unmatched") 
  print(p)
}

allout <- allout %>% 
  mutate(balance_pb = map2(outdf, match_type,
                           .f = ~extract_balance_df(calc_pb_balance(voterfile = voterfile, 
                                                                    matched = .x, 
                                                                    fields = c(allout$matching_fields[[11]])), 
                                                    label = .y)))

comb_compar <- bind_rows(cbtout$Balance %>% 
                           dplyr::select(Diff.Un) %>% 
                           tibble::rownames_to_column("variable") %>% 
                           mutate(model = "base"),
                         cbtout_inc$Balance %>% 
                           dplyr::select(Diff.Un) %>% 
                           tibble::rownames_to_column("variable") %>% 
                           mutate(model = "inc")) %>% 
  mutate(startpoint = 0)

comb_compar <- allout %>% 
  filter(match_type = ) %>% 
  bind_rows(.$balance_pb) %>% 
  mutate(start_point = 0)

p <- comb_compar %>% 
  filter(variable != "inmatch") %>% 
  ggplot(aes(x = Diff.Un, y = forcats::fct_reorder(variable, Diff.Un), color = model, fill = model, shape = model, group = model)) +
  # geom_segment(aes(xend = start_point, yend = variable )) +
  # geom_line() +
  # geom_point() +
  geom_bar(stat = "identity", position = "dodge")
  geom_vline(aes(xintercept = 0), alpha = .6)
  
plotly::ggplotly(p)



### checking district coverage

allout <- allout %>% 
  mutate(balance_pb_dist = map2(outdf, match_type,
                           .f = ~extract_balance_df(calc_pb_balance(voterfile = voterfile, 
                                                                    matched = .x, 
                                                                    fields = c("agegroup", "NYCCD")), 
                                                    label = .y)))


comb_compar <- allout %>% 
  # filter(match_type = ) %>% 
  bind_rows(.$balance_pb_dist) %>% 
  mutate(start_point = 0)

p <- comb_compar %>% 
  filter(variable != "inmatch") %>% 
  ggplot(aes(x = Diff.Un, y = forcats::fct_reorder(variable, Diff.Un), color = model, fill = model, shape = model, group = model)) +
  # geom_segment(aes(xend = start_point, yend = variable )) +
  # geom_line() +
  # geom_point() +
  geom_bar(stat = "identity", position = "dodge")
geom_vline(aes(xintercept = 0), alpha = .6)

plotly::ggplotly(p)




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

cbtout_byrace <- dplyr::select(all_pb, -VANID, -NYCCD, -pb) %>% 
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

### relax.cem code exploring match sensitivity failures #########------------------
library(dplyr)
library(tidyr)
library(cem)
library(ggplot2)


progmatch <- readRDS("data/cleaned_R_results/progmatch.rds")
relax.plot(progmatch, grou)

tail(progmatch$G1, 20)
522/68

tail(progmatch$G0)

table(tail(progmatch$G1$var, 50))

p <- progmatch$G1 %>% 
  arrange(G1) %>% 
  mutate(index = row_number()) %>% 
  ggplot() + geom_point(aes(index, G1, color = var))
plotly::ggplotly(p)

res <- progmatch$G1

res %>% 
  separate(Relaxed, into = c("relax1", "relax2"), sep = ",") %>% 
  filter(G1 > 100) %>% 
  count(relax1) %>% 
  arrange(desc(n))

res %>% 
  separate(Relaxed, into = c("relax1", "relax2"), sep = ",") %>% 
  filter(G1 > 100) %>% 
  count(relax2) %>% 
  arrange(desc(n))

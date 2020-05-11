## comparing the with and without district incumbency values
# !diagnostics off

# Load libraries
library(dplyr)
library(tidyr)
library(purrr)
library(furrr)
library(broom)
library(ggplot2)
library(lubridate)
library(stringr)
library(cobalt)
library(ggpubr)

### functions and formats ------------
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
  

extract_balance_df <- function(tab, label) {
  tab$Balance %>% 
    dplyr::select(Diff.Un) %>% 
    tibble::rownames_to_column("variable") %>% 
    mutate(model = label)
}
 
### load data ------------------
suffix <- ""
voterfile <- readRDS(paste0("data/cleaned_R_results/voterfile_for_matching", suffix, ".rds"))
allout <- readRDS(paste0("data/cleaned_R_results/matching_res", suffix, ".RDS")) 

# ------------
# cbtout <- calc_pb_balance(voterfile, allout$outdf[[1]], allout$matching_fields[[1]])
# 
# love.plot(cbtout, threshold = .1, abs = FALSE,  title = "") + 
#   theme(legend.position = "none")+     
#   xlab("Mean differences matched-unmatched")
# 
# allout <- allout %>% 
#   mutate(balance_pb = map2(outdf, match_type,
#                            .f = ~extract_balance_df(calc_pb_balance(voterfile = voterfile, 
#                                                                     matched = .x, 
#                                                                     fields = allout$matching_fields[[1]]), 
#                                                     label = .y)))
# 
# p1 <- allout %>% #slice(1:3) %>% 
#   pluck("balance_pb") %>% 
#   bind_rows() %>% 
#   filter(variable != "inmatch") %>% 
#   mutate(variable = as.factor(variable)) %>% 
#   mutate(variable = forcats::fct_reorder(variable, Diff.Un, .desc = T)) %>% 
#   # allout$balance_pb[[1]] %>% 
#   mutate(startpoint = 0) %>% 
#   ggplot(aes(y = Diff.Un, x = variable, color = model, shape = model, group = model)) +
#   geom_line() +
#   geom_point() +
#   geom_hline(aes(yintercept = 0), alpha = .6) +
#   coord_flip() +
#   labs(y = "Difference matched - unmatched") +
#   theme_minimal()
# 
# plotly::ggplotly(p1)


#### pb voters in sample and out of sample --------------------------
testout <- calc_pb_balance(voterfile, allout$outdf[[1]], fields = allout$matching_fields[[15]])
t2 <-      calc_pb_balance(allout$outdf[[1]], fields = c("age", varlist))

love.plot(testout, threshold = .1, abs = FALSE, #var.names = vnames, 
          title = "", line = T, 
          sample.names = c("Before Matching", "After Matching")) +
  theme(legend.position = "bottom") +
  labs(x = "Mean differences PB-nonPB", color = "", shape = "")


pb_balance_plot  <- function(testout, match_type) {
  p <- love.plot(testout, threshold = .1, abs = TRUE, #var.names = vnames, 
                 title = match_type, line = T) +
    theme(legend.position = "bottom") +
    labs(x = "Mean differences PB - not PB", color = "", shape = "",
         title = match_type)
  p
}

varlist <- allout$matching_fields[allout$match_type == "All, fine"][[1]]

allout <- allout %>% #select(-balance, -loveplot)
  mutate(balance = map(outdf, calc_pb_balance, voterfile = voterfile, fields = c("age", varlist))) %>%
  mutate(loveplot = map2(.x = balance, .y = match_type, .f=pb_balance_plot))

ggarrange(plotlist = allout$loveplot[2:16], nrow = 3, ncol = 5)



comb_compar <- allout %>% 
  mutate(balance_pb = map2(outdf, match_type, 
                           .f = ~extract_balance_df(calc_pb_balance(voterfile = voterfile, 
                                                                    matched = .x, 
                                                                    fields = c("age", varlist)), 
                                                    label = .y))) %>% 
  pluck("balance_pb") %>% 
  bind_rows() %>% 
  mutate(start_point = 0)

p <- comb_compar %>% 
  filter(variable != "inmatch") %>% 
  ggplot(aes(x = Diff.Un, y = forcats::fct_reorder(variable, Diff.Un), color = model, fill = model, shape = model, group = model)) +
  geom_segment(aes(xend = start_point, yend = variable )) +
  # geom_line() +
  geom_point() +
  # geom_bar(stat = "identity", position = "dodge")
geom_vline(aes(xintercept = 0), alpha = .6)

plotly::ggplotly(p)

##### case/control balance -------------------------------------------

for (i in 1:nrow(allout)) {
  testout <- calc_control_balance(voterfile, allout$outdf[[i]], fields = c(age, varlist))
  
  p <- love.plot(testout, threshold = .1, abs = FALSE, #var.names = vnames, 
            title = "", line = T, 
            sample.names = c("Before Matching", "After Matching")) +
    theme(legend.position = "bottom") +
    labs(x = "Mean differences PB-nonPB", color = "", shape = "",
         title = allout$match_type[[i]])
  print(p)
}

for (i in 1:nrow(allout)) {
  testout <- calc_control_balance(voterfile, allout$outdf[[i]], fields = "NYCCD")
  
  p <- love.plot(testout, threshold = .1, abs = FALSE, #var.names = vnames, 
                 title = "", line = T, 
                 sample.names = c("Before Matching", "After Matching")) +
    theme(legend.position = "bottom") +
    labs(x = "Mean differences PB-nonPB", color = "", shape = "",
         title = allout$match_type[[i]])
  print(p)
}


##

cc_balance_plot  <- function(testout, plotlabel) {
  # tt <- testout
  
  p <- love.plot(testout, threshold = .1, abs = FALSE, title = "plotlabel", #var.names = vnames, 
                 line = T, sample.names = c("Before Matching", "After Matching")) +
    theme(legend.position = "bottom") +
    labs(x = "Mean differences PB - not PB", color = "", shape = "")
  p
}


##

# plan(multiprocess)
varlist <- allout$matching_fields[allout$match_type == "All, fine"][[1]]

allout <- allout %>%# slice(15:16) %>% 
  #select(-balance, -loveplot)
  # mutate(balance = map(outdf, calc_control_balance, voterfile = voterfile, fields = c("age", varlist))) #%>%
  mutate(loveplot = map2(.x = balance, .y = match_type, .f=cc_balance_plot))
allout$loveplot <- vector("list", length = nrow(allout))
for (r in 1:nrow(allout )) {
  testout <- allout$balance[[r]]
  label  <- allout$match_type[[r]]
  allout$loveplot[[r]] <- p<- cc_balance_plot(testout, label)
  print(p)
}
for (r in seq_along(nrow(allout ))) {
 print(allout$loveplot[[r]])
}

  
# allout %>% select(match_type, outdf, balance) %>% saveRDS(file = "data/temp/cc_loveplots.rds")
# allout  <- readRDS("data/temp/cc_loveplots.rds")
g <- ggarrange(plotlist = allout$loveplot[2:16], nrow = 3, ncol = 5, common.legend = TRUE)
ggsave(plot = g, filename = "data/temp/giant_plot.png", width = 18, height = 15)


comb_compar <- allout %>% 
  mutate(balance_pb = map2(outdf, match_type, 
                           .f = ~extract_balance_df(calc_pb_balance(voterfile = voterfile, 
                                                                    matched = .x, 
                                                                    fields = c("age", varlist)), 
                                                    label = .y))) %>% 
  pluck("balance_pb") %>% 
  bind_rows() %>% 
  mutate(start_point = 0)

p <- comb_compar %>% 
  filter(variable != "inmatch") %>% 
  ggplot(aes(x = Diff.Un, y = forcats::fct_reorder(variable, Diff.Un), color = model, fill = model, shape = model, group = model)) +
  geom_segment(aes(xend = start_point, yend = variable )) +
  # geom_line() +
  geom_point() +
  # geom_bar(stat = "identity", position = "dodge")
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

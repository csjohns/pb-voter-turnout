## comparing the with and without district incumbency values
# !diagnostics off

# Load libraries
library(dplyr)
library(tidyr)
library(purrr)
# library(furrr)
library(broom)
library(ggplot2)
library(lubridate)
library(stringr)
library(cobalt)
library(ggpubr)

### functions and formats ------------
source("rr_matching_balance_functions.R")
 
### load data ------------------
suffix <- ""
voterfile <- readRDS(paste0("data/cleaned_R_results/voterfile_for_matching", suffix, ".rds"))
allout <- readRDS(paste0("data/cleaned_R_results/matching_res", suffix, ".RDS")) 

voterfile <- voterfile %>% 
  mutate_at(vars(starts_with("comp_")), na_if, "-99")

varlist <- allout$matching_fields %>% unlist() %>% unique()

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


allout <- allout %>% 
  filter(match_type %in% c("Tract super", "Tract fine", "Dist + compet", "Dist comp")) %>% 
  # select(-balance, -loveplot) %>%
  mutate(balance = map(outdf, calc_pb_balance, voterfile = voterfile, fields = c("age", varlist))) #%>%
  # mutate(loveplot = pmap(.l = list(testout = balance, match_type = match_type), .f=pb_balance_plot))

allout$loveplot <- vector("list", length = nrow(allout))
for (r in 1:nrow(allout )) {
  testout <- allout$balance[[r]]
  label  <- allout$match_type[[r]]
  allout$loveplot[[r]] <- p<- pb_balance_plot(testout, label)
}

ggarrange(plotlist = allout$loveplot, nrow = 2, ncol = 2)



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

# plan(multiprocess)

allout <- allout %>% filter(match_type %in% c("Tract fine", "Tract super")) %>%# glimpse()
  # select(-balance, -loveplot) %>%
  mutate(balance = map(outdf, calc_control_balance, voterfile = voterfile, fields = c("age", varlist))) #%>%
  # mutate(loveplot = map2(.x = balance, .y = match_type, .f=cc_balance_plot))
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
plotly::ggplotly(allout$loveplot[[1]])

  
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

allout <- allout %>% 
  mutate(g_early_plot = map2())

all_pb %>% 
  ggplot() + geom_bar(aes(x = g_early, fill = as.factor(inmatch)), position = "dodge") + facet_wrap(~Race)
voterfile%>% 
  ggplot() + geom_density(aes(x = g_early, fill = as.factor(pb)), position = "dodge") + facet_grid(pb~Race, scales = "free")
voterfile%>% 
  ggplot() + geom_density(aes(x = comp_2017_primary, fill = as.factor(pb)), position = "dodge") + facet_grid(pb~Race, scales = "free")
all_pb %>% 
  ggplot() + geom_bar(aes(x = p_early, fill = as.factor(inmatch)), position = "dodge") + facet_wrap(~Race)
voterfile%>% 
  ggplot() + geom_density(aes(x = p_early, fill = as.factor(pb)), position = "dodge") + facet_grid(pb~Race, scales = "free")

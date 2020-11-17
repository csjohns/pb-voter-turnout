
# Load libraries
library(dplyr)
library(tidyr)
library(MatchIt)
library(cem)

# source("rr_vf_processing.R")

suffix <- ""
voterfile <- readRDS(paste0("data/cleaned_R_results/voterfile_for_matching", suffix, ".rds"))

#### Implementing Matching, starting with exact ###-----------------------------------------------------------------------------------------------------------------------------------------------  # 

## Exact matching to narrow field of possibility
# source("rr_exact_match.R")

### Creating matching dataframe based on the potential matches from m.exact --------------------------------------------------------------------------------
matchable_vans <- readRDS(paste0("data/cleaned_R_results/matchablevans", suffix, ".rds"))

matching_df <- voterfile %>%
  # select(-matches(missing_cols)) %>% 
  filter(VANID %in% matchable_vans) %>% 
  mutate_at(vars(starts_with("comp")), replace_na, NA) %>% 
  mutate(agegroup = cut(age, breaks = c(0, 18.5, 25.5, 39.5, 49.5, 59.5, 69.5, 79.5, Inf))) 

### create match model parameters
source("rr_matching_levels.R")
source("rr_matching_functions.R")

testout <- cem_only(df = matching_df,
                      fields = matching_models$matching_fields[[4]],
                      cutpoints = matching_models$cutpoints[[4]],
                      grouping = matching_models$grouping[[4]])
testout$out
progmatch <- relax.cem(testout$out, testout$df, plot = FALSE, depth = 2, 
                       fixed = c("Race", "Sex", "g_early", "g_2008", "g_2009", "g_2010", "p_early", "p_2008", "p_2009", "p_2010", "pp_2008"))


testout2 <- cem_only(df = matching_df,
                    fields = matching_models$matching_fields[[5]],
                    cutpoints = matching_models$cutpoints[[5]],
                    grouping = matching_models$grouping[[5]])
progmatch2 <- relax.cem(testout2$out, testout2$df, plot = FALSE, depth = 2, 
                       fixed = c("Race", "Sex", "g_early", "g_2008", "g_2009", "g_2010", "p_early", "p_2008", "p_2009", "p_2010", "pp_2008", 
                                 "white", "college", "medhhinc", "majmatch"))

saveRDS(progmatch2, "data/temp/progmatch2.rds")
####

#####
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
  filter(G1 > 800) %>% 
  count(relax1) %>% 
  arrange(desc(n))

res %>% 
  separate(Relaxed, into = c("relax1", "relax2"), sep = ",") %>% 
  filter(G1 > 800) %>% 
  count(relax2) %>% 
  arrange(desc(n))

res %>% 
  filter(stringr::str_detect(Relaxed, "jenks")) %>% 
  select(G1) %>% summary()


## checking out this district level variables
voterfile %>% 
  mutate(pb = as.factor(pb)) %>% 
  ggplot() +
  geom_histogram(aes(x = dist_white, fill = incumbent_2017), position = "dodge") + 
  facet_grid(pb ~ incumbent_2013, scales = "free") +
  geom_vline(aes(xintercept =testout$out$breaks$dist_white[[2]])) +
  geom_vline(aes(xintercept =testout$out$breaks$dist_white[[3]])) 

voterfile %>% 
  mutate(pb= as.factor(pb)) %>% 
  ggplot() +
  geom_histogram( aes(x = comp_2017_primary, fill  = pb), position = "stack")  +facet_wrap(~as.factor(pb), scales = "free", nrow = 2) +
  # geom_vline(aes(xintercept  =median(voterfile$comp_2017_primary, na.rm = T)))
  geom_vline(aes(xintercept  =BAMMtools::getJenksBreaks(sample(voterfile$comp_2017_primary, 20000), 3)[2]))

  voterfile %>% 
  mutate(pb = as.factor(pb)) %>% 
  ggplot() +
  geom_smooth(aes(x = dist_white, y = white, color = pb)) +
    geom_abline()
    
  voterfile %>% 
    mutate(pb = as.factor(pb)) %>% 
    ggplot() +
    geom_histogram(aes(x = white, fill = incumbent_2017), position = "dodge") + 
    facet_grid(pb ~ incumbent_2013, scales = "free") +
    geom_vline(aes(xintercept =testout$out$breaks$dist_white[[2]])) +
    geom_vline(aes(xintercept =testout$out$breaks$dist_white[[3]])) 
  
  BAMMtools::getJenksBreaks(sample(voterfile$white, 20000), 4)
  BAMMtools::getJenksBreaks(voterfile$white[voterfile$pb ==1 ], 4)
  # ok - it actually looks like these measures are highly correlated, and its possible that my breaks were in conflict with each other?
  
  
  table(voterfile$competitivejenks_2013, voterfile$incumbent_2013, voterfile$pb) %>% prop.table(1)
  
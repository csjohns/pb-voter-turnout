library(dplyr)
library(tidyr)
library(glue)
library(lubridate)
library(stringr)
library(data.table)
library(MatchIt)
library(cem)


 load("vf_dataformatching.RData")
 load("compet.Rdata")  
 

## Repeating CEM with different specifications

## The one I used last - as was + majmatch


df_cutpoints <- list(
  white = quantile(matching_df$white, c(0,.2,.4,.6,.8,1)),
  #college = quantile(matching_df$college, c(0,.5,1)),
  medhhinc = quantile(matching_df$medhhinc, c(0,.2,.4,.6,.8,1))
  , comp_g_2014 = quantile(compet_select$g_2014_comp, probs = c(0, .25, .75, 1), na.rm = TRUE),
  comp_g_2016 = quantile(compet_select$g_2016_comp, probs = c(0, .25, .75, 1), na.rm = TRUE),
  comp_p_2014 = quantile(compet_select$p_2014_comp, probs = c(0, .25, .75, 1), na.rm = TRUE),
  comp_pp_2016 = quantile(compet_select$pp_2016_comp, probs = c(0, .25, .75, 1), na.rm = TRUE)
)


c.out <- matching_df %>% select(-VANID, -college) %>% 
  mutate_at(vars(starts_with("g_"), starts_with(("p_"))), as.factor) %>% 
  mutate(pp_2004 = as.factor(pp_2004),
         pp_2008 = as.factor(pp_2008),
         Race = as.factor(Race),
         Sex = as.factor(Sex)) %>% 
  cem(treatment = "pb", data = .,  
      grouping = list(
        g_early = list("0",c("1,2"), c("3", "4"), c("5", "6"), c("7", "8")), 
        p_early = list("0",c("1,2"),  c("3", "4"), c("5", "6"), c("7", "8"))
      ), 
      cutpoints = df_cutpoints,
      verbose = 1) ### DON'T USE K2K - TOO MUCH MEMORY DEMAND. RANDOMLY DRAW FROM STRATA AFTER THE FACT

c.out
c.out.base <- summary(c.out)

### Creating the pairwise k2k match including one random sampled control for every pb voter  --------------------------------------------------------------------------------
c.match <- data.frame(VANID = matching_df$VANID, pb = matching_df$pb, cem_group = c.out$strata, race =matching_df$Race)

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

c.base_majmatch <- c.match

# 
#                 G0   G1
# All       1282112 6203
# Matched    223250 3811
# Unmatched 1058862 2392
# 3811/6203  0.6143801 - 0.499214 from original

# 9-8 post cleain
#               G0   G1
# All       1126750 6158
# Matched    188131 3634
# Unmatched  938619 2524
# .5901 from matching df, .4757 from original

# post GIS
# G0   G1
# All       1035663 6151
# Matched    176832 3899
# Unmatched  858831 2252
# .634 frommatching (.474 from orig)

####  Base with college added back in ------------------------------------------------------------------
rm(c.match, c.out)

set.seed(12018)

df_cutpoints <- list(
  white = quantile(matching_df$white, c(0,.2,.4,.6,.8,1)),
  college = quantile(matching_df$college, c(0,.5,1)),
  medhhinc = quantile(matching_df$medhhinc, c(0,.2,.4,.6,.8,1))
  , comp_g_2014 = quantile(compet_select$g_2014_comp, probs = c(0, .25, .75, 1), na.rm = TRUE),
  comp_g_2016 = quantile(compet_select$g_2016_comp, probs = c(0, .25, .75, 1), na.rm = TRUE),
  comp_p_2014 = quantile(compet_select$p_2014_comp, probs = c(0, .25, .75, 1), na.rm = TRUE),
  comp_pp_2016 = quantile(compet_select$pp_2016_comp, probs = c(0, .25, .75, 1), na.rm = TRUE)
)


c.out <- matching_df %>% select(-VANID) %>% 
  mutate_at(vars(starts_with("g_"), starts_with(("p_"))), as.factor) %>% 
  mutate(pp_2004 = as.factor(pp_2004),
         pp_2008 = as.factor(pp_2008),
         Race = as.factor(Race),
         Sex = as.factor(Sex)) %>% 
  cem(treatment = "pb", data = .,  
      grouping = list(
        g_early = list("0",c("1,2"), c("3", "4"), c("5", "6"), c("7", "8")), 
        p_early = list("0",c("1,2"),  c("3", "4"), c("5", "6"), c("7", "8"))
      ), 
      cutpoints = df_cutpoints,
      verbose = 1) ### DON'T USE K2K - TOO MUCH MEMORY DEMAND. RANDOMLY DRAW FROM STRATA AFTER THE FACT

c.out 
c.out.college <- summary(c.out)

### Creating the pairwise k2k match including one random sampled control for every pb voter  --------------------------------------------------------------------------------
c.match <- data.frame(VANID = matching_df$VANID, pb = matching_df$pb, cem_group = c.out$strata, race =matching_df$Race)

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

c.college <- c.match

#                G0   G1
# All       1282112 6203
# Matched    179983 3628
# Unmatched 1102129 2575
# 0.5848783 match [1] 0.4752423 from original

# after clean up
# G0   G1
# All       1126750 6158
# Matched    152216 3425
# Unmatched  974534 2733
# .5562 from matching, .4483 from original

#post GIS
# G0   G1
# All       1035663 6151
# Matched    142649 3700
# Unmatched  893014 2451
# .602 from matching, .446 from original

####  more granular  ------------------------------------------------------------------
rm(c.match, c.out)

df_cutpoints <- list(
  white = quantile(matching_df$white, c(0,.2,.4,.6,.8,1)),
  college = quantile(matching_df$college, c(0,.5,1)),
  medhhinc = quantile(matching_df$medhhinc, c(0,.2,.4,.6,.8,1))
  , comp_g_2014 = quantile(compet_select$g_2014_comp, probs = c(0,.2,.4,.6,.8,1), na.rm = TRUE),
  comp_g_2016 = quantile(compet_select$g_2016_comp, probs = c(0,.2,.4,.6,.8,1), na.rm = TRUE),
  comp_p_2014 = quantile(compet_select$p_2014_comp, probs = c(0,.2,.4,.6,.8,1), na.rm = TRUE),
  comp_pp_2016 = quantile(compet_select$pp_2016_comp, probs = c(0,.2,.4,.6,.8,1), na.rm = TRUE)
)


c.out <- matching_df %>% select(-VANID) %>% 
  mutate_at(vars(starts_with("g_"), starts_with(("p_"))), as.factor) %>% 
  mutate(pp_2004 = as.factor(pp_2004),
         pp_2008 = as.factor(pp_2008),
         Race = as.factor(Race),
         Sex = as.factor(Sex)) %>% 
  cem(treatment = "pb", data = .,  
      cutpoints = df_cutpoints,
      verbose = 1) ### DON'T USE K2K - TOO MUCH MEMORY DEMAND. RANDOMLY DRAW FROM STRATA AFTER THE FACT

c.out 
c.out.granular <- summary(c.out)


### Creating the pairwise k2k match including one random sampled control for every pb voter  --------------------------------------------------------------------------------
c.match <- data.frame(VANID = matching_df$VANID, pb = matching_df$pb, cem_group = c.out$strata, race =matching_df$Race)

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

c.granular <- c.match

# G0   G1
# All       1282112 6203
# Matched    173770 3329
# Unmatched 1108342 2874
# .5366758 matched - 0.4360755 from original

### after adjusting districts etc:
# 
# G0   G1
# All       1126750 6158
# Matched    148588 3199
# Unmatched  978162 2959
# .5195 matched, .4187 from original 7640

## post GIS
# G0   G1
# All       1035663 6151
# Matched    137493 3346
# Unmatched  898170 2805
# .544 matched, .??? from original


######

####  Refined match back in ------------------------------------------------------------------
rm(c.match, c.out)

df_cutpoints <- list(
  white = quantile(matching_df$white, c(0,.2,.4,.6,.8,1)),
  college = quantile(matching_df$college, c(0,.5,1)),
  medhhinc = quantile(matching_df$medhhinc, c(0,.2,.4,.6,.8,1))
  , comp_g_2014 = unique(quantile(compet_select$g_2014_comp, probs = c(0,.2,.4,.6,.8,1), na.rm = TRUE)),
  comp_g_2016 = unique(quantile(compet_select$g_2016_comp, probs = c(0,.2,.4,.6,.8,1), na.rm = TRUE)),
  comp_p_2014 = unique(quantile(compet_select$p_2014_comp, probs = c(0,.2,.4,.6,.8,1), na.rm = TRUE)),
  comp_pp_2016 = unique(quantile(compet_select$pp_2016_comp, probs = c(0,.2,.4,.6,.8,1), na.rm = TRUE))
)


c.out <- matching_df %>% select(-VANID) %>% 
  mutate_at(vars(starts_with("g_"), starts_with(("p_"))), as.factor) %>% 
  mutate(pp_2004 = as.factor(pp_2004),
         pp_2008 = as.factor(pp_2008),
         Race = as.factor(Race),
         Sex = as.factor(Sex)) %>% 
  cem(treatment = "pb", data = .,  
      grouping = list(
        g_early = list("0",c("1,2"), c("3", "4", "5"), c("6", "7", "8")), 
        p_early = list("0",c("1,2"),  c("3", "4","5"), c("6", "7", "8"))
      ), 
      cutpoints = df_cutpoints,
      verbose = 1) ### DON'T USE K2K - TOO MUCH MEMORY DEMAND. RANDOMLY DRAW FROM STRATA AFTER THE FACT

c.out 
c.out.refined <- summary(c.out)

### Creating the pairwise k2k match including one random sampled control for every pb voter  --------------------------------------------------------------------------------
c.match <- data.frame(VANID = matching_df$VANID, pb = matching_df$pb, cem_group = c.out$strata, race =matching_df$Race)

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

c.refined <- c.match
# 
# G0   G1
# All       1035663 6151
# Matched    146008 3884
# Unmatched  889655 2267
# .631 from matching df

### using defaults for competitiveness

df_cutpoints <- list(
  white = quantile(matching_df$white, c(0,.2,.4,.6,.8,1)),
  college = quantile(matching_df$college, c(0,.2,.4,.6,.8,1)),
  medhhinc = quantile(matching_df$medhhinc, c(0,.2,.4,.6,.8,1))
  , comp_g_2014 = unique(quantile(matching_df$comp_g_2014, c(0, .05, seq(.1,.9, .1), .95, 1))),
  comp_g_2016 = unique(quantile(matching_df$comp_g_2016, c(0, .05, seq(.1,.9, .1), .95, 1))),
  comp_p_2014 = unique(quantile(matching_df$comp_p_2014, c(0, .05, seq(.1,.9, .1), .95, 1))),
  comp_pp_2016 = unique(quantile(matching_df$comp_pp_2016, c(0, .05, seq(.1,.9, .1), .95, 1)))
)


c.out <- matching_df %>% select(-VANID) %>% 
  mutate_at(vars(starts_with("g_"), starts_with(("p_"))), as.factor) %>% 
  mutate(pp_2004 = as.factor(pp_2004),
         pp_2008 = as.factor(pp_2008),
         Race = as.factor(Race),
         Sex = as.factor(Sex)) %>% 
  cem(treatment = "pb", data = .,  
      grouping = list(
        g_early = list("0",c("1,2"), c("3", "4"), c("5", "6"), c("7", "8")), 
        p_early = list("0",c("1,2"),  c("3", "4"), c("5", "6"), c("7", "8"))
      ), 
      cutpoints = df_cutpoints,
      verbose = 1) ### DON'T USE K2K - TOO MUCH MEMORY DEMAND. RANDOMLY DRAW FROM STRATA AFTER THE FACT

c.out 
c.out.refined_comp <- summary(c.out)


### Creating the pairwise k2k match including one random sampled control for every pb voter  --------------------------------------------------------------------------------
c.match <- data.frame(VANID = matching_df$VANID, pb = matching_df$pb, cem_group = c.out$strata, race =matching_df$Race)

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

c.refined_comp <- c.match
# G0   G1
# All       1035663 6151
# Matched     96877 3079
# Unmatched  938786 3072
# 50% matchee
# save(voterfile, c.granular, c.college, c.base_majmatch, c.refined, c.refined_comp, file = "matchcompare_gis.RData")

#### Comparing estimates from the differently specified matches ------------------------------------------------------------------
library(lme4)
library(broom)
library(ggplot2)
source("create_pb_long.R")

fit_lme <- function(formula, df, name) {
  res <-  glmer(formula, data = df, family = binomial(), nAGQ = 0) 
  out <- tidy(res) %>% 
    mutate(source = name)
  pcp <- sum(diag(table(res@resp$y, as.numeric(fitted(res)>=.5))))/nrow(df)
  out$pcp <- pcp
  out
}

# load("matchcompare_gis.RData")

vf_analysis_base <- c.base_majmatch %>% dplyr::select(-n_treat, -n_control) %>% 
  left_join(voterfile) %>% 
  rename(comp_g_2016 = g_2016_comp, comp_g_2014 = g_2014_comp, comp_p_2014 = p_2014_comp, comp_pp_2016 = pp_2016_comp) 

vf_analysis_college <- c.college %>% dplyr::select(-n_treat, -n_control) %>% 
  left_join(voterfile) %>% 
  rename(comp_g_2016 = g_2016_comp, comp_g_2014 = g_2014_comp, comp_p_2014 = p_2014_comp, comp_pp_2016 = pp_2016_comp) 

vf_analysis_granular <- c.granular %>% dplyr::select(-n_treat, -n_control) %>% 
  left_join(voterfile) %>% 
  rename(comp_g_2016 = g_2016_comp, comp_g_2014 = g_2014_comp, comp_p_2014 = p_2014_comp, comp_pp_2016 = pp_2016_comp) 

vf_analysis_ref <- c.refined_comp %>% dplyr::select(-n_treat, -n_control) %>% 
  left_join(voterfile) %>% 
  rename(comp_g_2016 = g_2016_comp, comp_g_2014 = g_2014_comp, comp_p_2014 = p_2014_comp, comp_pp_2016 = pp_2016_comp) 

rm(voterfile)

pb_long_base <- create_pb_long(vf_analysis_base)
pb_long_college <- create_pb_long(vf_analysis_college)
pb_long_granular <- create_pb_long(vf_analysis_granular)
pb_long_ref <- create_pb_long(vf_analysis_ref)

model_form <- turned_out ~ pb + after_pb + Race + Sex + as.factor(year) + election_type + age + I(age^2) + I(age_at_vote < 18) + medhhinc_10k + white + college_pct + majmatch + (1 | VANID) + (1|NYCCD)
base <- fit_lme(model_form, pb_long_base, "base")
college <- fit_lme(model_form, pb_long_college, "Selected match")
granular <- fit_lme(model_form, pb_long_granular, "More granular")
refined <- fit_lme(model_form, pb_long_ref, "Most granular")

all <- bind_rows(base, college, granular, refined) %>% 
  mutate(source = factor(source, levels = c("base", "Selected match", "More granular", "Most granular")))
all %>% group_by(source) %>% summarize(pcp = mean(pcp))


vnames <- data.frame(old = unique(all$term), new = unique(all$term), stringsAsFactors = FALSE) %>% 
  mutate(new = str_replace_all(new, pattern = "after_pb", replacement = "After PB"),
         new = str_replace_all(new, pattern = "pb", replacement = "PB district"),
         new = str_replace_all(new, pattern = "RaceB", replacement = "Black"),
         new = str_replace_all(new, pattern = "RaceA", replacement = "Asian"),
         new = str_replace_all(new, pattern = "RaceH", replacement = "Hispanic"),
         new = str_replace_all(new, pattern = "RaceW", replacement = "White"),
         new = str_replace_all(new, pattern = "RaceU", replacement = "Unknown"),
         new = str_replace_all(new, pattern = "SexM", replacement = "Male"),
         new = str_replace_all(new, pattern = "election_typepp", replacement = "Presidential primary"),
         new = str_replace_all(new, pattern = "election_typep", replacement = "Primary election"),
         
         new = str_replace_all(new, pattern = "I\\(age\\^2\\)", replacement = "Age^2"),
         new = str_replace_all(new, pattern = "medhhinc_10k", replacement = "Median household income"),
         new = str_replace_all(new, pattern = "majmatchTRUE", replacement = "Majority race alignment"),
         new = ifelse(new == "high_school", "% High school diploma", new),
         new = ifelse(new == "college_pct", "% College degree", new),
         new = ifelse(new == "medhhinc", "Median household income", new),
         new = ifelse(new == "white", "% White", new),
         # new = ifelse(new == "black", "% Black", new),
         # new = ifelse(new == "asian", "% Asian", new),
         # new = ifelse(new == "pacislander", "% Pacific Islander", new),
         # new = ifelse(new == "latinx", "% Latinx", new),
         # new = ifelse(new == "mixed", "% Multiple races", new),
         # new = ifelse(new == "other", "% Other race", new),
         new = ifelse(new == "majmatch", "Majority race alignment", new),
         new = ifelse(new == "age", "Age", new),
         new = ifelse(new == "comp_g_2014", "Competitiveness 2014 general", new),
         new = ifelse(new == "comp_g_2016", "Competitiveness 2016 general", new),
         new = ifelse(new == "comp_p_2014", "Competitiveness 2014 primary", new),
         new = ifelse(new == "comp_pp_2016", "Competitiveness 2016 pres. primary", new)
  ) 

for (v in 1:nrow(all)) {
  all$term[v] <- vnames$new[vnames$old == all$term[v]]
}

all %>% 
    filter(!str_detect(term, '\\d\\d\\d\\d$|\\(Intercept\\)|age\\_at\\_vote')) %>% 
  filter(!str_detect(term, '\\d\\d\\d\\d$|\\(Intercept\\)|age\\_at\\_vote')) %>% 
  mutate(term = factor(term, levels = c("PB district", "After PB", "Primary election", "Presidential primary", 
                                        "Black", "Asian", "Hispanic", "Unknown", "White",
                                        "Male", "Age", "Age^2", "% College degree", "Median household income", "% White", "Majority race alignment"))) %>% 
  filter(source != "base") %>% 
ggplot() + 
  geom_hline(aes(yintercept = 0), alpha = .7) +
  geom_pointrange(aes(x = term, 
                      y = estimate, 
                      ymin = estimate - 1.96*std.error, 
                      ymax = estimate + 1.96*std.error, 
                      color = source, linetype = source, shape = source),
                  position = position_dodge(width = 1)) +
  # ylim(-2.75,3) +
  labs(x = "", y = "Coefficient estimate", color = "", linetype = "", shape = "") +
  coord_flip()+
  theme_minimal()
ggsave(file = "Paper_text/Figs/match_spec_all.pdf", width = 6, height = 7)

## The result of these investigations is a pretty resounding statement that the granularity of the match is not significant
## - estimates are pretty robustly consistent across the specifications of the match.

race_form <- turned_out ~ pb + after_pb + Race*after_pb + Sex + as.factor(year)*Race + election_type + age + I(age^2) + I(age_at_vote < 18) + medhhinc_10k + white + college_pct + majmatch + (1 | VANID) + (1|NYCCD)
base_race <- fit_lme(race_form, pb_long_base, "base")
college_race <- fit_lme(race_form, pb_long_college, "Selected match")
granular_race <- fit_lme(race_form, pb_long_granular, "More granular")
refined_race <- fit_lme(race_form, pb_long_ref, "Most granular")


all_race <- bind_rows(base_race, college_race, granular_race, refined_race) %>% 
  mutate(source = factor(source, levels = c("base", "Selected match", "More granular", "Most granular")))
all_race %>% group_by(source) %>% summarize(pcp = mean(pcp))


all_race %>% filter(!str_detect(term, '\\d\\d\\d\\d$|\\(Intercept\\)|age\\_at\\_vote')) %>% 
  filter(source != "base") %>% 
  ggplot() + 
  geom_pointrange(aes(x = term, 
                      y = estimate, 
                      ymin = estimate - 1.96*std.error, 
                      ymax = estimate + 1.96*std.error, 
                      color = source),
                  position = position_dodge(width = 1)) +
  # ylim(-3,4) +
  labs(x = "", y = "Est. coefficient", color = "") +
  coord_flip()+
  theme_minimal()
ggsave("Paper_text/Figs/match_spec_race.pdf", width = 5, height = 8)


#####
educ_form <- turned_out ~ pb + after_pb + college_pct*after_pb + Race + Sex + as.factor(year)*college_pct + election_type + age + I(age^2) + I(age_at_vote < 18) + medhhinc_10k + white + college_pct + majmatch + (1 | VANID) + (1|NYCCD)
base_educ <- fit_lme(educ_form, pb_long_base, "base")
college_educ <- fit_lme(educ_form, pb_long_college, "Selected match")
granular_educ <- fit_lme(educ_form, pb_long_granular, "More granular")
refined_educ <- fit_lme(educ_form, pb_long_ref, "Most granular")

all_educ <- bind_rows(base_educ, college_educ, granular_educ, refined_educ) %>% 
  mutate(source = factor(source, levels = c("base", "Selected match", "More granular", "Most granular")))
all_educ %>% group_by(source) %>% summarize(pcp = mean(pcp))

all_educ %>% filter(!str_detect(term, '\\d\\d\\d\\d$|\\(Intercept\\)|age\\_at\\_vote')) %>% 
  filter(source != "base") %>% 
ggplot() + 
  geom_pointrange(aes(x = term, 
                      y = estimate, 
                      ymin = estimate - 1.96*std.error, 
                      ymax = estimate + 1.96*std.error, 
                      color = source),
                  position = position_dodge(width = 1)) +
  labs(x = "", y = "Est. coefficient", color = "") +
  # ylim(-3, 5) +
  coord_flip()+
  theme_minimal()
ggsave("Paper_text/Figs/match_spec_educ.pdf", width = 6, height = 8)


ggplot(all_race) + 
  geom_pointrange(aes(x = term, 
                      y = estimate, 
                      ymin = estimate - 1.96*std.error, 
                      ymax = estimate + 1.96*std.error, 
                      color = source),
                  position = position_dodge(width = 1)) +
  ylim(-3,3) +
  coord_flip()+
  theme_minimal()

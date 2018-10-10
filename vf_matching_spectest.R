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
        g_early = list("0",c("1,2"), c("3", "4"), c("5", "6"), c("7,", "8")), 
        p_early = list("0",c("1,2"),  c("3", "4"), c("5", "6"), c("7,", "8"))
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
# All       1035760 6169
# Matched    174213 3645
# Unmatched  861547 2524
# .591 frommatching (.515 from orig)

####  Base with college added back in ------------------------------------------------------------------
rm(c.match, c.out)

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
        g_early = list("0",c("1,2"), c("3", "4"), c("5", "6"), c("7,", "8")), 
        p_early = list("0",c("1,2"),  c("3", "4"), c("5", "6"), c("7,", "8"))
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
# All       1035760 6169
# Matched    141033 3434
# Unmatched  894727 2735
# .5567 from matching

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
# All       1035760 6169
# Matched    137399 3217
# Unmatched  898361 2952
# .521 matched


save(voterfile, c.granular, c.college, c.base_majmatch, file = "matchcompare_gis.RData")

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

load("matchcompare_gis.RData")

vf_analysis_base <- c.base_majmatch %>% dplyr::select(-n_treat, -n_control) %>% 
  left_join(voterfile) %>% 
  rename(comp_g_2016 = g_2016_comp, comp_g_2014 = g_2014_comp, comp_p_2014 = p_2014_comp, comp_pp_2016 = pp_2016_comp) 

vf_analysis_college <- c.college %>% dplyr::select(-n_treat, -n_control) %>% 
  left_join(voterfile) %>% 
  rename(comp_g_2016 = g_2016_comp, comp_g_2014 = g_2014_comp, comp_p_2014 = p_2014_comp, comp_pp_2016 = pp_2016_comp) 

vf_analysis_granular <- c.granular %>% dplyr::select(-n_treat, -n_control) %>% 
  left_join(voterfile) %>% 
  rename(comp_g_2016 = g_2016_comp, comp_g_2014 = g_2014_comp, comp_p_2014 = p_2014_comp, comp_pp_2016 = pp_2016_comp) 

rm(voterfile)

pb_long_base <- create_pb_long(vf_analysis_base)
pb_long_college <- create_pb_long(vf_analysis_college)
pb_long_granular <- create_pb_long(vf_analysis_granular)

model_form <- turned_out ~ pb + after_pb + Race + Sex + as.factor(year) + election_type + age + I(age^2) + I(age_at_vote < 18) + medhhinc_10k + white + college_pct + majmatch + (1 | VANID) + (1|NYCCD)
base <- fit_lme(model_form, pb_long_base, "base")
college <- fit_lme(model_form, pb_long_college, "college")
granular <- fit_lme(model_form, pb_long_granular, "granular")

all <- bind_rows(base, college, granular)
all %>% group_by(source) %>% summarize(pcp = mean(pcp))

ggplot(all) + 
  geom_pointrange(aes(x = term, 
                      y = estimate, 
                      ymin = estimate - 1.96*std.error, 
                      ymax = estimate + 1.96*std.error, 
                      color = source),
                  position = position_dodge(width = 1)) +
  ylim(-2.75,2.75) +
  coord_flip()+
  theme_minimal()

## The result of these investigations is a pretty resounding statement that the granularity of the match is not significant
## - estimates are pretty robustly consistent across the specifications of the match.

race_form <- turned_out ~ pb + after_pb + Race*after_pb + Sex + as.factor(year)*Race + election_type + age + I(age^2) + I(age_at_vote < 18) + medhhinc_10k + white + college_pct + majmatch + (1 | VANID) + (1|NYCCD)
base_race <- fit_lme(race_form, pb_long_base, "base")
college_race <- fit_lme(race_form, pb_long_college, "college")
granular_race <- fit_lme(race_form, pb_long_granular, "granular")

all_race <- bind_rows(base_race, college_race, granular_race)
all_race %>% group_by(source) %>% summarize(pcp = mean(pcp))

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

#####
educ_form <- turned_out ~ pb + after_pb + college_pct*after_pb + Race + Sex + as.factor(year)*college_pct + election_type + age + I(age^2) + I(age_at_vote < 18) + medhhinc_10k + white + college_pct + majmatch + (1 | VANID) + (1|NYCCD)
base_educ <- fit_lme(educ_form, pb_long_base, "base")
college_educ <- fit_lme(educ_form, pb_long_college, "college")
granular_educ <- fit_lme(educ_form, pb_long_granular, "granular")

all_educ <- bind_rows(base_educ, college_educ, granular_educ)
all_educ %>% group_by(source) %>% summarize(pcp = mean(pcp))

ggplot(all_educ) + 
  geom_pointrange(aes(x = term, 
                      y = estimate, 
                      ymin = estimate - 1.96*std.error, 
                      ymax = estimate + 1.96*std.error, 
                      color = source),
                  position = position_dodge(width = 1)) +
  ylim(-3,3) +
  coord_flip()+
  theme_minimal()


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

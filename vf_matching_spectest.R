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
        g_early = list("0",c("1,2"), c("3", "4", "5"), c("6", "7,", "8")), 
        p_early = list("0",c("1,2"), c("3", "4", "5"), c("6", "7,", "8"))), 
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
        g_early = list("0",c("1,2"), c("3", "4", "5"), c("6", "7,", "8")), 
        p_early = list("0",c("1,2"), c("3", "4", "5"), c("6", "7,", "8"))), 
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

save(voterfile, c.granular, c.college, c.base_majmatch, file = "matchcompare.RData")

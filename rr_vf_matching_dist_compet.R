##### 
### Matching PB to non-PB voters

##############################################################################################################################
###
### NYC PB Voters Project 
### Matching pb/non-pb voters
### Carolina Johnson
### 
### 1. Create exact match subset to limit size
### 2. Create matching dataframe
### 3. Define CEM match thresholds
### 4. Run CEM
### 5. Create analysis dataframe from 1:1 match
### 6. Save matched dataframe to RDS
### 7. Run match balance checking
### 8. Clean up voterfile for space (for subsequent analysis)
###
##############################################################################################################################

# Load libraries
library(dplyr)
library(tidyr)
library(MatchIt)
library(cem)

# source("rr_vf_processing.R")
voterfile <- readRDS("data/cleaned_R_results/voterfile_for_matching.rds")

#### Implementing Matching, starting with exact ###-----------------------------------------------------------------------------------------------------------------------------------------------  # 
  
## Exact matching to narrow field of possibility
  
# summary(voterfile)

exact_df <- voterfile %>% 
  select(VANID, pb,  Race, agegroup, Sex, g_early, g_2008, g_2009, g_2010, 
         g_2011, p_early, p_2008, p_2009, p_2010, pp_2004, pp_2008) %>% 
  na.omit()

# votehash: tried and rejected as super hit to performance

m.exact <- matchit(pb ~ agegroup + Sex + g_early + g_2008 + g_2009 + g_2010 + 
                   g_2011 + p_early + p_2008 + p_2009 + p_2010 + pp_2004 + pp_2008, data = exact_df, method = "exact")

treat_sub <- unique(m.exact$subclass[m.exact$treat == TRUE])
table(m.exact$treat, is.na(m.exact$subclass))
## filter to rows with classes
matchable_vans <- exact_df$VANID[!is.na(m.exact$subclass)]
# saveRDS(matchable_vans, "data/cleaned_R_results/matchablevans.rds")
rm(m.exact)
gc()

### Creating matching dataframe based on the potential matches from m.exact --------------------------------------------------------------------------------
# matchable_vans <- readRDS("data/cleaned_R_results/matchablevans.rds")
matching_df <- voterfile %>%
  filter(VANID %in% matchable_vans) %>% 
  # rename(comp_g_2016 = g_2016_comp, comp_g_2014 = g_2014_comp, comp_p_2014 = p_2014_comp, comp_pp_2016 = pp_2016_comp) %>% 
  mutate(agegroup = cut(age, breaks = c(0, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60,65, 70,75, 80,85, 90, Inf))) %>% 
  select(VANID, pb, Race, agegroup, Sex, 
         g_early, g_2008, g_2009, g_2010, p_early, p_2008, p_2009, p_2010, pp_2004, pp_2008, 
         white, college,
         medhhinc , majmatch 
         , starts_with("comp")
         , white_pct, college_pct#, incumbent_2017, 
         # black_pct, noneng_pct#, votespop
         # ,g_2014_comp, g_2016_comp, p_2014_comp, pp_2016_comp
  ) %>% 
  mutate_at(vars(starts_with("comp")), replace_na, 1) %>% 
  na.omit()

# save(voterfile, matching_df, file = "vf_dataformatching_dist.RData")
# load("vf_dataformatching.RData")
### Implementing CEM - defining cutpoints for continuous variables  --------------------------------------------------------------------------------
### this uses the 'college' model from vf_matching_spectest.R.  Use that file if you need to demonstrate the fit more generally

set.seed(12018)

df_cutpoints <- list(
  white = quantile(matching_df$white, c(0,.2,.4,.6,.8,1)),
  college = quantile(matching_df$college, c(0,.5,1)),
  medhhinc = quantile(matching_df$medhhinc, c(0,.2,.4,.6,.8,1))
  , white_pct = c(0, 0.19836858, 0.49529454 , 1)
  , college_pct = c(0,0.21320295, 0.37701862, 1 )
  # , black_pct = BAMMtools::getJenksBreaks(matching_df$black_pct, 5)
  # , noneng_pct = BAMMtools::getJenksBreaks(matching_df$noneng_pct, 5)
  # , votespop = BAMMtools::getJenksBreaks(matching_df$votespop, 5)
  # , black_pct = c(0, .13, .45, 1)
  # , noneng_pct = c(0, .375, .56)
  # , votespop = quantile(matching_df$votespop, c(0,.2,.4,.6,.8,1))
  , comp_2008_primary = quantile(matching_df$comp_2008_primary, c(0, 0.5, 1))
  , comp_2009_primary = quantile(matching_df$comp_2009_primary, c(0, 0.5, 1))
  , comp_2010_general = quantile(matching_df$comp_2010_general, c(0, 0.5, 1))
  , comp_2012_primary = quantile(matching_df$comp_2012_primary, c(0, 0.5, 1))
  , comp_2014_general = quantile(matching_df$comp_2014_general, c(0, 0.5, 1))
  , comp_2014_primary = quantile(matching_df$comp_2014_primary, c(0, 0.5, 1))
  , comp_2016_primary = quantile(matching_df$comp_2016_primary, c(0, 0.5, 1))
  , comp_2017_primary = quantile(matching_df$comp_2017_primary, c(0, 0.5, 1))
)
### all the competetion is bad sad


c.out <- matching_df %>% select(-VANID) %>% 
  mutate_at(vars(starts_with("g_"), starts_with(("p_"))), as.factor) %>% 
  mutate(pp_2004 = as.factor(pp_2004),
         pp_2008 = as.factor(pp_2008),
         Race = as.factor(Race),
         Sex = as.factor(Sex)) %>% 
  cem(treatment = "pb", data = .,  
      grouping = list(
        g_early = list("0",c("1","2", "3"), c("4", "5", "6"), c("7", "8")), 
        p_early = list("0",c("1","2", "3"), c("4", "5", "6"), c("7", "8"))
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

#3702 match here, 

### creating analysis DF by joining voterfile to the CEM match output - effectively returns voterfile info filtered to matched dataset -----
vf_analysis <- c.college %>% dplyr::select(-n_treat, -n_control) %>% 
  left_join(voterfile)# %>%
  rename(comp_g_2016 = g_2016_comp, comp_g_2014 = g_2014_comp, comp_p_2014 = p_2014_comp, comp_pp_2016 = pp_2016_comp)

# vf_analysis %>% filter(pb == 1) %>% select(Race, Sex, medhhinc, college, white, g_early, p_early, age, majmatch) %>% summary()
# vf_analysis %>% filter(pb == 0) %>% select(Race, Sex, medhhinc, college, white, g_early, p_early, age, majmatch)  %>% summary()

#### Saving matched datafile as R object for future use  --------------------------------------------------------------------------------
save(vf_analysis, file = "data/cleaned_R_results/vf_analysis.RData")
save(c.college, voterfile,   file = "vf_matching.RData")

rm(list = setdiff(ls(), c("voterfile", "c.college", "vf_analysis")))
gc()
### Match balance checking ------------------------------------------------------------------------------------------------

source("pub_balance_checking.R")

### Cleaning up voterfile for space --------------------------------------------------------------------------------------------------------
rm(list = setdiff(ls(), "vf_analysis"))

gc()



    
##### 
### Matching PB to non-PB voters

##############################################################################################################################
###
### NYC PB Voters Project 
### Matching pb/non-pb voters
### Carolina Johnson
### 
### 3. Define CEM match thresholds
###
### Creating list-columns of 
###
##############################################################################################################################

library(purrr)

matching_models <- tibble(match_type = c("All vars, fine", "All vars, coarse", "Excl compet", "Excl district", "Excl tract", "Only Exact"),
                          matching_fields = vector("list", 6),
                          cutpoints = vector("list", 6),
                          grouping = vector("list", 6))

fine_cuts <- list(
  white = quantile(matching_df$white, c(0,.2,.4,.6,.8,1), na.rm = T),
  college = quantile(matching_df$college, c(0,.5,1), na.rm = T),
  medhhinc = quantile(matching_df$medhhinc, c(0,.2,.4,.6,.8,1), na.rm = T)
  , dist_white = c(0, 0.19836858, 0.49529454 , 1)
  , dist_college = c(0,0.21320295, 0.37701862, 1 )
  , comp_2008_primary = quantile(matching_df$comp_2008_primary, c(0, 0.5, 1), na.rm = T)
  , comp_2009_primary = quantile(matching_df$comp_2009_primary, c(0, 0.5, 1), na.rm = T)
  , comp_2010_general = quantile(matching_df$comp_2010_general, c(0, 0.5, 1), na.rm = T)
  , comp_2012_primary = quantile(matching_df$comp_2012_primary, c(0, 0.5, 1), na.rm = T)
  , comp_2014_general = quantile(matching_df$comp_2014_general, c(0, 0.5, 1), na.rm = T)
  , comp_2014_primary = quantile(matching_df$comp_2014_primary, c(0, 0.5, 1), na.rm = T)
  , comp_2016_primary = quantile(matching_df$comp_2016_primary, c(0, 0.5, 1), na.rm = T)
  , comp_2017_primary = quantile(matching_df$comp_2017_primary, c(0, 0.5, 1), na.rm = T)
)

coarse_cuts <- list(
  white = quantile(matching_df$white, c(0, 0.5, 1), na.rm = T),
  college = quantile(matching_df$college, c(0,.5,1), na.rm = T),
  medhhinc = quantile(matching_df$medhhinc, c(0,.5,1), na.rm = T)
  , dist_white = quantile(matching_df$dist_white, c(0,.5,1), na.rm = T)
  , dist_college = quantile(matching_df$dist_college, c(0,.5,1), na.rm = T)
  , comp_2008_primary = quantile(matching_df$comp_2008_primary, c(0, 0.5, 1), na.rm = T)
  , comp_2009_primary = quantile(matching_df$comp_2009_primary, c(0, 0.5, 1), na.rm = T)
  , comp_2010_general = quantile(matching_df$comp_2010_general, c(0, 0.5, 1), na.rm = T)
  , comp_2012_primary = quantile(matching_df$comp_2012_primary, c(0, 0.5, 1), na.rm = T)
  , comp_2014_general = quantile(matching_df$comp_2014_general, c(0, 0.5, 1), na.rm = T)
  , comp_2014_primary = quantile(matching_df$comp_2014_primary, c(0, 0.5, 1), na.rm = T)
  , comp_2016_primary = quantile(matching_df$comp_2016_primary, c(0, 0.5, 1), na.rm = T)
  , comp_2017_primary = quantile(matching_df$comp_2017_primary, c(0, 0.5, 1), na.rm = T)
)


fine_group <- list(
  g_early = list("0",c("1","2", "3"), c("4", "5", "6"), c("7", "8")), 
  p_early = list("0",c("1","2", "3"), c("4", "5", "6"), c("7", "8"))
)
coarse_group <-  list(
    g_early = list(c("0"), c("1","2", "3", "4", "5", "6", "7", "8")), 
    p_early = list(c("0"), c("1","2", "3", "4", "5", "6", "7", "8"))
  )

varlists <- list(
  allvars = names(select(voterfile, Race, agegroup, Sex, 
                   g_early, g_2008, g_2009, g_2010, p_early, p_2008, p_2009, p_2010, pp_2004, pp_2008, 
                   white, college, medhhinc , majmatch,starts_with("comp"),dist_white, dist_college, contains("incumbent"))),
  excl_compet = names(select(voterfile, Race, agegroup, Sex, 
                             g_early, g_2008, g_2009, g_2010, p_early, p_2008, p_2009, p_2010, pp_2004, pp_2008, 
                             white, college, medhhinc, majmatch, dist_white, dist_college, contains("incumbent"))),
  excl_district = names(select(voterfile, Race, agegroup, Sex, 
               g_early, g_2008, g_2009, g_2010, p_early, p_2008, p_2009, p_2010, pp_2004, pp_2008, 
               white, college, medhhinc , majmatch)),
  excl_tract = names(select(voterfile, Race, agegroup, Sex, 
                            g_early, g_2008, g_2009, g_2010, p_early, p_2008, p_2009, p_2010, pp_2004, pp_2008, 
                            dist_white, dist_college)),
  only_exact = names(select(voterfile, Race, agegroup, Sex, 
                            g_early, g_2008, g_2009, g_2010, p_early, p_2008, p_2009, p_2010, pp_2004, pp_2008))
)

matching_models$matching_fields <- list(varlists$allvars, 
                                        varlists$allvars, 
                                        varlists$excl_compet,
                                        varlists$excl_district,
                                        varlists$excl_tract,
                                        varlists$only_exact)

keep_vars <- function(cutlist, name_set) {cutlist[intersect(names(cutlist), varlists[[name_set]])]}

matching_models$cutpoints <- list(fine_cuts,
                                  coarse_cuts,
                                  keep_vars(fine_cuts, "excl_compet"),
                                  keep_vars(fine_cuts, "excl_district"),
                                  keep_vars(fine_cuts, "excl_tract"),
                                  keep_vars(fine_cuts, "only_exact"))


matching_models$grouping <- list(fine_group,
                                  coarse_group,
                                  keep_vars(fine_group, "excl_compet"),
                                  keep_vars(fine_group, "excl_district"),
                                  keep_vars(fine_group, "excl_tract"),
                                  keep_vars(fine_group, "only_exact"))

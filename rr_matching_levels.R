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

match_names <- c("Only exact",
                 "Tract super",
                 "Tract fine",
                 "Tract coarse",
                 "Dist comp", 
                 "Dist white", 
                 "Dist medhhinc", 
                 "Dist college", 
                 "incumb. 13",
                 "incumb. 17",
                 "Group dist",
                 "Dist + tract",
                 "Dist + compet",
                 "Compet",
                 "Compet + tract, fine",
                 "Compet + tract, coarse",
                 "All, coarse",
                 "All, fine")
granular <- stringr::str_extract(match_names, ("fine|coarse|super")) %>% 
  replace_na("fine")
            
matching_models <- tibble(match_type = match_names,
                          matching_fields = vector("list", length(match_names)),
                          cutpoints = vector("list", length(match_names)),
                          grouping = vector("list", length(match_names)))


## load threshold lists (runn against full matching_df - generated in )
load("data/cleaned_R_results/cutpoints.Rdata")


super_group <- list(
  g_early = list("0", c("1","2"), c("3", "4"), c("5", "6"), c("7", "8")), 
  p_early = list("0", c("1","2"), c("3", "4"), c("5", "6"), c("7", "8"))
)

super_cuts <- fine_cuts
super_cuts$age <- c(seq(-0.5, 89.5, 5), Inf)

fine_group <- list(
  g_early = list("0",c("1","2", "3"), c("4", "5", "6"), c("7", "8")), 
  p_early = list("0",c("1","2", "3"), c("4", "5", "6"), c("7", "8"))
)
coarse_group <-  list(
    g_early = list(c("0"), c("1","2", "3", "4", "5", "6", "7", "8")), 
    p_early = list(c("0"), c("1","2", "3", "4", "5", "6", "7", "8")),
    agegroup = list(c("(0,18.5]", "(18.5, 25.5]"), c("(25.5,39.5]", "(39.5,49.5]", "(49.5,59.5]"), c("(59.5,69.5]", "(69.5,79.5]", "(79.5,Inf]"))
  )

varlists <- list(
  exact = names(select(matching_df, Race, agegroup, Sex, 
                       g_early, g_2008, g_2009, g_2010, p_early, p_2008, p_2009, p_2010, pp_2008)),
  tract_super = names(select(matching_df, Race, age, Sex, 
                             g_early, g_2008, g_2009, g_2010, p_early, p_2008, p_2009, p_2010, pp_2008, 
                             white, college, medhhinc , majmatch)),
  tract = names(select(matching_df, Race, agegroup, Sex, 
                       g_early, g_2008, g_2009, g_2010, p_early, p_2008, p_2009, p_2010, pp_2008, 
                       white, college, medhhinc , majmatch)),
  dist_comp = names(select(matching_df, Race, agegroup, Sex, 
                               g_early, g_2008, g_2009, g_2010, p_early, p_2008, p_2009, p_2010, pp_2008, 
                               white, college, medhhinc , majmatch, contains("jenks"))),
  dist_white = names(select(matching_df, Race, agegroup, Sex, 
                           g_early, g_2008, g_2009, g_2010, p_early, p_2008, p_2009, p_2010, pp_2008, 
                           white, college, medhhinc , majmatch,  dist_white)),
  dist_medhhinc = names(select(matching_df, Race, agegroup, Sex, 
                           g_early, g_2008, g_2009, g_2010, p_early, p_2008, p_2009, p_2010, pp_2008, 
                           white, college, medhhinc , majmatch, dist_medhhinc)),
  dist_college = names(select(matching_df, Race, agegroup, Sex, 
                           g_early, g_2008, g_2009, g_2010, p_early, p_2008, p_2009, p_2010, pp_2008, 
                           white, college, medhhinc , majmatch, dist_college)),
  dist_inc13 = names(select(matching_df, Race, agegroup, Sex, 
                           g_early, g_2008, g_2009, g_2010, p_early, p_2008, p_2009, p_2010, pp_2008, 
                           white, college, medhhinc , majmatch, incumbent_2013)),
  dist_inc17 = names(select(matching_df, Race, agegroup, Sex, 
                           g_early, g_2008, g_2009, g_2010, p_early, p_2008, p_2009, p_2010, pp_2008, 
                           white, college, medhhinc , majmatch, incumbent_2017)),
  dist_only = names(select(matching_df, Race, agegroup, Sex, 
                            g_early, g_2008, g_2009, g_2010, p_early, p_2008, p_2009, p_2010, pp_2008, 
                            match_group)),
  dist_tract = names(select(matching_df, Race, agegroup, Sex, 
                            g_early, g_2008, g_2009, g_2010, p_early, p_2008, p_2009, p_2010, pp_2008, 
                            white, college, medhhinc, majmatch, match_group)),
  dist_compet = names(select(matching_df, Race, agegroup, Sex, 
                            g_early, g_2008, g_2009, g_2010, p_early, p_2008, p_2009, p_2010, pp_2008, 
                            starts_with("comp_"), match_group)),
  compet_only = names(select(matching_df, Race, agegroup, Sex, 
                                g_early, g_2008, g_2009, g_2010, p_early, p_2008, p_2009, p_2010, pp_2008, 
                                starts_with("comp_"))),
  compet_tract = names(select(matching_df, Race, agegroup, Sex, 
                             g_early, g_2008, g_2009, g_2010, p_early, p_2008, p_2009, p_2010, pp_2008, 
                             white, college, medhhinc , majmatch, starts_with("comp_"))),
  allvars = names(select(matching_df, Race, agegroup, Sex, 
                       g_early, g_2008, g_2009, g_2010, p_early, p_2008, p_2009, p_2010, pp_2008, 
                       white, college, medhhinc , majmatch,starts_with("comp_"), match_group)))

matching_models$matching_fields <- list(varlists$exact,
                                        varlists$tract_super,
                                        varlists$tract,
                                        varlists$tract,
                                        varlists$dist_comp,
                                        varlists$dist_white,
                                        varlists$dist_medhhinc,
                                        varlists$dist_college,
                                        varlists$dist_inc13,
                                        varlists$dist_inc17,
                                        varlists$dist_only,
                                        varlists$dist_tract,
                                        varlists$dist_compet,
                                        varlists$compet_only,
                                        varlists$compet_tract,
                                        varlists$compet_tract,
                                        varlists$allvars,
                                        varlists$allvars)

keep_vars <- function(cutlist, name_set) {cutlist[intersect(names(cutlist), name_set)]}

for (i in 1:length(match_names)) {
  if (granular[i] == "fine") {
    matching_models$cutpoints[[i]] <- keep_vars(fine_cuts, matching_models$matching_fields[[i]])
  } else if (granular [i] == "coarse") {
    matching_models$cutpoints[[i]] <- keep_vars(coarse_cuts, matching_models$matching_fields[[i]])
  }
  else if (granular [i] == "super") {
    matching_models$cutpoints[[i]] <- keep_vars(super_cuts, matching_models$matching_fields[[i]])
  }
}


for (i in 1:length(match_names)) {
  if (granular[i] == "fine") {
    matching_models$grouping[[i]] <- keep_vars(fine_group, matching_models$matching_fields[[i]])
  } else if (granular [i] == "coarse") {
    matching_models$grouping[[i]] <- keep_vars(coarse_group, matching_models$matching_fields[[i]])
  }
  else if (granular [i] == "super") {
    matching_models$grouping[[i]] <- keep_vars(super_group, matching_models$matching_fields[[i]])
  }
}
# 
# 
# 
#   map2()
#   list(fine_cuts,
#                                   coarse_cuts,
#                                   keep_vars(fine_cuts, "excl_compet"),
#                                   keep_vars(fine_cuts, "dist_comp"),
#                                   keep_vars(fine_cuts, "dist_white"),
#                                   keep_vars(fine_cuts, "dist_medhhinc"),
#                                   keep_vars(fine_cuts, "dist_college"),
#                                   keep_vars(fine_cuts, "dist_inc13"),
#                                   keep_vars(fine_cuts, "dist_inc17"),
#                                   keep_vars(fine_cuts, "excl_dist"),
#                                   keep_vars(fine_cuts, "excl_comp_dist"))
# 
# 
# matching_models$grouping <- list(fine_group,
#                                   coarse_group,
#                                  keep_vars(fine_group, "excl_compet"),
#                                  keep_vars(fine_group, "dist_comp"),
#                                  keep_vars(fine_group, "dist_white"),
#                                  keep_vars(fine_group, "dist_medhhinc"),
#                                  keep_vars(fine_group, "dist_college"),
#                                  keep_vars(fine_group, "dist_inc17"),
#                                  keep_vars(fine_group, "dist_inc13"),
#                                  keep_vars(fine_group, "excl_dist"),
#                                  keep_vars(fine_group, "excl_comp_dist"))

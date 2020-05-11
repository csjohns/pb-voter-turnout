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

match_names <- c("All vars, fine", "All vars, coarse", "Excl compet", "Only Exact")
matching_models <- tibble(match_type = match_names,
                          matching_fields = vector("list", length(match_names)),
                          cutpoints = vector("list", length(match_names)),
                          grouping = vector("list", length(match_names)))


## load threshold lists (runn against full matching_df - generated in )
load("data/cleaned_R_results/cutpoints.Rdata")

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
  allvars = names(select(matching_df, Race, agegroup, Sex, effective_district,
                   g_early, g_2008, g_2009, g_2010, p_early, p_2008, p_2009, p_2010, pp_2008, 
                   white, college, medhhinc , majmatch,starts_with("comp_"))),
  excl_compet = names(select(matching_df, Race, agegroup, Sex, effective_district,
                             g_early, g_2008, g_2009, g_2010, p_early, p_2008, p_2009, p_2010, pp_2008, 
                             white, college, medhhinc, majmatch)),
  only_exact = names(select(matching_df, Race, agegroup, Sex, effective_district,
                            g_early, g_2008, g_2009, g_2010, p_early, p_2008, p_2009, p_2010, pp_2008))
)

matching_models$matching_fields <- list(varlists$allvars, 
                                        varlists$allvars, 
                                        varlists$excl_compet,
                                        varlists$only_exact)

keep_vars <- function(cutlist, name_set) {cutlist[intersect(names(cutlist), varlists[[name_set]])]}

matching_models$cutpoints <- list(fine_cuts,
                                  coarse_cuts,
                                  keep_vars(fine_cuts, "excl_compet"),
                                  keep_vars(fine_cuts, "only_exact"))


matching_models$grouping <- list(fine_group,
                                  coarse_group,
                                  keep_vars(fine_group, "excl_compet"),
                                  keep_vars(fine_group, "only_exact"))
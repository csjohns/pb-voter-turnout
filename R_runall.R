
##############################################################################################################################
###
### NYC PB Voters Project 
### Header file to (re)run all processing
### Carolina Johnson
### 
### Calls each script in sequence. Recommend restarting R session between new file runs to ensure clean environment and R session
### Online Appenix is reproduced in a separate RMarkdown file
###
##############################################################################################################################


# 1 process whole file for matching
source("rr_vf_processing_for_matching.R")

# 2a - run first in different session (loads and saves to disc, no objects produced for working environment)
source("rr_matching_cutpoints.R")

# 2 run CEM matching - run three times altering suffix each time
rm(list = ls())
suffix <- ""
source("rr_vf_matching_iterate.R")

rm(list = ls())
suffix <- "_within_dist"
source("rr_vf_matching_iterate.R")

rm(list = ls())
suffix <- "_placebo"
source("rr_vf_matching_iterate.R")

# 3 - regression iterations over model and match specification - standard and within district:
rm(list = ls())
suffix <- ""
source("rr_vf_regression_iterate.R")

rm(list = ls())
suffix <- "_within_dist"
source("rr_vf_regression_iterate.R")

# 3-2 regression iterations for placebo model (fewer iterations as computationally more demanding)
rm(list = ls())
source("rr_vf_regression_iterate_placebo.R")

# 4 load all and create match comparison figure for paper
rm(list = ls())
source("rr_vf_regression_compare_method.R")

# 5 final model 
rm(list = ls())
source("rr_vf_regression_final.R")

# 6 interactions and predictions
rm(list = ls())
source("rr_vf_regression_preds.R")

# 7 descriptive stats
source("descriptives.R")

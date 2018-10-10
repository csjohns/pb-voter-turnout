## code to fill in easy missing NYCCDs from the ed/cd GIS mapping (missing some districts, but better than none :/)
library(stringr)

nyccds <- read.csv("ed-nyccd-map.csv", as.is = TRUE)
nyccds <- nyccds %>% 
  mutate(ED = paste0("Ad ", str_sub(ElectDist, 1, 2), " - Ed ", str_sub(ElectDist, 3,5)))

missing <- which(is.na(vf_analysis$NYCCD))
for (i in missing){
    vf_analysis$NYCCD[i] <- min(nyccds$CounDist[nyccds$ED == vf_analysis$ED[i]])
}
# 
# 
# nocd <- vf_analysis %>% filter(is.na(NYCCD))
# 
# cdfill <- read.csv("partial_file_NYCCD.csv", as.is = TRUE)
# names(cdfill)[1] <- "VANID"
# sum(cdfill$VANID %in% vf_analysis$VANID)
# dim(cdfill)
# 
# for (i in 1:nrow(vf_analysis)){
#   if(is.na(vf_analysis$NYCCD[i])){
#     vf_analysis$NYCCD[i] <- cdfill$NYCCD[cdfill$VANID == vf_analysis$VANID[i]]
#   }
# }
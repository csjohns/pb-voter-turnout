### Reading in the original voter files, combining into a single file, and exporting to send to Sonya
library(dplyr)
library(tidyr)
library(stringr)
library(readxl)
library(xlsx)
library(lubridate)

fs_xls <- list.files("Originals/", pattern = "*.xlsx", full.names = TRUE)
fs_csv <- list.files("Originals/", pattern = "D.*..csv", full.names = TRUE)


xls <- list()
for (f in seq_along(fs_xls)) {
  data <- read_excel(fs_xls[f], na = c("","12/31/1899", "1899-12-31")) 
  if("X__1" %in% names(data)) { data <- select(data, -X__1)}
  data$sourcefile <- fs_xls[f]
  xls[[f]] <- data
    # %>% 
  # mutate(`Birth Date` = ifelse(is.character(`Birth Date`), mdy(`Birth Date`), `Birth Date`))
}

cbind(fs_xls, lapply(xls, function(x) class(x$`Birth Date`)))


csv <- list()
for (f in seq_along(fs_csv)) {
  data <- read.csv(fs_csv[f], as.is = TRUE) %>%
    rename(`Name` = FIRST.NAME,
           `Last Name` = LAST.NAME) %>%
    mutate(Address = paste(ADDRESS.1, ADDRESS.2, "," , CITY,  ",", STATE, ZIP),
           `NYC District` = 39,
           sourcefile = fs_csv[f],
           cycle = "5"
           ) %>%
    select(Name, `Last Name`, Address, `NYC District`, sourcefile, cycle)
  csv[[f]] <- data
}

alldistricts <- bind_rows(xls) %>%
  mutate(cycle = substring(sourcefile, str_length(sourcefile)-5, str_length(sourcefile)-5))
  
alldistricts <-  bind_rows(csv) %>% bind_rows(alldistricts, .) 

alldistricts <- alldistricts %>%
  select(-sourcefile) %>%
  rename(`PB Vote District` = `NYC District`) 


write.csv(alldistricts, "Originals/combined_originals_updated.csv", row.names = FALSE, na = "")

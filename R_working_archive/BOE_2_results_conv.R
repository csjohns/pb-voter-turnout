# BOE to results format data:

# 1) row is ED (must carefully combine ad and ed numbers, with appropriate padding)
# 2) filter out rows that are not actually votes for candidates
# 3) collect district numbers if they exist
# 4) collect office using the categories provided - expand with Sonya's input
# 5) Collect party (to create the unique party-candidate pairs for each ballot line)
# 6) split election date into year, month, and day collumns

nyc14 <- read.csv("BOE_results_forscripting.csv", as.is = T)
nyc <- nyc14 %>% 
  filter(!Unit.Name %in% c("Affidavit", "Absentee/Military", "Federal", "Public Counter", "Scattered", "Emergency")) %>%
  rename(ed = ED) %>%
  mutate(ED  = paste("AD ", AD, " - Ed ", str_pad(ed, width = 3, side = "left", pad = 0), sep = "")) %>%
  separate(Event, c("election_type", "date"), sep = " - ") %>%
  separate(date, c("ElectionMonth", "ElectionDay", "ElectionYear"), sep = "/") %>%
  mutate_at(vars(ElectionMonth, ElectionDay, ElectionYear), as.numeric) %>%
  separate(Unit.Name, c("Candidate", "Party"), sep = " \\(") %>%
  mutate(Party = str_replace(Party, "\\)", "")) %>%
  rename(DistrictNumber = District.Key) %>%
  mutate(DistrictNumber = as.numeric(DistrictNumber)) %>%
  rename(Office = Office.Position.Title,
         VoteCount = Tally) %>%
  select(Office, County, ED, ElectionYear, ElectionMonth, ElectionDay, DistrictNumber, Candidate, Party,VoteCount)
write.csv(nyc, "nyc14_governor.csv", row.names = FALSE)
  
tr <- bind_rows(results, nyc)


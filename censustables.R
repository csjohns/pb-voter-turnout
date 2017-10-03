## Joining census to pb table

#Download census tables:
library(acs)
geo <- geo.make(state = "NY", county = c(005, 047, 061, 085, 081), tract = "*")

tractID <- function(acsdata){
  paste0(
    str_pad(acsdata@geography$county, 3, "left", pad="0"),
    str_pad(acsdata@geography$tract, 6, "left", pad="0")
  )
}

# Population counts
B03002 <- acs.fetch(endyear = 2015,  geography = geo,
                    table.number = "B03002", col.names = "pretty")
race <- data.frame(
  tract = tractID(B03002),
  B03002@estimate,
  stringsAsFactors = FALSE
)

<<<<<<< HEAD
race <- race %>% 
  transmute(tract = tract,
         white = Hispanic.or.Latino.by.Race..Not.Hispanic.or.Latino./Hispanic.or.Latino.by.Race..Total.,
         black = Hispanic.or.Latino.by.Race..Not.Hispanic.or.Latino..Black.or.African.American.alone/Hispanic.or.Latino.by.Race..Total.,
         asian = Hispanic.or.Latino.by.Race..Not.Hispanic.or.Latino..Asian.alone/Hispanic.or.Latino.by.Race..Total.,
         pacislander = Hispanic.or.Latino.by.Race..Not.Hispanic.or.Latino..Native.Hawaiian.and.Other.Pacific.Islander.alone/Hispanic.or.Latino.by.Race..Total.,
         latinx = Hispanic.or.Latino.by.Race..Hispanic.or.Latino./Hispanic.or.Latino.by.Race..Total.,
         mixed = Hispanic.or.Latino.by.Race..Not.Hispanic.or.Latino..Two.or.more.races./Hispanic.or.Latino.by.Race..Total.,
         other = Hispanic.or.Latino.by.Race..Not.Hispanic.or.Latino..Some.other.race.alone/Hispanic.or.Latino.by.Race..Total.)

=======
>>>>>>> 3421ce28f7c2401a8857679ff010e7e69bcf6114


## MED HH INCOME
B19013 <- acs.fetch(endyear = 2015,  geography = geo,
                    table.number = "B19013", col.names = "pretty", case.sensitive = F)
colnames(B19013@estimate)

inc <- data.frame(
  tract = tractID(B19013),
  B19013@estimate,
  stringsAsFactors = FALSE
)

inc <- inc %>% rename(medhhinc = B19013..Median.Household.Income.in.the.Past.12.Months..in.2015.Inflation.Adjusted.Dollars...Median.household.income.in.the.past.12.months..in.2015.Inflation.adjusted.dollars.)

##EDUCATION

B15003 <- acs.fetch(endyear = 2015,  geography = geo,
                    table.number = "B15003", col.names = "pretty")

educ <- data.frame(
  tract = tractID(B15003),
  B15003@estimate,
  stringsAsFactors = FALSE
)

educ <- educ %>%
  mutate(high_school = ((Educational.Attainment.for.the.Population.25.Years.and.Over..Regular.high.school.diploma +
                           Educational.Attainment.for.the.Population.25.Years.and.Over..Some.college..less.than.1.year +
                           Educational.Attainment.for.the.Population.25.Years.and.Over..Some.college..1.or.more.years..no.degree +
                           Educational.Attainment.for.the.Population.25.Years.and.Over..Associate.s.degree +
                           Educational.Attainment.for.the.Population.25.Years.and.Over..Bachelor.s.degree +
                           Educational.Attainment.for.the.Population.25.Years.and.Over..Master.s.degree +
                           Educational.Attainment.for.the.Population.25.Years.and.Over..Professional.school.degree +
                           Educational.Attainment.for.the.Population.25.Years.and.Over..Doctorate.degree)/Educational.Attainment.for.the.Population.25.Years.and.Over..Total.)*100,
         college = ((Educational.Attainment.for.the.Population.25.Years.and.Over..Bachelor.s.degree +
                       Educational.Attainment.for.the.Population.25.Years.and.Over..Master.s.degree +
                       Educational.Attainment.for.the.Population.25.Years.and.Over..Professional.school.degree +
                       Educational.Attainment.for.the.Population.25.Years.and.Over..Doctorate.degree)/Educational.Attainment.for.the.Population.25.Years.and.Over..Total.)*100
  ) %>%
  select(tract, high_school, college)

pb <- pb %>% mutate(countycode = recode(County, BRONX = "005", KINGS = "047", `NEW YORK` = "061", QUEENS = "081", RICHMOND = "085")) %>%
  mutate(countycode = ifelse(countycode %in% c("005", "047", "061", "081", "085"), countycode, NA),
         tract = paste0(countycode, str_pad(CensusTract, 6, "left", "0")))

pbt <- pb %>% 
<<<<<<< HEAD
  left_join(educ) %>%
  left_join(inc) %>% 
  left_join(race)

sum(is.na(pbt$medhhinc))
=======
  left_join(educ)

sum(is.na(pbt$high_school))
>>>>>>> 3421ce28f7c2401a8857679ff010e7e69bcf6114
pbt$City[is.na(pbt$high_school)]

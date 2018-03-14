##### 
### Matching PB to non-PB voters

library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)
library(data.table)
library(MatchIt)
library(cem)

#source("credentials.R") # loads the access credentials
#source("dbDownload.R")

stringNAs <- function(x){
  ifelse(x, "", NA)
}

### Load PB data ### -------------------------------------------------------------------------------------

pb <- dbDownload(table = "pb", username = username, password = password, dbname = db.name, host = hostname, port = port)
rm(password, username, hostname, db.name, port) # if you want to remove the credentials from your environment 
# load("pb_orig.Rdata")
#pb <- pb_orig 
#rm(pb_orig)
pb <- pb %>% select(-DWID, -row_names) %>% 
  filter(DoR != "" & !is.na(DoR))
pb <- pb %>% mutate(DoB = mdy(DoB),
                    pb = 1)


### Load full voterfile data ### -------------------------------------------------------------------------------------
### Limit it to only non-PB districts and VANIDS


load("pbdistricts.Rdata")
pbdistricts <- na.omit(pbdistricts)

# loading full voter file
voterfile <- fread("personfileFULL20170731-15112428081/personfileFULL20170731-15112428081.txt")
voterfile <- voterfile[!CityCouncilName %in% pbdistricts & !`Voter File VANID` %in% pb$VANID & !is.na(CityCouncilName) & RegistrationStatusName != "Applicant"]

voterfile <- as.data.frame(voterfile)

voterfile <- voterfile  %>%
  rename(Ethnicity = EthnicCatalistName,
         DoB = DOB,
         DoR = DateReg,
         RegStatus = RegistrationStatusName,
         County = CountyName,
         #City = CityName,
         Zip = Zip5,
         Lat = Latitude,
         Long = Longitude,
         NYCCD = CityCouncilName,
         CensusTract = CensusTractName,
         VANID = `Voter File VANID`,
         ED = PrecinctName)

voterfile <- voterfile  %>%
  mutate(County = recode(County, Bronx = "BRONX" , Kings = "KINGS", `New York` = "NEW YORK", Queens = "QUEENS", Richmond = "RICHMOND")) %>% 
  mutate(DoB = mdy(DoB))
  
  names(voterfile) <- names(voterfile) %>% 
    str_replace("General", "g_20") %>%
    str_replace("PresidentialPrimary", "pp_20") %>%
    str_replace("Primary", "p_20")
  
  voterfile <- voterfile %>%
    select(-starts_with("Special"), -ends_with("Party")) %>%
    select(-Lat, -Long, -starts_with("Street"), -starts_with("Apt"), - VHHID, -StateFileID, -DWID, 
           -starts_with("Reported"))
  voterfile <- voterfile %>% 
    filter(DoR != "")
  
  ### Load compare structure of the two data frames ### -------------------------------------------------------------------------------------
  
  setdiff(names(pb), names(voterfile))
  setdiff(names(voterfile), names(pb))
  # 4618885 + nrow(pb)
  ### Join to voter file ### -------------------------------------------------------------------------------------
  voterfile <- pb %>% 
    filter(pbdistrict %in% c(23, 39)) %>% ## this is filtering to only districts we have full/near full data for
    #select(-row_names, -DWID,VANID, starts_with("pb")) %>% 
    #full_join(voterfile, .)
    bind_rows(voterfile)
  
  
  
  convLogicVote <- function(x){as.numeric(x != "")} 
    
  
  voterfile <- voterfile %>% mutate(pb = ifelse(is.na(pb), 0, pb))
  voterfile <- voterfile %>% 
    mutate_at(vars(starts_with("p_")), funs(convLogicVote)) %>% 
    mutate_at(vars(starts_with("g_")), funs(convLogicVote)) %>% 
    mutate_at(vars(starts_with("pp_")), funs(convLogicVote)) 
  
  voterfile <- voterfile %>% 
    rowwise() %>% 
    mutate(age = year(Sys.Date()) - year(DoB),
           g_early = sum(g_2000, g_2001, g_2002, g_2003, g_2004, g_2005, g_2006, g_2007, na.rm = TRUE),
           p_early = sum(p_2000, p_2001, p_2002, p_2003, p_2004, p_2005, p_2006, p_2007, na.rm = TRUE)) %>% 
    group_by()
  
### Including census data ### ----------------------------------------------------------------------------------------------------------------------------
  #source("censustables.R")
  load("census.Rdata")
  voterfile <- voterfile %>% filter(County %in% c("BRONX", "KINGS", "NEW YORK", "QUEENS", "RICHMOND")) %>% 
    mutate(countycode = recode(County, BRONX = "005", KINGS = "047", `NEW YORK` = "061", QUEENS = "081", RICHMOND = "085"),
           tract = paste0(countycode, str_pad(CensusTract, 6, "left", "0")))
  # voterfile$countycode <- recode(voterfile$County, BRONX = "005", KINGS = "047", `NEW YORK` = "061", QUEENS = "081", RICHMOND = "085")
  # voterfile$tract <- paste0(voterfile$countycode, str_pad(voterfile$CensusTract, 6, "left", "0"))

  # voterfile <- voterfile %>% 
  #  mutate(countycode = recode(County, BRONX = "005", KINGS = "047", `NEW YORK` = "061", QUEENS = "081", RICHMOND = "085"),
  #         tract = paste0(countycode, str_pad(CensusTract, 6, "left", "0")))
  gc()
  voterfile <- voterfile %>% 
    left_join(educ) %>%
    left_join(inc) %>% 
    left_join(race)
  
  voterfile <- voterfile %>% 
    mutate(agegroup = cut(age, breaks = c(0, 20, 30, 40, 50, 60,70,80,Inf)))
    
### MATCHING EXPERIMENTS ###-----------------------------------------------------------------------------------------------------------------------------------------------  # 
  
## Exact matching to narrow field of possibility
  
  summary(voterfile)
  
  exact_df <- voterfile %>% 
    select(VANID, pb,  Race, agegroup, Sex, g_early, g_2008, g_2009, g_2010, g_2011, p_early, p_2008, p_2009, p_2010, pp_2004, pp_2008) %>% 
    na.omit()
  
  m.exact <- matchit(pb ~ Race +agegroup +Sex + g_early + g_2008 + g_2009 + g_2010 + g_2011 + p_early + p_2008 + p_2009 + p_2010 + pp_2004 + pp_2008, data = exact_df, method = "exact")
  
  treat_sub <- unique(m.exact$subclass[m.exact$treat == TRUE])
  table(m.exact$treat, is.na(m.exact$subclass))
  ## filter to rows with classes
  matchable_vans <- exact_df$VANID[!is.na(m.exact$subclass)]
  
  rm(m.exact)
  gc()
  
  
  
  ### Creating matching dataframe based on the potential matches from m.exact --------------------------------------------------------------------------------
  matching_df <- voterfile %>%
    filter(VANID %in% matchable_vans) %>% 
    mutate(agegroup = cut(age, breaks = c(0, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60,65, 70,75, 80,85, 90, Inf))) %>% 
    select(VANID, pb, Race, agegroup, Sex, g_early, g_2008, g_2009, g_2010, g_2011, p_early, p_2008, p_2009, p_2010, pp_2004, pp_2008, white, college, medhhinc) %>% 
    na.omit()
  
  #m.out <- matchit(pb ~ Race + age + Sex + g_early + g_2008 + g_2009 + g_2010 + g_2011 + p_early + p_2008 + p_2009 + p_2010 + pp_2004 + pp_2008 + white + high_school + medhhinc,
  #          data = matching_df, method = "cem", k2k = TRUE)
#ps  <- pair(m.out, matching_df)
### Implementing CEM - defining cutpoints for continuous variables  --------------------------------------------------------------------------------
  
df_cutpoints <- list(
  white = quantile(matching_df$white, c(0,.2,.4,.6,.8,1)),
  #college = quantile(matching_df$college, c(0,.5,1)),
  medhhinc = quantile(matching_df$medhhinc, c(0,.2,.4,.6,.8,1))
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
    
    ### creating analysis DF by joining voterfile to the CEM match output
    vf_analysis <- c.match %>% select(-n_treat, -n_control) %>% 
      left_join(voterfile)
    
    vf_analysis %>% filter(pb == 1) %>% select(Race, Sex, medhhinc, college, white, g_early, p_early, age) %>% summary()
    vf_analysis %>% filter(pb == 0) %>% select(Race, Sex, medhhinc, college, white, g_early, p_early, age)  %>% summary()
    #### filtering voter file to match -------------------------------------
    # vf_analysis <- voterfile %>% filter(VANID %in% c.match$VANID)
    

####  replicating transformation and first regressions with the matched data -------------------------------------------------------------------------------------
    
    # Dealing with DoB, calculating total PB votes (not that these will usually be off by 1 because 2014 has too many voters (everyone who came before))
    pb <- vf_analysis %>% 
      group_by(VANID) %>%
      mutate(DoB = ymd(DoB),
             totpb = sum(pb_2012, pb_2013, pb_2014, pb_2015 , pb_2016, na.rm = T)
      )
    
    
    ## Reshaping long for regression ----------------------------------------------------------------
    pb_long <- pb %>% filter(totpb >0) %>% select(VANID, totpb, starts_with("pb_")) %>%
      gather( year, pb, starts_with("pb_")) %>%
      mutate(year = as.numeric(str_replace(year, "pb_", "")),
             pb = as.numeric(pb))
      glimpse(pb_long)
    summary(pb_long)
    
    # this code calculates pb start year - not that this code is good even though 2014 is a wacky error year since voters who didn't vote before 2014 will indeed have their start year be 2014
    pb_long <- pb_long %>% group_by(VANID) %>%
      arrange(VANID, year) %>%
      mutate(pbyear  = ifelse(pb == 1, year, NA),
             pb_start = min(pbyear, na.rm = TRUE)
      )
    summary(pb_long)
    
    elec_long <- pb %>% select(-starts_with("pb_")) %>%
      gather(election, turned_out, starts_with("p_2"), starts_with("g_2"), starts_with("pp_")) %>%
      separate(election, c("election_type", "year")) %>%
      mutate(#turned_out = ifelse(turned_out != "", 1, 0),
             year = as.numeric(year)) 
    
    pb_long <- pb_long %>% select(-pb) %>% full_join(elec_long)
    pb_long <- pb_long %>%
      mutate(pb = ifelse(is.na(pb), 0, pb),
             after_pb = as.numeric(year > pb_start),
             after_pb = ifelse(is.na(after_pb), 0, after_pb),
             # repeater = totpb > 1, removing this because errors in 2014 means every early voter is a repeater, which isn't correct
             age_at_vote = year - year(DoB) )
    ## I think there are some nonsense ages in here and I need to investigate DoB coding more
    
    pb_long <- pb_long %>%  filter(year >= 2008)
    
    
    ## simple logit model with covariates ----------------------------------------
    #dist39_groups <- unique(pb$cem_group[pb$NYCCD ==39 | pb$pbdistrict == 39])
    
    library(margins)
    covar_formula <- turned_out ~ pb + after_pb + as.factor(year) +  election_type  + Race + age + Sex + medhhinc #+ Sex commented out for new data - prefect prediction in Sex- not enough variation in all xtabs. Could make sex Binary and it would run
    covar_logit <- pb_long %>% glm(covar_formula, data = ., family = binomial())
    summary(covar_logit)
    dydx(pb_long, covar_logit, "after_pb", change = c(0,1))[[1]] %>% mean
    covar_lm <- lm(covar_formula, data = pb_long, family = binomial())
    summary(covar_lm)
    
    ## Trying with lmer getting random effects for individuals
    library(lme4)
    
    # only39 <- pb_long %>% filter(cem_group %in% dist39_groups) %>% group_by() %>% 
    #   mutate(turned_out = ifelse(is.na(turned_out), 0, turned_out),
    #          VANID = as.factor(VANID), 
    #          NYCCD = as.factor(NYCCD))
    ggplot(pb_long) + geom_bar(aes(x = as.factor(turned_out), fill = election_type), stat = "count", position = "dodge") + facet_wrap(~year)
    ggplot(pb_long) + geom_bar(aes(x = as.factor(turned_out), fill = as.factor(after_pb)), position = "dodge") + facet_wrap(~year)
    
    bas_log <- glm(turned_out ~ pb + after_pb + as.factor(year) + election_type , data = pb_long)
    
## making FE dummies
library(simcf)
pb_long_orig <- pb_long

pb_long <- pb_long_orig
raceFE <- makeFEdummies(pb_long$Race) 
colnames(raceFE) <- paste("race", colnames(raceFE), sep = "_")
yearFE <- makeFEdummies(pb_long$year)
colnames(yearFE) <- paste("year", colnames(yearFE), sep = "_")
election_typeFE <- makeFEdummies(pb_long$election_type)
colnames(election_typeFE) <- paste("election", colnames(election_typeFE), sep = "_")
# pb_long <- pb_long %>% 
  # select(-Race, -year, -election_type)
pb_long <- bind_cols(pb_long, as.data.frame(raceFE), as.data.frame(yearFE), as.data.frame(election_typeFE))
  # bind_cols(raceFE, yearFE, election_typeFE)
logit_lme_f <- turned_out ~ pb + after_pb + as.factor(year) + election_type + (1 | VANID) 
lme_logit <- glmer(logit_lme_f, data = pb_long, family = binomial(), nAGQ = 0)    #start = list(fixef = bas_log$coefficients), 


logit_full_fm <- turned_out ~ pb + after_pb*Race + as.factor(year) + election_type + age + medhhinc + white +  (1 | VANID) + (1|NYCCD)
lme_full <-  glmer(logit_full_fm, data = pb_long, family = binomial(), nAGQ = 0)    #start = list(fixef = bas_log$coefficients), 
AIC(lme_full)
BIC(lme_full)
AIC(lme_logit)
BIC(lme_logit)

# AIC(lme_full)
# [1] 96998.3
# > BIC(lme_full)
# [1] 97194.14
# 
unconditioned <- glmer(turned_out ~ pb + after_pb + as.factor(year) + election_type + age + medhhinc + white +  (1 | VANID) + (1|NYCCD))
#predicted turnout
nd <- expand.grid(pb = 0, after_pb = c(0, 1), Race = c("U", "A", "H", "B", "W"), 
                  year = c(2016), election_type = "g", age = median(pb_long$age), medhhinc = median(pb_long$medhhinc), white = median(pb_long$white), stringsAsFactors = FALSE)
nd$preds <-  predict(lme_full, newdata = nd, type = "response", re.form = NA)


nd  %>%  
  mutate(after_pb = factor(after_pb, levels = c(0,1), labels = c("No PB", "After PB")),
         Race = factor(Race, levels = c("W", "B", "H", "A", "U"), labels = c("White", "Black", "Hispanic", "Asian", "Unknown"))) %>% 
ggplot(aes(y = preds, x = Race)) + 
  geom_segment(aes(xend = Race, y = 0, yend = preds, color = as.factor(after_pb)))+
  geom_point(aes(color = as.factor(after_pb))) +
  labs(title = "Predicted probability of voting in general election in 2016 \n before and after PB (in a non-PB district)", 
       x = "", y = "Predicted probability of Voting", color = "") +
  coord_flip() +
  theme_minimal()

######### 

logit_full_fm_mi <- turned_out ~ pb + after_pbmedhhinc + Race + as.factor(year) + election_type + age + white +  (1 | VANID) + (1|NYCCD)
lme_full_mi <-  glmer(logit_full_fm, data = pb_long, family = binomial(), nAGQ = 0)    #start = list(fixef = bas_log$coefficients), 
AIC(lme_full)
BIC(lme_full)
AIC(lme_logit)
BIC(lme_logit)

# AIC(lme_full)
# [1] 96998.3
# > BIC(lme_full)
# [1] 97194.14
# 
# unconditioned <- glmer(turned_out ~ pb + after_pb + as.factor(year) + election_type + age + medhhinc + white +  (1 | VANID) + (1|NYCCD))
#predicted turnout
nd_mi <- expand.grid(pb = 0, after_pb = c(0, 1), Race = "B",
                  year = c(2016), election_type = "g", age = median(pb_long$age), medhhinc = quantile(pb_long$medhhinc, probs = c(.1, .9)),
                   white = median(pb_long$white), stringsAsFactors = FALSE)
nd_mi$preds <-  predict(lme_full_mi, newdata = nd_mi, type = "response", re.form = NA)


nd_mi  %>%  
  mutate(after_pb = factor(after_pb, levels = c(0,1), labels = c("No PB", "After PB")),
         medhhinc = factor(medhhinc)) %>% 
  ggplot(aes(y = preds, x = medhhinc)) + 
  #geom_segment(aes(xend = medhhinc, y = 0, yend = preds, color = as.factor(after_pb)))+
  geom_point(aes(color = as.factor(after_pb))) +
  labs(title = "Predicted probability of voting in general election in 2016 \n before and after PB (in a non-PB district)", 
       x = "Median Census Tract HH Income", y = "Predicted probability of Voting", color = "") +
  coord_flip() +
  theme_minimal()


## poorer model fit. BUt huge effects from race (SE?).  Possibly because turnout is so much lower, and the logit kicks in more?
pb_long %>% mutate(turned_out = factor(turned_out, levels = c(0, 1), labels = c("Did not vote", "Voted")),
                  after_pb = factor(after_pb, levels = c(0,1), labels = c("No PB", "After PB"))) %>% 
  ggplot() + geom_bar(aes(x = as.factor(year),  fill = turned_out), position = "fill") + 
  facet_grid(Race~after_pb, scales = "free") +coord_flip() + scale_y_continuous(labels = scales::percent) +
  labs(y="", x="") +theme_minimal()


for (t in c("after_pb", "after_pb:RaceB", "after_pb:RaceH", "after_pb:RaceW", "after_pb:RaceA")){
mean(dydx(head(pb_long), lme_full, t))
}

newdat <-  expand.grid(pb = 0, after_pb = c(0, 1), year = c(2016), election_type = "g")
regressPredict <- function(df, form, newdat){
  newdat$pred <- glmer(form, data = df, family = binomial(), nAGQ = 0) %>% 
    predict(., newdata = newdat, type = "response", re.form = NA)
  return(newdat)
}

glmer_byRace <- pb_long %>% 
  group_by(Race) %>% 
  do(tidy(glmer(logit_lme_f, data = ., family = binomial(), nAGQ = 0)))

glmer_predbyRace <- pb_long %>% 
  group_by(Race) %>% 
  do(regressPredict(df = ., form = logit_lme_f, newdat = newdat))

ggplot(subset(glmer_byRace, term == "after_pb"), aes(y = estimate, x = Race)) + 
  geom_pointrange(aes(ymin = estimate - 1.96*std.error, ymax = estimate + 1.96*std.error)) +
  coord_flip() + theme_minimal()

tidy(lme_logit) %>% 
  mutate(Race = "All") %>% 
  bind_rows(glmer_byRace) %>% 
  filter(term == "after_pb") %>% 
  mutate(Race = factor(Race, levels = c("All", "W", "B", "H", "A", "U"), labels = c("All", "White", "Black", "Hispanic", "Asian", "Unknown"))) %>% 
  ggplot(aes(y = estimate, x = Race)) + 
  geom_pointrange(aes(ymin = estimate - 1.96*std.error, ymax = estimate + 1.96*std.error)) +
  coord_flip() + theme_minimal()


newdat %>% 
  mutate(pred = predict(lme_logit, newdata = ., type = "response", re.form = NA),
         Race = "All") %>%
  bind_rows(glmer_predbyRace) %>% 
  mutate(after_pb = factor(after_pb, levels = c(0,1), labels = c("No PB", "After PB")),
         Race = factor(Race, levels = c("All", "W", "B", "H", "A", "U"), labels = c("All", "White", "Black", "Hispanic", "Asian", "Unknown"))) %>% 
  ggplot(aes(y = pred, x = Race)) + 
  geom_point(aes(color = after_pb)) +
  labs(title = "Predicted probability of voting in general election in 2016 \nwith and without PB (in a non-PB district)", 
       x = "", y = "Predicted probability", color = "") +
  coord_flip() +
  theme_minimal()

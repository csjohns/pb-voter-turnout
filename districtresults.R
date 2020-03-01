library(tidyverse)

df <- read_csv("PB Elected Officials - Primary & General.csv") %>%
  select(`District/Ward`, `Election Year`, `Election Type`, `Date entered Office`,
         `Term Limit`, pctVotes, `Opponents pctVote`, `Total numVotes`) %>%
  rename(district = `District/Ward`,
         year = `Election Year`,
         election = `Election Type`, 
         enter_office = `Date entered Office`,
         term_limit = `Term Limit`, 
         dist_cand_votes = pctVotes,
         next_up = `Opponents pctVote`,
         dist_totvotes = `Total numVotes`) %>%
  mutate(vote_margin_pct_next = dist_cand_votes - next_up) %>%
  group_by(district, year) %>%
  mutate(main_election = vote_margin_pct_next == min(vote_margin_pct_next),
         incumbent = year != enter_office,
         competitive50 = vote_margin_pct_next < 0.5,
         competitive33 = vote_margin_pct_next < 0.334,
         competitivejenks = vote_margin_pct_next < 0.4141) %>%
  filter(main_election == TRUE)

write.csv(df, file = "districtresults.csv")

### Plots to help decide dichotomous "competitive" variable

p <- df %>%
  ggplot(aes(x = vote_margin_pct_next)) +
  geom_histogram() + 
  labs(x = "Vote share spread btw winner and runner-up")

ggsave(p, file = "histvotespread.png")

p <- df %>%
  ggplot(aes(vote_margin_pct_next, y = dist_totvotes)) +
  geom_point(color = "black") +
  geom_smooth() +
  facet_wrap(~factor(incumbent, labels = c("Not incumbent", "Incumbent"))) +
  labs(x = "Vote share spread btw winner and runner-up",
       y = "Total votes")
ggsave(p, file = "totalvotesbyshare.png")

p <- df %>%
  mutate(candvotes = dist_cand_votes * dist_totvotes,
         runnerup = next_up * dist_totvotes) %>%
  ggplot(aes(x = vote_margin_pct_next, y = runnerup)) +
  geom_point(color = "black") + 
  geom_smooth() +
  facet_wrap(~factor(incumbent, labels = c("Not incumbent", "Incumbent"))) +
  labs(x = "Vote share spread btw winner and runner-up",
       y = "Total votes for runner-up")

ggsave(p, file = "votesforrunnerup.png")

library(classInt)
classIntervals(df$vote_margin_pct_next, n = 2, style = "jenks")

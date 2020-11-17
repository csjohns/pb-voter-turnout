
max_race <- results %>% 
  mutate(ad = str_replace(ED, "AD", "Ad")) %>% 
  filter(ElectionMonth %in% c(4, 9, 11)) %>% 
  # mutate(VoteCount = as.integer(VoteCount)) %>% 
  group_by(ED, ElectionYear, ElectionMonth, ElectionDay, Office) %>%
  mutate(ed_totvotes = sum(VoteCount, na.rm = TRUE)) %>% 
group_by(ED, ElectionYear, ElectionMonth, ElectionDay) %>% # filter(ED == "Ad 23 - Ed 001") %>% View
  mutate(max_vote = as.numeric(ed_totvotes == max(ed_totvotes))) %>% 
  filter(max_vote == 1) %>% 
  mutate(Office = ordered(Office, levels = c("President", "Governor", "US Senate", "CD", "SD", "AD", "Member of the City Council", "Mayor"))) %>% 
  filter(Office == min(Office)) %>%
  mutate(Office = as.character(Office)) %>%
  select(-Party, -Candidate, -VoteCount) %>% 
  distinct() 
  
ggplot(max_race) +
  geom_bar(aes(x= Office)) +
  facet_grid(ElectionYear ~ ElectionMonth) + coord_flip() + 
  labs(y = "# EDs with this race as receiving most votes (in case of tie both represented)")
glimpse(results)

is_outlier <- function(x) {
  x < quantile(x, 0.25) - (1.5 * IQR(x)) | x > quantile(x, 0.75) + (1.5 * IQR(x))
}

ads <- max_race %>% 
  separate(ED, into = c("ad", "ed"), sep = " - ")  %>%
  group_by(ad, ElectionYear, ElectionMonth, Office) %>% 
  summarize(n = n(),
            adototvotes = sum(ed_totvotes)) %>% 
  group_by(ad, ElectionYear, ElectionMonth) %>% 
  mutate(toteds = sum(n),
         pcteds = n/toteds,
         ad_totvotes = sum(adototvotes)) %>% 
  group_by(ElectionYear, ElectionMonth, Office) %>% 
  mutate(outlierlab = ifelse(is_outlier(pcteds), str_remove(ad, "Ad "), NA)) 

pl <- ads %>% 
  ggplot(aes(x = Office, y = pcteds)) +
  # geom_jitter(aes(x= Office, y = pcteds, color = ad), width = .5, height = 0, alpha = .5) +
  # geom_density(aes(x = pcteds, color = Office))+
  geom_boxplot() +
  geom_text(aes(label = outlierlab, color = ad_totvotes), vjust = -1)+
  facet_grid(ElectionYear ~ ElectionMonth, scales = "free_y") + 
  # stat_summary(aes(label = "pp"), geom = "text",
  #       fun.y = function(y) {o <- boxplot.stats(y)$out; if(length(o) == 0) NA else o},
  #       hjust = -1) +
  coord_flip() +
  labs(y = "% EDs in each AD showing each race as receiving most votes (in case of tie, 'higher' ballot line kept)") +
  scale_color_viridis_c() + theme_bw()
  theme(legend.position = "none")
pl
library(plotly)
ggplotly(pl)

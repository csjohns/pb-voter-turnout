## covariate exploration 
#load pb from somewhere else 

library(ggplot2)

# binary  by continuous
ggplot(pb_long, aes(x = age, y = turned_out)) +
  geom_hline(aes(yintercept = mean(pb_long$turned_out)), color = "blue", alpha = .3) +
  geom_point(alpha = .4) +
  geom_smooth(colour="blue", size=1.5)+
  theme_bw()

ggplot(pb_long, aes(x = medhhinc, y = turned_out)) +
  geom_hline(aes(yintercept = mean(pb_long$turned_out)), color = "blue", alpha = .3) +
  geom_point(alpha = .4) +
  geom_smooth(colour="blue", size=1.5)+
  theme_bw()


ggplot(pb_long, aes(x = college, y = turned_out)) +
  geom_hline(aes(yintercept = mean(pb_long$turned_out)), color = "blue", alpha = .3) +
  geom_point(alpha = .4) +
  geom_smooth(colour="blue", size=1.5)+
  theme_bw()

ggplot(pb_long, aes(x = high_school, y = turned_out)) +
  geom_hline(aes(yintercept = mean(pb_long$turned_out)), color = "blue", alpha = .3) +
  geom_point(alpha = .4) +
  geom_smooth(colour="blue", size=1.5)+
  theme_bw()

ggplot(pb_long, aes(x = high_school, y = turned_out)) +
  geom_hline(aes(yintercept = mean(pb_long$turned_out)), color = "blue", alpha = .3) +
  geom_point(alpha = .4) +
  geom_smooth(colour="blue", size=1.5)+
  theme_bw()

ggplot(pb_long, aes(x = white, y = turned_out)) +
  geom_hline(aes(yintercept = mean(pb_long$turned_out)), color = "blue", alpha = .3) +
  geom_point(alpha = .4) +
  geom_smooth(colour="blue", size=1.5)+
  theme_bw()

# categorical by categorical
ggplot(pb_long, aes(x = race, fill = as.factor(turned_out)))+
  geom_bar(position = "fill")+
  theme_minimal()

ggplot(pb_long, aes(x = Sex, fill = as.factor(turned_out)))+
  geom_bar(position = "fill")+
  theme_minimal()

ggplot(pb_long, aes(x = majmatch, fill = as.factor(turned_out)))+
  geom_bar(position = "fill")+
  theme_minimal()

ggplot(pb_long, aes(x = as.factor(year), fill = as.factor(turned_out)))+
  geom_bar(position = "fill")+
  theme_minimal()

ggplot(pb_long, aes(x = election_type, fill = as.factor(turned_out)))+
  geom_bar(position = "fill")+
  theme_minimal()

## this is doing some very basic plots exploring distribution of voting across subsets

ggplot(pb_long) + geom_bar(aes(x = as.factor(turned_out), fill = election_type), stat = "count", position = "dodge") + facet_wrap(~year)
ggplot(pb_long) + geom_bar(aes(x = as.factor(turned_out), fill = as.factor(after_pb)), position = "dodge") + facet_wrap(~year)

pb_long %>% mutate(turned_out = factor(turned_out, levels = c(0, 1), labels = c("Did not vote", "Voted")),
                   after_pb = factor(after_pb, levels = c(0,1), labels = c("No PB", "After PB"))) %>% 
  filter(election_type == "g" & year == 2008)  %>% 
  ggplot() + geom_bar(aes(x = as.factor(year),  fill = turned_out), position = "fill") + 
  facet_grid(Race~after_pb*pb, scales = "free") +coord_flip() + scale_y_continuous(labels = scales::percent) +
  labs(y="", x="") +theme_minimal() + labs(title = "Turnout in 2008 general election")


pb_long %>% mutate(turned_out = factor(turned_out, levels = c(0, 1), labels = c("Did not vote", "Voted")),
                   after_pb = factor(after_pb, levels = c(0,1), labels = c("No PB", "After PB"))) %>% 
  filter(election_type == "g" & year == 2016)  %>% 
  ggplot() + geom_bar(aes(x = as.factor(year),  fill = turned_out), position = "fill") + 
  facet_grid(Race~after_pb*pb, scales = "free") +coord_flip() + scale_y_continuous(labels = scales::percent) +
  labs(y="", x="") +theme_minimal() + labs(title = "Turnout in 2016 general election")

pb_long %>% mutate(turned_out = factor(turned_out, levels = c(0, 1), labels = c("Did not vote", "Voted")),
                   after_pb = factor(after_pb, levels = c(0,1), labels = c("No PB", "After PB"))) %>% 
  filter(election_type == "g" & year == 2012)  %>% 
  ggplot() + geom_bar(aes(x = as.factor(year),  fill = turned_out), position = "fill") + 
  facet_grid(Race~after_pb*pb, scales = "free") +coord_flip() + scale_y_continuous(labels = scales::percent) +
  labs(y="", x="") +theme_minimal() + labs(title = "Turnout in 2012 general election")



p <- pb_long %>% mutate(turned_out = factor(turned_out, levels = c(0, 1), labels = c("Did not vote", "Voted")),
                        after_pb = factor(after_pb, levels = c(0,1), labels = c("No PB", "After PB"))) %>% 
  filter(election_type == "g")  %>% 
  ggplot() + geom_bar(aes(x = as.factor(year),  fill = turned_out), position = "fill") + 
  facet_grid(Race~after_pb*pb, scales = "free") 
ggplotly(p)

pb_long %>%
  group_by(year, election_type,race, after_pb, pb) %>% 
  summarize(nvoters = n(),
            turnout = sum(turned_out, na.rm = T)/n()) %>% 
  filter(year == 2016, election_type == "g")



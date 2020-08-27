library(dplyr)
library(tidyr)
library(ggplot2)

within_reg <- readRDS("data/cleaned_R_results/iter_regress_lmers_within_dist.rds") %>% 
  filter(match_type == "Tract super") %>% 
  mutate(comparison = "Within PB district")

placebo_reg <- readRDS("data/cleaned_R_results/iter_regress_lmers_placebo.rds") %>% 
  filter(match_type == "Tract super") %>% 
  mutate(comparison = "Placebo (non-PB to non-PB)")

standard_reg <- readRDS("data/cleaned_R_results/iter_regress_lmers.rds") %>% 
  filter(match_type == "Tract super") %>% 
  mutate(comparison = "Primary method")

all_regs <- bind_rows(standard_reg, placebo_reg, within_reg)

all_regs %>% 
  ungroup() %>% 
  filter(group == "fixed" & term == "after_pb") %>% 
  mutate(model_label = model_name,
         model_label = ifelse(comparison != "Primary method", NA, model_label),
         model_name = factor(model_name, 
                             levels = c("lme_comp", "logit_minimal_form"), 
                             labels = c("Full model", "Minimal model")),
         model_label = factor(model_label ,
                             levels = c("lme_comp", "logit_minimal_form"), 
                             labels = c("Full model", "Minimal model"))) %>% 
  ggplot(aes(x = forcats::fct_reorder(comparison, estimate, .desc = F),  y = estimate, ymin = conf.low, ymax = conf.high, 
             color = model_name, shape = model_name)) +
  geom_pointrange(position = position_dodge(width = .5)) +
  geom_point(position = position_dodge(width =.5 )) +
  geom_hline(aes(yintercept = 0)) +
  geom_text(aes(y = 0.55, label = model_label), hjust = 1, position = position_dodge(width = .5), size = 3) +
  # ylim(-5, 5) +
  labs(x = "", color = "", y = "PB effect coefficient estimate") +#, 
       # title= "Estimated effect of PB within primary match methodology \nand with alternative comparison frameworks") + 
  # guides(color =  guide_legend(reverse=F)) +
  coord_flip() +
  theme_minimal() + 
  theme(legend.position = "none")

ggsave("Paper_text/Figs/robust_compare.pdf", width = 5, height = 2.5)

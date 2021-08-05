# Load libraries --------------------------------------------------------------

# Libs, helpers, data
source(here::here("scripts", "03_load_data.R"))


# Meta-analysis model

l1_l2_brm <- readRDS(here("models", "l1_l2_meta_analysis_mod_0.rds"))
l1_l3_brm <- readRDS(here("models", "l1_l3_meta_analysis_mod_0.rds"))
l2_l3_brm <- readRDS(here("models", "l2_l3_meta_analysis_mod_0.rds"))

l1_l2df = l1_l2_brm %>% 
  as.data.frame() %>% 
  dplyr::select(b_Intercept) %>% 
  mutate(parameter = "l1_l2") 

l1_l3df = l1_l3_brm %>% 
  as.data.frame() %>% 
  dplyr::select(b_Intercept) %>% 
  mutate(parameter = "l1_l3") 

l2_l3df = l2_l3_brm %>% 
  as.data.frame() %>% 
  dplyr::select(b_Intercept) %>% 
  mutate(parameter = "l2_l3") 

all_df = rbind(l1_l2df, l1_l3df, l2_l3df)

all_df %>%
  ggplot(., aes(x = b_Intercept, y = parameter)) + 
  geom_vline(xintercept = 0, lty = 3) +
  tidybayes::stat_halfeye(pch = 21, point_fill = "white", point_size = 3, 
                          .width = c(0.66, 0.95)) + 
  labs(title = "Meta-analysis models per language pairing", 
       subtitle = "Forest plot of population estimates", 
       caption = "Posterior means +/- 66% and 95% CI", 
       y = "Language Pairing", x = "Estimate") + ggsave(here("plots", "plot.png"))

#_____________________


# Meta analysis posterior studies ----------------------------------------------
# adapted from Casillas (2021)

# plot 1 -----------------------------------------------------------------------

# Get draws for each study
study_draws <- spread_draws(l2_l3_brm, r_study[Study,], b_Intercept) %>% 
  mutate(b_Intercept = r_study + b_Intercept)

# Get draws for pooled effect
pooled_effect_draws <- spread_draws(l2_l3_brm, b_Intercept) %>% 
  mutate(Study = "Pooled Effect")

# Combine it and clean up
forest_data <- bind_rows(study_draws, pooled_effect_draws) %>% 
  ungroup() %>% 
           mutate(Study = str_replace_all(Study, "[.]", " ")) %>% 
  mutate(Study = reorder(Study, b_Intercept), 
         Study = relevel(Study, "Pooled Effect", after = Inf)) 
                  
# Calculate mean qi intervals for right margin text
forest_data_summary <- group_by(forest_data, Study) %>% 
  mean_qi(b_Intercept, .width = 0.95) 

# Calculate mean qi intervals for pooled effect
pooled_summary <- group_by(forest_data, Study) %>% 
  mean_qi(b_Intercept, .width = c(0.5, 0.8, 0.95)) %>% 
  filter(Study == "Pooled Effect")

# Plot it all
p_post_studies <- forest_data %>% 
  ggplot() + 
  aes(x = b_Intercept, y = Study) + 
  geom_text(data = mutate_if(forest_data_summary, is.numeric, round, 2),
            aes(label = glue("{b_Intercept} [{.lower}, {.upper}]"), x = Inf), 
            hjust = "inward", family = "Times") + 
  geom_tile(data = pooled_summary, aes(width = .lower - .upper),
            alpha = 0.2, height = Inf, fill = "#31688EFF") +
  stat_pointinterval(point_fill = "white", shape = 21, .width = c(0.8, 0.95)) +
  coord_cartesian(xlim = c(-2, 2)) + 
  labs(x = expression(italic("SMD")), y = NULL) +
  theme(axis.text.y = element_text(hjust = 0)) + ggtitle("L2-L3 effect sizes by study") +
  ggsave(here("plots", "l2_l3.png"))

# plot 2 -----------------------------------------------------------------------

# Get draws for each study
study_draws2 <- spread_draws(l1_l3_brm, r_study[Study,], b_Intercept) %>% 
  mutate(b_Intercept = r_study + b_Intercept)

# Get draws for pooled effect
pooled_effect_draws2 <- spread_draws(l1_l3_brm, b_Intercept) %>% 
  mutate(Study = "Pooled Effect")

# Combine it and clean up
forest_data2 <- bind_rows(study_draws2, pooled_effect_draws2) %>% 
  ungroup() %>% 
  mutate(Study = reorder(Study, b_Intercept), 
         Study = relevel(Study, "Pooled Effect", after = Inf))

# Calculate mean qi intervals for right margin text
forest_data_summary2 <- group_by(forest_data2, Study) %>% 
  mean_qi(b_Intercept, .width = 0.95) 

# Calculate mean qi intervals for pooled effect
pooled_summary2 <- group_by(forest_data2, Study) %>% 
  mean_qi(b_Intercept, .width = c(0.5, 0.8, 0.95)) %>% 
  filter(Study == "Pooled Effect")

# Plot it all
p_post_studies <- forest_data2 %>% 
  ggplot() + 
  aes(x = b_Intercept, y = Study) + 
  geom_text(data = mutate_if(forest_data_summary2, is.numeric, round, 2),
            aes(label = glue("{b_Intercept} [{.lower}, {.upper}]"), x = Inf), 
            hjust = "inward", family = "Times") + 
  geom_tile(data = pooled_summary, aes(width = .lower - .upper),
            alpha = 0.2, height = Inf, fill = "#31688EFF") +
  stat_pointinterval(point_fill = "white", shape = 21, .width = c(0.8, 0.95)) +
  coord_cartesian(xlim = c(-2, 2)) + 
  labs(x = expression(italic("SMD")), y = NULL) +
  minimal_adj() + 
  theme(axis.text.y = element_text(hjust = 0)) + ggtitle("L1-L3 effect sizes by study") +
  ggsave(here("plots", "l1_l3.png"))

# plot 3 -----------------------------------------------------------------------

# Get draws for each study
study_draws3 <- spread_draws(l1_l2_brm, r_study[Study,], b_Intercept) %>% 
  mutate(b_Intercept = r_study + b_Intercept)

# Get draws for pooled effect
pooled_effect_draws3 <- spread_draws(l1_l2_brm, b_Intercept) %>% 
  mutate(Study = "Pooled Effect")

# Combine it and clean up
forest_data3 <- bind_rows(study_draws3, pooled_effect_draws3) %>% 
  ungroup() %>% 
  mutate(Study = reorder(Study, b_Intercept), 
         Study = relevel(Study, "Pooled Effect", after = Inf))

# Calculate mean qi intervals for right margin text
forest_data_summary3 <- group_by(forest_data3, Study) %>% 
  mean_qi(b_Intercept, .width = 0.95) 

# Calculate mean qi intervals for pooled effect
pooled_summary3 <- group_by(forest_data3, Study) %>% 
  mean_qi(b_Intercept, .width = c(0.5, 0.8, 0.95)) %>% 
  filter(Study == "Pooled Effect")

# Plot it all
p_post_studies <- forest_data3 %>% 
  ggplot() + 
  aes(x = b_Intercept, y = Study) + 
  geom_text(data = mutate_if(forest_data_summary3, is.numeric, round, 2),
            aes(label = glue("{b_Intercept} [{.lower}, {.upper}]"), x = Inf), 
            hjust = "inward", family = "Times") + 
  geom_tile(data = pooled_summary, aes(width = .lower - .upper),
            alpha = 0.2, height = Inf, fill = "#31688EFF") +
  stat_pointinterval(point_fill = "white", shape = 21, .width = c(0.8, 0.95)) +
  coord_cartesian(xlim = c(-2, 2)) + 
  labs(x = expression(italic("SMD")), y = NULL) +
  minimal_adj() + 
  theme(axis.text.y = element_text(hjust = 0)) + ggtitle("L1-L2 effect sizes by study") +
  ggsave(here("plots", "l1_l2.png"))

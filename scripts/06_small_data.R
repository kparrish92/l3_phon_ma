
# Libs, helpers, data
source(here::here("scripts", "03_load_data.R"))

# Meta-analysis model

l1_l2_brm <- readRDS(here("models", "l1_l2_meta_analysis_mod_0.rds"))
l1_l3_brm <- readRDS(here("models", "l1_l3_meta_analysis_mod_0.rds"))
l2_l3_brm <- readRDS(here("models", "l2_l3_meta_analysis_mod_0.rds"))



# what is the probability that the average effect size is greater than 0.4?

# l1 l2 

avg_es1 <- as.data.frame(l1_l2_brm, pars = "b_")[, 1]
prob_gt_41 <- ((sum(avg_es1 < -0.4) / length(avg_es1))) %>% round(., 2)

# l2 l3
avg_es2 <- as.data.frame(l2_l3_brm, pars = "b_")[, 1]
prob_gt_42 <- ((sum(avg_es2 < -0.4) / length(avg_es2))) %>% round(., 2)

# l1 l3 
avg_es3 <- as.data.frame(l1_l3_brm, pars = "b_")[, 1]
prob_gt_43 <- ((sum(avg_es3 < -0.4) / length(avg_es3))) %>% round(., 2)

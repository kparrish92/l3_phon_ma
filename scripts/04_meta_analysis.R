# Source everything -----------------------------------------------------------

source(here::here("scripts", "03_load_data.R"))

# -----------------------------------------------------------------------------

# transform data to give absolute values of es - not interested in directionality 
# due to the variability in languages present 

ma_data$es_l1_l2 = abs(ma_data$es_l1_l2)
ma_data$es_l1_l3 = abs(ma_data$es_l1_l3)
ma_data$es_l2_l3 = abs(ma_data$es_l2_l3)

# show mean effect sizes 

mean(ma_data$es_l1_l2)
mean(ma_data$es_l1_l3)
mean(ma_data$es_l2_l3)

ma_data_long <- ma_data %>% 
  pivot_longer(c(`es_l1_l2`, `es_l1_l3`, `es_l2_l3`), names_to = "comparison", 
               values_to = "es")

# desc data 
## mean effect sizes 

ma_data_long %>% 
  group_by(comparison) %>% 
  summarise(mean_es = mean(es), sd_es = sd(es))

# lm 

l1_l2_model = lmer(es_l1_l2 ~ se_l1_l2 + (1 | study), data = ma_data)
l1_l3_model = lmer(es_l1_l3 ~ se_l1_l3 + (1 | study), data = ma_data)
l2_l3_model = lmer(es_l2_l3 ~ se_l2_l3 + (1 | study), data = ma_data)

fixef(l1_l2_model)
fixef(l1_l3_model)
fixef(l2_l3_model)


# Set priors
priors <- c(prior(normal(0, 1), class = Intercept),
            prior(cauchy(0, 1), class = sd))

df = ma_data %>% 
  rename(es = es_l1_l2) %>% 
  rename(se = se_l1_l2)

df2 = ma_data %>% 
  rename(es = es_l2_l3) %>% 
  rename(se = se_l2_l3)

df3 = ma_data %>% 
  rename(es = es_l1_l3) %>% 
  rename(se = se_l1_l3)

m0_brm <- brm(
  formula = es | se(se) ~ 1 + (1 | study) + (1 | consonant), 
  family = gaussian, 
  data = df,
  prior = priors,
  iter = 4000, warmup = 2000,  cores = 4, chains = 4, 
  file = here::here("models", "l1_l2_meta_analysis_mod_0"))

# -.1 l1_l2

m1_brm <- brm(
  formula = es | se(se) ~ 1 + (1 | study) + (1 | consonant), 
  family = gaussian, 
  data = df2,
  prior = priors,
  iter = 4000, warmup = 2000,  cores = 4, chains = 4, 
  file = here::here("models", "l2_l3_meta_analysis_mod_0"))

m2_brm <- brm(
  formula = es | se(se) ~ 1 + (1 | study) + (1 | consonant), 
  family = gaussian, 
  data = df3,
  prior = priors,
  iter = 4000, warmup = 2000,  cores = 4, chains = 4, 
  file = here::here("models", "l1_l3_meta_analysis_mod_0"))



fixef(m0_brm)
fixef(m1_brm)
fixef(m2_brm)
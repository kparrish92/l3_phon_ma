# ------------------------------------------------------------------------------
# 01_Literature review 
# Coding the means and sds of each 
# ------------------------------------------------------------------------------
#
# Source helpers and libs -----------------------------------------------------

source(here::here("scripts", "01_helpers.R"))

# -----------------------------------------------------------------------------

#
# ------------------------------------------------------------------------------
# Llama & Cordoso (2018)
# VOT production of /ptk/ by mirror image groups of English/French & L3 Spanish
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
study1 = meta_l3_within(n = 15, 
               l1m = 60.56, l2m = 46.48, l3m = 31.8, 
               l1sd = 16.96, l2sd = 21.58, l3sd = 15.62) %>% 
  mutate(study = "Llama and Cordoso 2018_p_grp1", g1l1 = "english", g1l2 = "french", g1l3 = "spanish",
         analysis = "Kruskal-Wallis test", consonant = "p")
# ------------------------------------------------------------------------------
study2 = meta_l3_within(n = 15, 
                        l1m = 27.8, l2m = 50.28, l3m = 18.54, 
                        l1sd = 8.35, l2sd = 12.79, l3sd = 5.42) %>% 
  mutate(study = "Llama and Cordoso 2018_p_grp2", g1l1 = "french", g1l2 = "english", g1l3 = "spanish",
         analysis = "Kruskal-Wallis test", consonant = "p")
# ------------------------------------------------------------------------------
study3 = meta_l3_within(n = 15, 
                        l1m = 70.27, l2m = 53.43, l3m = 36.36, 
                        l1sd = 14.69, l2sd = 19.42, l3sd = 15.52) %>% 
  mutate(study = "Llama and Cordoso 2018_t_grp1", g1l1 = "english", g1l2 = "french", g1l3 = "spanish",
         analysis = "Kruskal-Wallis test", consonant = "t")
# ------------------------------------------------------------------------------
study4 = meta_l3_within(n = 15, 
                        l1m = 39.11, l2m = 62.91, l3m = 25.16, 
                        l1sd = 9.22, l2sd = 13.19, l3sd = 7.42) %>% 
  mutate(study = "Llama and Cordoso 2018_t_grp2", g1l1 = "french", g1l2 = "english", g1l3 = "spanish",
         analysis = "Kruskal-Wallis test", consonant = "t")
# ------------------------------------------------------------------------------
study5 = meta_l3_within(n = 15, 
                        l1m = 74.3, l2m = 61.7, l3m = 51.2, 
                        l1sd = 12.49, l2sd = 17.92, l3sd = 14.81) %>% 
  mutate(study = "Llama and Cordoso 2018_k_grp1", g1l1 = "english", g1l2 = "french", g1l3 = "spanish",
         analysis = "Kruskal-Wallis test", consonant = "k")
# ------------------------------------------------------------------------------
study6 = meta_l3_within(n = 15, 
                        l1m = 54.6, l2m = 69.93, l3m = 42.21, 
                        l1sd = 11.93, l2sd = 11.07, l3sd = 9.77) %>% 
  mutate(study = "Llama and Cordoso 2018_k_grp2", g1l1 = "french", g1l2 = "english", g1l3 = "spanish",
         analysis = "Kruskal-Wallis test", consonant = "k")


# ------------------------------------------------------------------------------
# Wrembel(2011)
# vot production by 2 groups of Polish L1 English L2 French L3
# two groups of the same profile, intermediate and advanced 
# ------------------------------------------------------------------------------
study7 = meta_l3_within(n = 16, 
                        l1m = 37, l2m = 68, l3m = 53, 
                        l1sd = 18, l2sd = 19, l3sd = 21) %>% 
  mutate(study = "Wrembel 2011a", g1l1 = "polish", g1l2 = "english", g1l3 = "french",
         analysis = "t.test", consonant = "ptk")
# ------------------------------------------------------------------------------
study8 = meta_l3_within(n = 16, 
                        l1m = 40, l2m = 72, l3m = 56, 
                        l1sd = 20, l2sd = 20, l3sd = 23) %>% 
  mutate(study = "Wrembel 2011b", g1l1 = "polish", g1l2 = "english", g1l3 = "french",
         analysis = "t.test", consonant = "ptk")
# ------------------------------------------------------------------------------
# Wrembel (2015) - p, groups a and b 
# ------------------------------------------------------------------------------
study9 = meta_l3_within(n = 38, 
                        l1m = 22.8, l2m = 65, l3m = 39.5, 
                        l1sd = 11.5, l2sd = 28.1, l3sd = 22.2) %>% 
  mutate(study = "Wrembel 2015_p_grp1", g1l1 = "polish", g1l2 = "english", g1l3 = "french",
         analysis = "", consonant = "p")
# ------------------------------------------------------------------------------
study10 = meta_l3_within(n = 26, 
                        l1m = 28.2, l2m = 57.5, l3m = 41.9, 
                        l1sd = 13.6, l2sd = 30.4, l3sd = 20.8) %>% 
  mutate(study = "Wrembel 2015_p_grp2", g1l1 = "polish", g1l2 = "english", g1l3 = "german",
         analysis = "", consonant = "p")
# ------------------------------------------------------------------------------
study11 = meta_l3_within(n = 38, 
                        l1m = 29.8, l2m = 74.2, l3m = 42.3, 
                        l1sd = 13.9, l2sd = 27.8, l3sd = 20.2) %>% 
  mutate(study = "Wrembel 2015_t_grp1", g1l1 = "polish", g1l2 = "english", g1l3 = "french",
         analysis = "", consonant = "t")
# ------------------------------------------------------------------------------
study12 = meta_l3_within(n = 26, 
                         l1m = 32.9, l2m = 73.9, l3m = 47.9, 
                         l1sd = 13.5, l2sd = 28.8, l3sd = 22.7) %>% 
  mutate(study = "Wrembel 2015_t_grp2", g1l1 = "polish", g1l2 = "english", g1l3 = "german",
         analysis = "", consonant = "t")
# ------------------------------------------------------------------------------
study13 = meta_l3_within(n = 38, 
                         l1m = 50.4, l2m = 90.1, l3m = 67.2, 
                         l1sd = 14.8, l2sd = 22, l3sd = 21.8) %>% 
  mutate(study = "Wrembel 2015_k_grp1", g1l1 = "polish", g1l2 = "english", g1l3 = "french",
         analysis = "", consonant = "k")
# ------------------------------------------------------------------------------
study14 = meta_l3_within(n = 26, 
                         l1m = 56, l2m = 89.2, l3m = 70, 
                         l1sd = 14.2, l2sd = 25.4, l3sd = 19.4) %>% 
  mutate(study = "Wrembel 2015_k_grp2", g1l1 = "polish", g1l2 = "english", g1l3 = "german",
         analysis = "", consonant = "k")
# ------------------------------------------------------------------------------
study15 = meta_l3_within(n = 33, 
                         l1m = 23.3, l2m = 38.3, l3m = 33.9, 
                         l1sd = 11.1, l2sd = 17.2, l3sd = 16.6) %>% 
  mutate(study = "Wrembel 2015_p_grp3", g1l1 = "polish", g1l2 = "german", g1l3 = "english",
         analysis = "", consonant = "p")
# ------------------------------------------------------------------------------
study16 = meta_l3_within(n = 28, 
                         l1m = 22.9, l2m = 25.7, l3m = 32.3, 
                         l1sd = 11.5, l2sd = 16.5, l3sd = 19.2) %>% 
  mutate(study = "Wrembel 2015_p_grp4", g1l1 = "polish", g1l2 = "french", g1l3 = "english",
         analysis = "", consonant = "p")
# ------------------------------------------------------------------------------
study17 = meta_l3_within(n = 33, 
                         l1m = 31.3, l2m = 38, l3m = 40.9, 
                         l1sd = 14.8, l2sd = 18.7, l3sd = 19.4) %>% 
  mutate(study = "Wrembel 2015_t_grp3", g1l1 = "polish", g1l2 = "german", g1l3 = "english",
         analysis = "", consonant = "t")
# ------------------------------------------------------------------------------
study18 = meta_l3_within(n = 28, 
                         l1m = 29.4, l2m = 34.6, l3m = 40.8, 
                         l1sd = 13.9, l2sd = 17.3, l3sd = 22.3) %>% 
  mutate(study = "Wrembel 2015_t_grp4", g1l1 = "polish", g1l2 = "french", g1l3 = "english",
         analysis = "", consonant = "t")
# ------------------------------------------------------------------------------
study19 = meta_l3_within(n = 33, 
                         l1m = 52.2, l2m = 68.2, l3m = 65.3, 
                         l1sd = 14.1, l2sd = 17.6, l3sd = 20.8) %>% 
  mutate(study = "Wrembel 2015_k_grp3", g1l1 = "polish", g1l2 = "german", g1l3 = "english",
         analysis = "", consonant = "k")
# ------------------------------------------------------------------------------
study20 = meta_l3_within(n = 28, 
                         l1m = 48.8, l2m = 52.4, l3m = 65.4, 
                         l1sd = 13.8, l2sd = 20.4, l3sd = 26.3) %>% 
  mutate(study = "Wrembel 2015_k_grp4", g1l1 = "polish", g1l2 = "french", g1l3 = "english",
         analysis = "", consonant = "k")
# ------------------------------------------------------------------------------
    
all_studies <- rbind(study1, study2, study3, study4, study5, 
                     study6, study7, study8, study9, study10, 
                     study11, study12, study13, study14, study15,
                     study16, study17, study18, study19, study20)

all_studies %>% 
  write.csv(here("data", "tidy", "all_studies.csv"))


###### studies that I can't find 
### Llama Cordoso and Collins (2010)
### Wunder (2010)
### Tremblay (2010)
### Sypiańska (2013)
### Dominika Skrzypek & Elisabeth Zetterholm - IAM11 # Realisation of unvoiced plosives in L3 speakers of Swedish

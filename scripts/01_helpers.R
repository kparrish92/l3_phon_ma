# Helpers ---------------------------------------------------------------------

source(here::here("scripts", "00_libs.R"))

# -----------------------------------------------------------------------------

# function to take reported means and sds for each language and calculate a within-subjects
# effect size (es) between l1 and l2, l2 and l3, and l1 and l3, and the standard error (se)
# of each es for meta-analysis. 

# The formula for se of the es was originally reported in Wilson (2001) - Practical meta-analysis.

# -----------------------------------------------------------------------------

# values for testing that the function works 
n = 10
l1m = 15
l2m = 40
l3m = 38
l1sd = 8
l2sd = 20
l3sd = 19

# -----------------------------------------------------------------------------

meta_l3_within = function(n, l1m, l2m, l3m, l1sd, l2sd, l3sd)
{
# es 
es_l1_l2 <- (l1m - l2m)/(l1sd+l2sd)/2
es_l2_l3 <- (l2m - l3m)/(l2sd+l3sd)/2
es_l1_l3 <- (l1m - l3m)/(l1sd+l3sd)/2
# se
sel1_l2 <- (2*(1-.5))/n + es_l1_l2/2*n 
if(sel1_l2 < 0) {
  sel1_l2 = abs(sel1_l2)
}
sel2_l3 <- (2*(1-.5))/n + es_l2_l3/2*n 
if(sel2_l3 < 0) {
  sel2_l3 = abs(sel2_l3)
}
sel1_l3 <- (2*(1-.5))/n + es_l1_l3/2*n 
if(sel1_l3 < 0) {
  sel1_l3 = abs(sel1_l3)
}
se_l1_l2 = sqrt(sel1_l2)
se_l2_l3 = sqrt(sel2_l3)
se_l1_l3 = sqrt(sel1_l3)

return(tibble(es_l1_l2, es_l2_l3, es_l1_l3, se_l1_l2, se_l2_l3, se_l1_l3))
}

# testing the function 
meta_l3_within(n, l1m, l2m, l3m, l1sd, l2sd, l3sd)


# plotting function (Casillas, 2021)
minimal_adj <- function(...) {
  list(
    theme_minimal(base_size = 12, base_family = "Times"), 
    theme(
      axis.title.y = element_text(size = rel(.9), hjust = 0.95),
      panel.grid.major = element_line(colour = 'grey90', size = 0.15),
      panel.grid.minor = element_line(colour = 'grey90', size = 0.15))
  )
}


# Report estimate from posterior distribution summary (Casillas, 2021)

report_posterior <- function(df, param, is_meta = FALSE) {
  
  if (is_meta == FALSE) {
    
    # Extract wanted value from model output
    est  <- df[df$Parameter == param, "Median"]
    cis  <- df[df$Parameter == param, "95% HDI"]
    rope <- df[df$Parameter == param, "ROPE_Percentage"]
    mpe  <- df[df$Parameter == param, "pd"]
    
    capture.output(
      paste0("(&beta; = ", est, ", HDI = ", cis, ", ROPE = ", rope, 
             ", MPE = ", mpe, ")", "\n") %>% 
        cat()) %>% 
      paste()
  } else {
    # Extract wanted value from model output
    est  <- df[df$Parameter == param, "Median"]
    cis  <- df[df$Parameter == param, "95% HDI"]
    
    capture.output(
      paste0("(&beta; = ", est, ", HDI = ", cis, ")", "\n") %>% 
        cat()) %>% 
      paste()
  }
}

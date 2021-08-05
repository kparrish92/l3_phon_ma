# Helpers ---------------------------------------------------------------------

source(here::here("scripts", "00_libs.R"))

# -----------------------------------------------------------------------------
# for testing that the function works 
n = 10
l1m = 15
l2m = 40
l3m = 38
l1sd = 8
l2sd = 20
l3sd = 19

# power function within subject 

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

meta_l3_within(n, l1m, l2m, l3m, l1sd, l2sd, l3sd)

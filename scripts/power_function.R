################################################################################
# Post-hoc power analysis - independent samples
# 
# given mean and sd of two samples run a power analysis - then repeat X times
#
################################################################################


k = 1000 # number of iterations per power analysis
n = 10 # observed sample size
y = 100 # number of times PA is repeated  
n = 14 # n of grp
m1 = 25 
m2 = 20
sd1 = 5
sd2 = 5

# write function
power <- function(n, m1, m2, sd1, sd2, k){
  
result = matrix(nrow = k)

  for(thisRun in 1:k){
    mv1 = rnorm(n = n, m = m1, sd = sd1)
    mv2 = rnorm(n = n, m = m2, sd = sd2)
    mrT = t.test(mv1,mv2)
    result[thisRun] = mrT$p.value
}

return(100*sum(result < .05)/k)
}

power(n, m1, m2, sd1, sd2, k)

####### run 10 power analyses - see the results?
# sampling error within sampling error?


#hundred_pa = matrix(nrow = y)
  #for(thisRun2 in 1:y){
   # hundred_pa[thisRun2] = power(n, m1, m2, sd1, sd2, k)
#}

#view(hundred_pa)
#hist(hundred_pa)

# summary(hundred_pa)
#------------------------
#-----------------------

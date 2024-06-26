################################################################################
# Dependencies
################################################################################
source('elo_functions.R')

################################################################################
# Simulation
################################################################################
L=c(1,2,5,10,20,50,100,200,500)
res_elo_lag=list()
for(j in 1:length(L)){
  res_elo_lag[[j]]=elo_lag(n=1000,
                           m=200,
                           reps=200,
                           K=0.3,
                           games=1000+L[j],
                           LH=L[j],
                           mP=log(0.7/0.3),m_th=log(0.7/0.3))
}
# For the paper n=1000, m=200, and I think that reps=100 should be sufficient 
#here to get smooth results

################################################################################
# saving the result as a list
################################################################################
save(res_elo_lag, file = "output/ResultsEloLag.RData")
################################################################################
# Dependencies
################################################################################
source('elo_functions.R')
load("output/ResultsDoubleEloBaseline.RData",
     "output/ResultsDoubleElo_K.RData",
     "output/ResultsDoubleElo_mu.RData")

#res_double = 
################################################################################
# Simulation
################################################################################
K=c(0.3,0.1,0.2,0.4,0.5,0.3,0.3,0.3,0.3)
mP=c(rep(log(0.7/0.3),5),0,log(0.6/0.4),log(0.8/0.2),log(0.9/0.1))

Design=cbind(K,mP)

res_double_elo_posthoc=list()

for(j in 1:9){
  K=Design[j,1]
  mP=Design[j,2]
  prop_new=0.2
  res=elo_double_posthoc(n=n,
                         m=m,
                         reps=500,
                         games=1000,K=K,
                         mP=mP,
                         sP=0.5,
                         m_th=mP,s_th=1,
                         m_d=0,s_d=1.5,
                         res=res_double[[j]],
                         prop_new=prop_new)
  
  res_double_elo_posthoc[[j]]=res
  res_double_elo_posthoc[[j]]$mean=res$mean[order(res$true)[c(1:(n*prop_new))/prop_new],,]
  res_double_elo_posthoc[[j]]$var=res$var[order(res$true)[c(1:(n*prop_new))/prop_new],,]
  res_double_elo_posthoc[[j]]$true=res$true[order(res$true)[c(1:(n*prop_new))/prop_new]]
}

################################################################################
# saving the result as a list
################################################################################
save(res_double_elo_posthoc,file="ResultsNewPersons.RData")
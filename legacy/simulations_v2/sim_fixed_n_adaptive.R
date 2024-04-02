dyn.load('elo_for_simulations.so')
source('elo functions.R')

# Fixed items, non-adaptive selection
K=c(0.1,0.2,0.3,0.4,0.5,0.3,0.3)
s_d=c(rep(1,5),2,1)
true_items=c(rep(1,6),0)

Design=cbind(K,s_d,true_items)

res_fixed_nonadaptive=list()
for(j in 1:nrow(Design)){
  K=Design[j,1]
  s_d=Design[j,2]
  true_items=Design[j,3]
  games=1000
  if(j==3){games=5500} # 10 times more post-burnin iterations in the baseline condition to have a better estimate of the variances
  if(K==0.1){games=2000} # longer chains to make sure that it converges for smaller K
  if(K==0.2){games=1250}
  res_fixed_nonadaptive[[j]]=elo(n=500,m=100,reps=200,games=games,K=K,m_d=0,s_d=s_d,fixed_items=1,items_true=true_items)
}
save(res_fixed_nonadaptive,file='output/ResultsFixedAdaptive.RData')
# time estimate 205 seconds for n=500,m=100,reps=200. It will increase by 2, then n=1000, m=200, so about 7 minutes.

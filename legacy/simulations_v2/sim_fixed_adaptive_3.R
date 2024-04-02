dyn.load('elo_for_simulations.dll')
source('elo functions.R')

# Fixed items, adaptive item selection


K=c(0.1,0.2,0.3,0.4,0.5,0.3,0.3,0.3,0.3)
s_d=c(rep(1,5),2,1,1,1)
true_items=c(rep(1,6),0,1,1)
mP=c(rep(0,7),log(3),0)
sP=c(rep(0.5,8),0.25)

Design=cbind(K,s_d,true_items,mP,sP)
Design = Design[7:9, ]

res_fixed_adaptive=list()
for(j in 1:nrow(Design)){
  K=Design[j,1]
  s_d=Design[j,2]
  true_items=Design[j,3]
  mP=Design[j,4]
  sP=Design[j,5]
  games=1000
  if(j==3){games=5500} # 10 times more post-burnin iterations in the baseline condition to have a better estimate of the variances
  if(K==0.1){games=2000} # longer chains to make sure that it converges for smaller K
  if(K==0.2){games=1250}
  res_fixed_adaptive[[j]]=elo(n=500,m=100,reps=10,games=games,K=K,m_d=0,s_d=s_d,mP=mP,sP=sP,adaptive=1,fixed_items=1,items_true=true_items)
}
save(res_fixed_adaptive,file='output/ResultsFixedAdaptive_3.RData')

# time estimate for n=500, m=100, reps=10 is 331 seconds. It will increase by 
#2x2x20=80 when n=1000, m=200, reps=200, so about 6.5 hours
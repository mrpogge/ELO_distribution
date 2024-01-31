dyn.load('elo_for_simulations.dll')
source('elo functions.R')

# Non-fixed items, nonadaptive selection
K=c(0.1,0.2,0.3,0.4,0.5,0.3,0.3)
s_d=c(rep(1,5),2,1)
m_d=c(rep(0,6),-log(3))

Design=cbind(K,s_d,m_d)

res_nonadaptive=list()
for(j in 1:nrow(Design)){
  K=Design[j,1]
  s_d=Design[j,2]
  m_d=Design[j,3]
  games=1000
  if(j==3){games=5500} # 10 times more post-burnin iterations in the baseline condition to have a better estimate of the variances
  if(K==0.1){games=2000} # longer chains to make sure that it converges for smaller K
  if(K==0.2){games=1250}
  res_nonadaptive[[j]]=elo(n=500,m=100,reps=100,games=games,K=K,m_d=m_d,s_d=s_d)
}
save(res_nonadaptive,file='output/ResultsNonadaptive.RData')
# time estimate for n=500, m=100, reps=100 is 104 seconds. It will increase by 2x2=4 when n=1000, m=200, reps=200, so about 7 minutes
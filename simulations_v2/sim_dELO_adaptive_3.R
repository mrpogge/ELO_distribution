dyn.load('elo_for_simulations.dll')
source('elo functions.R')

# Adaptive scenarios with the Double Elo algorithm
K=c(0.1,0.2,0.3,0.4,0.5,0.3,0.3,0.3,0.3)
s_d=c(rep(1,5),2,1,1,1)
m_d=c(rep(0,6),-log(3),0,0)
mP=c(rep(0,7),log(3),0)
sP=c(rep(0.5,8),0.25)

Design=cbind(K,s_d,m_d,mP,sP)
Design = Design[7:9, ]

res_double=list()
for(j in 1:nrow(Design)){
  K=Design[j,1]
  s_d=Design[j,2]
  m_d=Design[j,3]
  mP=Design[j,4]
  sP=Design[j,5]
  games=1000
  if(j==3){games=5500} # 10 times more post-burnin iterations in the baseline condition to have a better estimate of the variances
  if(K==0.1){games=2000} # longer chains to make sure that it converges for smaller K
  if(K==0.2){games=1250}
  res_double[[j]]=elo_double(n=100,m=20,reps=10,games=games,K=K,mP=mP,sP=sP,m_d=m_d,s_d=s_d)
}
save(res_double,Design,file="output/ResultsDoubleElo_3.RData")

# time estimate for n=100, m=20, reps=10 is 27 seconds. It will increase by 10x10x20=2000 when n=1000, m=200, reps=200, so about 15 hours. 

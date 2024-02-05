dyn.load('elo_for_simulations.so')
source('elo functions.R')

K=c(0.1,0.2,0.3,0.4,0.5,0.3,0.3,0.3,0.3)
s_d=c(rep(1,5),2,1,1,1)
m_d=c(rep(0,6),-log(3),0,0)
mP=c(rep(0,7),log(3),0)
sP=c(rep(0.5,8),0.25)

Design=cbind(K,s_d,m_d,mP,sP)
res_double=list()
Design = Design[9, ]

K=Design[1]
s_d=Design[2]
m_d=Design[3]
mP=Design[4]
sP=Design[5]
games=2000
res_double=elo_double(n=1000, m=200, reps=200,games=games,K=K,mP=mP,sP=sP,m_d=m_d,s_d=s_d)
save(res_double,Design,file="output/ResultsDoubleElo_rerun9.RData")


# time estimate for n=100, m=20, reps=10 is 27 seconds. It will increase by 10x10x20=2000 when n=1000, m=200, reps=200, so about 15 hours. 

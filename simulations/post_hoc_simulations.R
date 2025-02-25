#########################################
#### Adaptive item selection based on true values

elo_true=function(n,m,reps,games,K,mP=0,sP=0.5,m_th=0,s_th=1,m_d=0,s_d=1.5,theta=NULL){
  tic()
  dyn.load("elo_for_simulations.dll")
  
  # n - number of persons
  # m - number of items
  # reps - number of replications
  # games - number of responses per person
  # K - K-factor
  # mP - mean of the normal kernel, desired probability correct
  # sP - sd of the normal kernel for item selection, SD around the desired probability correct
  # m_th and s_th - parameters of the normal distribution of the person abilities
  # m_d and s_d - parameters of the normal distribution of the item difficulties
  
  # specify true values
  if(is.null(theta)){
    theta = qnorm(seq(1/(n+1),n/(n+1),length=n),m_th,s_th)
    theta=sample(theta,n)
  }
  
  delta = qnorm(seq(1/(m+1),m/(m+1),length=m),m_d,s_d)
  
  A=-1/(2*sP^2)
  B=mP/sP^2
  
  #tic()
  
  theta_hat=rep(mean(theta),n*reps)
  delta_hat=rep(m_d,m*reps)
  
  mean_theta<-var_theta<-rep(0,n*games)
  mean_delta<-rep(0,m*games)
  
  tmp<-.C("elo_true",
            as.double(K),
            as.integer(reps),
            as.integer(games),
            as.integer(n),
            as.integer(m),
            as.double(theta),
            as.double(delta),
            as.double(theta_hat),
            as.double(delta_hat),
            as.double(mean_theta),
            as.double(var_theta),
            as.double(A),
            as.double(B),
            as.double(c(0:m)),
            as.double(mean_delta)		
    )
    
    mean_theta=matrix(tmp[[10]],ncol=games)
    var_theta=matrix(tmp[[11]],ncol=games)
    mean_delta=matrix(tmp[[15]],ncol=games)
  
  res=list(true=theta,mean=mean_theta,var=var_theta,mean_delta=mean_delta)
  
  toc()
  
  return(res)
  # the output is [[1]] - true values, 
  # [[2]] - means (across replications) of the Elo ratings of #each person at each iteration, 
  # [[3]] - variances (across replications) of the Elo ratings of #each person at each iteration
  # [[4]] - means (across replications) of the Elo ratings of each item at each iteration
}

res=elo_true(n=500,m=100,reps=20,games=2000,K=0.3,mP=log(0.7/0.3),m_th=log(0.7/0.3))

save(res,file="output/ResultsTrueElo.RData")

#########################################
#### Expected error in item difficulties

variance_inflation=function(n,m,reps,games,K,adaptive=0,mP=0,sP=0.5,fixed_items=0,items_true=0,m_th=0,s_th=1,m_d=0,s_d=1.5,theta=NULL){
  tic()
  dyn.load("elo_for_simulations.so")
  
  # n - number of persons
  # m - number of items
  # reps - number of replications
  # games - number of responses per person
  # K - K-factor
  # adaptive - 0 if nonadaptive, 1 if adaptive with normal kernel
  # mP - mean of the normal kernel, desired probability correct
  # sP - sd of the normal kernel for item selection, SD around the desired probability correct
  # fixed_items - 0 if items are updated, 1 if items are kept fixed
  # items_true - in case of keeping the items fixed: 0 if error is added to item estimates, 1 if true item parameters are used
  # m_th and s_th - parameters of the normal distribution of the person abilities
  # m_d and s_d - parameters of the normal distribution of the item difficulties
  
  # specify true values
  if(is.null(theta)){
    theta = qnorm(seq(1/(n+1),n/(n+1),length=n),m_th,s_th)
    theta=sample(theta,n)
  }
  
  delta = qnorm(seq(1/(m+1),m/(m+1),length=m),m_d,s_d)
  
  A=-1/(2*sP^2)
  B=mP/sP^2
  
  #tic()
  
  theta_hat=rep(mean(theta),n*reps)
  delta_hat=rep(m_d,m*reps)
  
  mean_theta<-var_theta<-exp_bias<-rep(0,n*games)
  mean_delta<-exp_bias_item <-rep(0,m*games)
  
  if(fixed_items==0){
    
     
    tmp<-.C("elo_inflation2",
            as.double(K),
            as.integer(reps),
            as.integer(games),
            as.integer(n),
            as.integer(m),
            as.double(theta),
            as.double(delta),
            as.double(theta_hat),
            as.double(delta_hat),
            as.double(mean_theta),
            as.double(var_theta),
            as.integer(adaptive),
            as.double(A),
            as.double(B),
            as.double(c(0:m)),
            as.double(exp_bias),
            as.double(exp_bias_item),
            as.double(mean_delta)
    )
    mean_delta=matrix(tmp[[18]],ncol=games)
    exp_bias=matrix(tmp[[16]],ncol=games)
    exp_bias_item=matrix(tmp[[17]],ncol=games)
  }
  
  mD=colMeans(mean_delta)
  exp_bias=apply(exp_bias,1,FUN=function(X){X-mD})
  
  res=list(true=theta,exp_bias=exp_bias)
  
  toc()
  
  return(res)
  # the output is [[1]] - true values, 
  # [[2]] - means (across replications) of the Elo ratings of #each person at each iteration, 
  # [[3]] - variances (across replications) of the Elo ratings of #each person at each iteration
  # [[4]] - means (across replications) of the Elo ratings of each item at each iteration
}

res=variance_inflation(n=500,m=100,reps=20,games=1000,K=0.3,adaptive=1,mP=log(0.7/0.3),m_th=log(0.7/0.3))
# For the paper we need n=500, m=100, games=1000 and reps=500. 

save(res,file='output/VarianceInflation.RData')


#########################################
### Autocorrelation in the updated-random scenario

elo_autocorrelation=function(n,m,reps,games,K,m_th=log(0.7/0.3),s_th=1,m_d=0,s_d=1.5,theta=NULL){
  tic()
  
  # n - number of persons
  # m - number of items
  # reps - number of replications
  # games - number of responses per person
  # K - K-factor
  # m_th and s_th - parameters of the normal distribution of the person abilities
  # m_d and s_d - parameters of the normal distribution of the item difficulties
  
  # specify true values
  if(is.null(theta)){
    theta = qnorm(seq(1/(n+1),n/(n+1),length=n),m_th,s_th)
    theta=sample(theta,n)
  }
  
  delta = qnorm(seq(1/(m+1),m/(m+1),length=m),m_d,s_d)
  
  
  #tic()
  
  theta_hat=rep(mean(theta),n*reps)
  delta_hat=rep(m_d,m*reps)
  
  Theta=rep(0,n*reps*games)
  Mean_delta=rep(0,reps*games)
  
  mean_theta<-var_theta<-rep(0,n*games)
  mean_delta<-rep(0,m*games)
  
  
  tmp<-.C("elo_autocorrelation",
          as.double(K),
          as.integer(reps),
          as.integer(games),
          as.integer(n),
          as.integer(m),
          as.double(theta),
          as.double(delta),
          as.double(theta_hat),
          as.double(delta_hat),
          as.double(mean_theta),
          as.double(var_theta),
          as.double(Theta),
          as.double(Mean_delta),
          as.double(c(0:m)),
          as.double(mean_delta)		
  )
  
  Theta=array(tmp[[12]],dim=c(n,reps,games))
  Theta_list=list()
  Mean_delta=matrix(tmp[[13]],ncol=games)
  for(j in 1:n){
    Theta_list[[j]]=Theta[j,,]-Mean_delta
  }
  Autocor=sapply(X=Theta_list,FUN=function(X){it=ncol(X);
  C=numeric(500);
  for(j in 1:500){
    C[j]=cor(X[,it],X[,it-j])
  }
  return(C)
  })
  
  return(list(Theta=Theta,MD=Mean_delta,A=Autocor))
  
}    

Design=c(0.1,0.2,0.3,0.4,0.5)

res=list()

for(j in 1:5){
  res[[j]]=elo_autocorrelation(n=500,m=100,reps=200,games=1000,K=Design[j])
}
# For the paper n=1000, m=200, reps=500

save(res,file='output/ResultsAutocorrelation.RData')

#########################################
### Lagged adaptive item selection

elo_lag=function(n,m,reps,games,K,LH,mP,sP=0.5,m_d=0,s_d=1.5,m_th,s_th=1){
  
  A=-1/(2*sP^2)
  B=mP/sP^2
  
  # specify true values
  
  theta = qnorm(seq(1/(n+1),n/(n+1),length=n),m_th,s_th)
  theta=sample(theta,n)
  
  delta = qnorm(seq(1/(m+1),m/(m+1),length=m),m_d,s_d)
  
  theta_hist=rep(0,n*reps*LH)
  delta_hist=rep(0,m*reps*LH)
  
  
  theta_hat=rep(m_th,n*reps)
  delta_hat=rep(m_d,m*reps)
  
  mean_theta<-var_theta<-rep(0,n*games)
  mean_delta<-rep(0,m*games)
  
  tic()
  Items_total<-Items_count<-rep(0,m*reps)
  
  tmp<-.C("elo_history",
          as.double(K),
          as.integer(reps),
          as.integer(games),
          as.integer(n),
          as.integer(m),
          as.double(theta),
          as.double(delta),
          as.double(theta_hat),
          as.double(delta_hat),
          as.double(theta_hist),
          as.double(delta_hist),
          as.double(mean_theta),
          as.double(var_theta),
          as.double(A),
          as.double(B),
          as.double(rep(0,m+1)),
          as.integer(0),
          as.integer(LH),
          as.double(mean_delta),
          as.integer(Items_total),
          as.integer(Items_count))
  
  toc()
  
  mean_theta=matrix(tmp[[12]],ncol=games)
  var_theta=matrix(tmp[[13]],ncol=games)
  mean_delta=matrix(tmp[[19]],ncol=games)
  theta_hist=array(tmp[[10]],dim=c(n,reps,LH))
  apply(theta_hist,1,FUN=function(X){C=NULL;for(i in 1:(ncol(X)-1)){C[i]=cor(X[,ncol(X)],X[,ncol(X)-i])};return(C)})->A
  
  res=list(true=theta,mean=mean_theta,var=var_theta,mean_delta=mean_delta,theta_hist=theta_hist,A=A)
  
  return(res)
}

L=c(1,2,5,10,20,50,100,200,500)
res=list()
for(j in 1:9){
  res[[j]]=elo_lag(n=500,m=100,reps=10,K=0.3,games=1000+L[j],LH=L[j],mP=log(0.7/0.3),m_th=log(0.7/0.3))
}
# For the paper n=1000, m=200, and reps=100 
save(res,file='output/ResultsDifferentLag.RData')

Design=cbind(c(0.3,0.1,0.2,0.4,0.5,0.3,0.3,0.3,0.3),c(rep(0.7,5),0.5,0.6,0.8,0.9))
res_lagged=list()
for(j in 1:9){
  K=Design[j,1]
  mP<-m_th<-log(Design[j,2]/(1-Design[j,2]))
  reps=100
  #reps=500;if(j==1){reps=10000} # this is the number of replications that we should run for the paper
  res_lagged[[j]]<-elo_lag(n=500,m=100,games=1500,reps=reps,K=K,mP=mP,m_th=m_th,LH=500)
}
# For the paper n=1000, m=200, 
save(res_lagged,file='output/ResultsLagged.RData')

### Simulation with Double Elo

K=c(0.3,0.1,0.2,0.4,0.5,0.3,0.3,0.3,0.3)
mP=c(rep(log(0.7/0.3),5),0,log(0.6/0.4),log(0.8/0.2),log(0.9/0.1))

Design=cbind(K,mP)
niter=c(1100,2000,1300,1000,1000,1000,1000,1100,1600)

n=500
m=100
# for the paper we will need n=1000, m=200

res_double=list()
for(j in 1:9){
  K=Design[j,1]
  mP=Design[j,2]
  reps=100
  #reps=500;if(j==1){reps=10000} # this is the number of replications that we should run for the paper
  res_double[[j]]=elo_double(n=n,m=m,reps=reps,games=niter[j],K=K,mP=mP,sP=0.5,m_d=0,s_d=1.5,m_th=mP,s_th=1)
  save(res_double,Design,file="output/ResultsDoubleElo.RData")
}


elo_double_posthoc=function(n,m,reps,games,K,mP,sP,m_th=0,s_th=1,m_d=0,s_d=1,res,prop_new=0.5){
  
  nit=dim(res$var)[2]
  V=mean(res$var[,(nit-499):nit,])
  M_th=rowMeans(((res$mean[,,1]+res$mean[,,2])/2)[,(nit-499):nit])[order(res$true)]
  M_d=rowMeans(((res$mean_delta[,,1]+res$mean_delta[,,2])/2)[,(nit-499):nit])
  
  tic()
  # n - number of persons
  # m - number of items
  # reps - number of replications
  # games - number of responses per person
  # K - K-factor
  # mP - mean of the normal kernel, desired logit of the probability correct
  # sP - sd of the normal kernel for item selection, SD around the desired logit of the probability correct
  # m_th and s_th - parameters of the normal distribution of the person abilities
  # m_d and s_d - parameters of the normal distribution of the item difficulties
  
  theta = qnorm(seq(1/(n+1),n/(n+1),length=n),m_th,s_th)
  P=sample(c(1:n),n)
  
  delta = qnorm(seq(1/(m+1),m/(m+1),length=m),m_d,s_d)
  
  A=-1/(2*sP^2)
  B=mP/sP^2
  
  theta=theta[P]
  
  theta_hat=NULL
  for(j in 1:(2*reps)){
    t=rnorm(n,M_th,sqrt(V))
    t[c(1:(n*prop_new))*(1/prop_new)]=m_th
    t=t[P]
    theta_hat=c(theta_hat,t)
  }
  
  delta_hat=NULL
  for(j in 1:(2*reps)){
    delta_hat=c(delta_hat,rnorm(m,M_d,sqrt(V)))
  }

  mean_theta<-var_theta<-rep(0,n*games*2)
  mean_delta<-var_delta<-rep(0,m*games*2)
  
  tmp<-.C("elo_double",
          as.double(K),
          as.integer(reps),
          as.integer(games),
          as.integer(n),
          as.integer(m),
          as.double(theta),
          as.double(delta),
          as.double(theta_hat),
          as.double(delta_hat),
          as.double(mean_theta),
          as.double(var_theta),
          as.double(A),
          as.double(B),
          as.double(rep(0,m+1)),
          as.double(mean_delta)
  )
  
  mean_theta=array(tmp[[10]],dim=c(n,games,2))
  var_theta=array(tmp[[11]],dim=c(n,games,2))
  mean_delta=array(tmp[[15]],dim=c(m,games,2))
  res=list(true=theta,mean=mean_theta,var=var_theta,mean_delta=mean_delta)
  toc()
  return(res)
  
  # the output is [[1]] - true values of the persons, 
  # [[2]] - means (across replications) of the Elo ratings of #each person at each iteration, separately for the two parallel chains, 
  # [[3]] - variances (across replications) of the Elo ratings of #each person at each iteration, separately for the two parallel chains, 
  # [[4]] - means (across replications) of the Elo ratings of each item at each iteration, separately for the two parallel chains, 
  
}

K=c(0.3,0.1,0.2,0.4,0.5,0.3,0.3,0.3,0.3)
mP=c(rep(log(0.7/0.3),5),0,log(0.6/0.4),log(0.8/0.2),log(0.9/0.1))

Design=cbind(K,mP)

res_new=list()

for(j in 1:9){
  K=Design[j,1]
  mP=Design[j,2]
  prop_new=0.2
  res=elo_double_posthoc(n=n,m=m,reps=50,games=1000,K=K,mP=mP,sP=0.5,m_th=mP,s_th=1,m_d=0,s_d=1.5,res=res_double[[j]],prop_new=prop_new)
  # for the paper we need reps=500
  res_new[[j]]=res
  res_new[[j]]$mean=res$mean[order(res$true)[c(1:(n*prop_new))/prop_new],,]
  res_new[[j]]$var=res$var[order(res$true)[c(1:(n*prop_new))/prop_new],,]
  res_new[[j]]$true=res$true[order(res$true)[c(1:(n*prop_new))/prop_new]]
  save(res_new,file="output/ResultsNewPersons.RData")
}

elo_save=function(n,m,reps,games,K,adaptive=0,mP=0,sP=1,fixed_items=0,items_true=0,m_th=0,s_th=1,m_d=0,s_d=1.5,theta=NULL){
  tic()
  dyn.load("elo_for_simulations.dll")
  
  # n - number of persons
  # m - number of items
  # reps - number of replications
  # games - number of responses per person
  # K - K-factor
  # adaptive - 0 if nonadaptive, 1 if adaptive with normal kernel
  # mP - mean of the normal kernel, desired probability correct
  # sP - sd of the normal kernel for item selection, SD around the desired probability correct
  # fixed_items - 0 if items are updated, 1 if items are kept fixed
  # items_true - in case of keeping the items fixed: 0 if error is added to item estimates, 1 if true item parameters are used
  # m_th and s_th - parameters of the normal distribution of the person abilities
  # m_d and s_d - parameters of the normal distribution of the item difficulties
  
  # specify true values
  if(is.null(theta)){
    theta = qnorm(seq(1/(n+1),n/(n+1),length=n),m_th,s_th)
    theta=sample(theta,n)
  }
  
  delta = qnorm(seq(1/(m+1),m/(m+1),length=m),m_d,s_d)
  
  A=-1/(2*sP^2)
  B=mP/sP^2
  
  #tic()
  
  theta_hat=rep(mean(theta),n*reps)
  delta_hat=rep(m_d,m*reps)
  
  mean_theta<-var_theta<-rep(0,n*games)
  mean_delta<-rep(0,m*games)
  
  if(fixed_items==0){
    
    tmp<-.C("elo",
            as.double(K),
            as.integer(reps),
            as.integer(games),
            as.integer(n),
            as.integer(m),
            as.double(theta),
            as.double(delta),
            as.double(theta_hat),
            as.double(delta_hat),
            as.double(mean_theta),
            as.double(var_theta),
            as.integer(adaptive),
            as.double(A),
            as.double(B),
            as.double(c(0:m)),
            as.double(mean_delta)		
    )
    
    theta_hat=tmp[[8]]
    delta_hat=tmp[[9]]
    mean_theta=matrix(tmp[[10]],ncol=games)
    var_theta=matrix(tmp[[11]],ncol=games)
    mean_delta=matrix(tmp[[16]],ncol=games)
  }
  
  if(fixed_items==1){
    if(items_true==1){
      d=delta
    }
    if(items_true==0){
      d=delta+rnorm(m,0,0.1)
    }
    tmp<-.C("elo_fixed_items",
            as.double(K),
            as.integer(reps),
            as.integer(games),
            as.integer(n),
            as.integer(m),
            as.double(theta),
            as.double(delta),
            as.double(theta_hat),
            as.double(d),
            as.double(mean_theta),
            as.double(var_theta),
            as.integer(adaptive),
            as.double(A),
            as.double(B),
            as.double(c(0:m)))
    
    mean_theta=matrix(tmp[[10]],ncol=games)
    var_theta=matrix(tmp[[11]],ncol=games)
    mean_delta=delta
  }
  
  res=list(true=theta,theta_hat=theta_hat,delta_hat=delta_hat)
  
  toc()
  
  return(res)
  # the output is [[1]] - true values, 
  # [[2]] - means (across replications) of the Elo ratings of #each person at each iteration, 
  # [[3]] - variances (across replications) of the Elo ratings of #each person at each iteration
  # [[4]] - means (across replications) of the Elo ratings of each item at each iteration
}

res=elo_save(500,100,500,1000,0.3,adaptive=0,m_th=log(0.7/0.3))



# Expected error in item difficulties
# Average error in person abilities
source("elo_functions.R")

variance_inflation2=function(n,m,reps,games,K,adaptive=0,mP=0,sP=0.5,fixed_items=0,items_true=0,m_th=0,s_th=1,m_d=0,s_d=1.5,theta=NULL){
  tic()
  dyn.load("elo_for_simulations.so")
  
  # n - number of persons
  # m - number of items
  # reps - number of replications
  # games - number of responses per person
  # K - K-factor
  # adaptive - 0 if nonadaptive, 1 if adaptive with normal kernel
  # mP - mean of the normal kernel, desired probability correct
  # sP - sd of the normal kernel for item selection, SD around the desired probability correct
  # fixed_items - 0 if items are updated, 1 if items are kept fixed
  # items_true - in case of keeping the items fixed: 0 if error is added to item estimates, 1 if true item parameters are used
  # m_th and s_th - parameters of the normal distribution of the person abilities
  # m_d and s_d - parameters of the normal distribution of the item difficulties
  #theta=NULL
  
  #mP<-m_th<-log(0.7/0.3)
  #sP=0.5
  #s_th=1
  #s_d=1.5
  #m_d=0
  #adaptive=1
  #fixed_items=0
  #K=0.3
  #n=500
  #m=100
  #reps=100
  #games=150
  
  # specify true values
  if(is.null(theta)){
    theta = qnorm(seq(1/(n+1),n/(n+1),length=n),m_th,s_th)
    theta=sample(theta,n)
  }
  
  delta = qnorm(seq(1/(m+1),m/(m+1),length=m),m_d,s_d)
  
  A=-1/(2*sP^2)
  B=mP/sP^2
  
  #tic()
  
  theta_hat=rep(mean(theta),n*reps)
  delta_hat=rep(m_d,m*reps)
  
  mean_theta<-var_theta<-exp_bias<-rep(0,n*games)
  mean_delta<-rep(0,m*games)
  exp_bias_items<-rep(0,m*reps*games)
  
    
    
    tmp<-.C("elo_inflation2",
            as.double(K),
            as.integer(reps),
            as.integer(games),
            as.integer(n),
            as.integer(m),
            as.double(theta),
            as.double(delta),
            as.double(theta_hat),
            as.double(delta_hat),
            as.double(mean_theta),
            as.double(var_theta),
            as.integer(adaptive),
            as.double(A),
            as.double(B),
            as.double(c(0:m)),
            as.double(exp_bias),
            as.double(exp_bias_items),
            as.double(mean_delta)
    )

    exp_bias=matrix(tmp[[16]],ncol=games)
   exp_bias_items<-ebi<-array(tmp[[17]],dim=c(m,reps,games))
  mean_delta=matrix(tmp[[18]],ncol=games)
  
  exp_bias_items[exp_bias_items==0]=NA
  exp_bias_items=apply(exp_bias_items,c(1,3),mean,na.rm=TRUE)
  
  mD=colMeans(mean_delta)
  exp_bias=apply(exp_bias,1,FUN=function(X){X-mD})
  exp_bias_items=apply(exp_bias_items,1,FUN=function(X){X-mD})
  res=list(true=theta,exp_bias=exp_bias,ebi=exp_bias_items)
  
  toc()
  
  return(res)
  # the output is [[1]] - true values, 
  # [[2]] - means (across replications) of the Elo ratings of #each person at each iteration, 
  # [[3]] - variances (across replications) of the Elo ratings of #each person at each iteration
  # [[4]] - means (across replications) of the Elo ratings of each item at each iteration
}

res=variance_inflation2(n=500,m=100,reps=2,games=1000,K=0.3,adaptive=1,mP=log(0.7/0.3),m_th=log(0.7/0.3))
# For the paper we need n=1000, m=200, games=1000 and reps=500. 



theta=res$true
v=c(-2,-1,0,1,2)+mean(theta)
P=c(which.min(abs(theta-v[1])))
for(i in 2:length(v)){P=c(P,which.min(abs(theta-v[i])))}
matplot((res$exp_bias[1:1000,P]),ylim=c(-6,6),type='l',lty=1,col=c(1:length(v)),xlab='Items answered',ylab='Expected error in item difficulty',cex.lab=2,cex.axis=2)
matplot(res$ebi[,c(9,26,50,75,92)],ylim=c(-6,6),type='l',ylab='Average error in person ability',xlab='Timepoint',cex.lab=2,cex.axis=2)

#exp_bias = expected bias
#ebi = expected bias items





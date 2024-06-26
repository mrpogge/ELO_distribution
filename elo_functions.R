################################################################################
# Wrapper functions for the C routines that perform simulations
# with different kind of ELO algorithms
################################################################################
library(tictoc)


################################################################################
# ELO algorithm wrapper:
# performs simulation with traditional ELO algorithm with or wtihout fixed items
################################################################################
elo=function(n,
             m,
             reps,
             games,
             K,
             adaptive=0,mP=0,sP=1,
             fixed_items=0,items_true=0,
             m_th=0,s_th=1,
             m_d=0,s_d=1.5,
             theta=NULL,
             OS = "MAC"){
#-------------------------------------------------------------------------------
# Params
#-------------------------------------------------------------------------------
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
  
#-------------------------------------------------------------------------------
# loading the compiled C routines
#-------------------------------------------------------------------------------
tic()
if(OS == "MAC"){
  dyn.load("elo_for_simulations.so")
} else if(OS == "WINDOWS"){
  dyn.load("elo_for_simulations.dll")
}

# specify true values
if(is.null(theta)){
  theta = qnorm(seq(1/(n+1),n/(n+1),length=n),m_th,s_th)
  theta=sample(theta,n)
}

delta = qnorm(seq(1/(m+1),m/(m+1),length=m),m_d,s_d)

A=-1/(2*sP^2)
B=mP/sP^2

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

res=list(true=theta,mean=mean_theta,var=var_theta,mean_delta=mean_delta)

toc()

return(res)
# the output is [[1]] - true values, 
# [[2]] - means (across replications) of the Elo ratings of #each person at each iteration, 
# [[3]] - variances (across replications) of the Elo ratings of #each person at each iteration
# [[4]] - means (across replications) of the Elo ratings of each item at each iteration
}


################################################################################
# double ELO algorithm wrapper:
# performs simulation with double ELO algorithm
################################################################################
elo_double=function(n,
                    m,
                    reps,
                    games,
                    K,
                    mP,sP,
                    m_th=0,s_th=1,
                    m_d=0,s_d=1,
                    OS = "MAC"){
# n - number of persons
# m - number of items
# reps - number of replications
# games - number of responses per person
# K - K-factor
# mP - mean of the normal kernel, desired logit of the probability correct
# sP - sd of the normal kernel for item selection, SD around the desired logit of the probability correct
# m_th and s_th - parameters of the normal distribution of the person abilities
# m_d and s_d - parameters of the normal distribution of the item difficulties

  #-------------------------------------------------------------------------------
  # loading the compiled C routines
  #-------------------------------------------------------------------------------
  tic()
  if(OS == "MAC"){
    dyn.load("elo_for_simulations.so")
  } else if(OS == "WINDOWS"){
    dyn.load("elo_for_simulations.dll")
  }
  
  
theta = qnorm(seq(1/(n+1),n/(n+1),length=n),m_th,s_th)
theta=sample(theta,n)

delta = qnorm(seq(1/(m+1),m/(m+1),length=m),m_d,s_d)

A=-1/(2*sP^2)
B=mP/sP^2

theta_hat=rep(m_th,n*reps*2)
delta_hat=rep(m_d,m*reps*2)

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

################################################################################
# modified ELO algorithm wrapper:
# performs simulation with modified ELO algorithm 
################################################################################
elo_modifications=function(n,
                           m,
                           reps,
                           games,
                           K,
                           mP=0,sP=0,
                           m_th=0,s_th=1,
                           m_d=0,s_d=2,
                           type,LH=10,
                           OS = "MAC"){
# n - number of persons
# m - number of items
# reps - number of replications
# games - number of responses per person
# K - K-factor
# mP - mean of the normal kernel, desired probability correct
# sP - sd of the normal kernel for item selection, SD around the desired probability correct
# m_th and s_th - parameters of the normal distribution of the person abilities
# m_d and s_d - parameters of the normal distribution of the item difficulties
# type: "MH", 'lag','average'
  
  #-------------------------------------------------------------------------------
  # loading the compiled C routines
  #-------------------------------------------------------------------------------
  tic()
  if(OS == "MAC"){
    dyn.load("elo_for_simulations.so")
  } else if(OS == "WINDOWS"){
    dyn.load("elo_for_simulations.dll")
  }
  
  

A=-1/(2*sP^2)
B=mP/sP^2

# specify true values

theta = qnorm(seq(1/(n+1),n/(n+1),length=n),m_th,s_th)
theta=sample(theta,n)

delta = qnorm(seq(1/(m+1),m/(m+1),length=m),m_d,s_d)

if(type!='MH'){
	theta_hist=rep(0,n*reps*LH)
	delta_hist=rep(0,m*reps*LH)
}

theta_hat=rep(0,n*reps)
delta_hat=rep(0,m*reps)

mean_theta<-var_theta<-rep(0,n*games)
mean_delta<-rep(0,m*games)

if(type=='MH'){

	tmp<-.C("elo_MH",
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
		as.double(rep(0,m)),
		as.double(rep(0,m)),
		as.double(mean_delta))

	mean_theta=matrix(tmp[[10]],ncol=games)
	var_theta=matrix(tmp[[11]],ncol=games)
	mean_delta=matrix(tmp[[17]],ncol=games)

}


if(type!="MH"){	

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
		as.integer(1*(type=='average')),
		as.integer(LH),
		as.double(mean_delta))

	mean_theta=matrix(tmp[[12]],ncol=games)
	var_theta=matrix(tmp[[13]],ncol=games)
	mean_delta=matrix(tmp[[19]],ncol=games)

}
	
res=list(true=theta,mean=mean_theta,var=var_theta,mean_delta=mean_delta)

return(res)

# the output is [[1]] - true values, 
#[[2]] - means (across replications) of the Elo ratings of #each person at each iteration, 
#[[3]] - variances (across replications) of the Elo ratings of #each person at each iteration
#[[4]] - means (across replications) of the items' ratings
}

################################################################################
# ideal ELO algorithm wrapper:
################################################################################
elo_ideal=function(reps,games,K,P,theta=NULL){
  tic()
  dyn.load("elo_for_simulations.dll")
  
  n=length(theta)
  
  tic()
  
  theta_hat=rep(0,n*reps)
  mean_theta<-var_theta<-rep(0,n*games)
  
  P=log(P/(1-P))
  
  tmp<-.C("elo_ideal",
          as.double(K),
          as.integer(reps),
          as.integer(games),
          as.integer(n),
          as.double(theta),
          as.double(theta_hat),
          as.double(mean_theta),
          as.double(var_theta),
          as.double(P))
      
    mean_theta=matrix(tmp[[7]],ncol=games)
    var_theta=matrix(tmp[[8]],ncol=games)
    
  res=list(true=theta,mean=mean_theta,var=var_theta)
  
  toc()
  
  return(res)
  # the output is [[1]] - true values, 
  # [[2]] - means (across replications) of the Elo ratings of #each person at each iteration, 
  # [[3]] - variances (across replications) of the Elo ratings of #each person at each iteration
}


################################################################################
# Supplementary simulations 
################################################################################
elo_true=function(n,m,reps,games,K,mP=0,sP=0.5,m_th=0,s_th=1,m_d=0,s_d=1.5,theta=NULL){
  tic()
  dyn.load("elo_for_simulations.so")
  
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

elo_autocorrelation=function(n,m,reps,games,K,m_th=log(0.7/0.3),s_th=1,m_d=0,s_d=1.5,theta=NULL){
  tic()
  dyn.load("elo_for_simulations.so")
  
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

elo_lag=function(n,m,reps,games,K,LH,mP,sP=0.5,m_d=0,s_d=1.5,m_th,s_th=1){
  dyn.load("elo_for_simulations.so")
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

elo_double_posthoc=function(n,m,reps,games,K,mP,sP,m_th=0,s_th=1,m_d=0,s_d=1,res,prop_new=0.5){
  dyn.load("elo_for_simulations.so")
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

elo_save=function(n,m,reps,games,K,adaptive=0,mP=0,sP=1,fixed_items=0,items_true=0,m_th=0,s_th=1,m_d=0,s_d=1.5,theta=NULL){
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
  
  return(tmp)
  # the output is [[1]] - true values, 
  # [[2]] - means (across replications) of the Elo ratings of #each person at each iteration, 
  # [[3]] - variances (across replications) of the Elo ratings of #each person at each iteration
  # [[4]] - means (across replications) of the Elo ratings of each item at each iteration
}



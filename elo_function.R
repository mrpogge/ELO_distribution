##############################################################################
##############################################################################
# Main wrapper function for ELO based simulation using a C routine
##############################################################################
##############################################################################
elo=function(n,
             m,
             reps,
             games,
             K,
             adaptive=0,mP=0,sP=0,
             fixed_items=0,
             items_true=0,
             save_replications=0,
             m_th=0,s_th=1,
             m_d=0,s_d=2,
             OS = "WINDOWS"){
  
  if(OS == "MAC"){
    dyn.load("elo.so")
  } else if(OS == "WINDOWS"){
    dyn.load("elo.dll")
  } else if(OS == "LINUX"){
    dyn.load("elo.o")
  } else {
    stop("Not supported operating system, please recompile the C file. //
         But first check for typos in the argument (MAC, WINDOWS, LINUX).")
  }
  
  
  ##############################################################################
  # Parameters
  ##############################################################################
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
  # save_replications - 0 only means and variances per timepoint are saved, 1 all replications are saved
  # m_th and s_th - parameters of the normal distribution of the person abilities
  # m_d and s_d - parameters of the normal distribution of the item difficulties

  ##############################################################################
  # Simulating true ability and difficulty
  ##############################################################################
  theta = qnorm(seq(1/(n+1),n/(n+1),length=n),m_th,s_th)
  theta=sample(theta,n) #create random ability order
  
  delta = qnorm(seq(1/(m+1),m/(m+1),length=m),m_d,s_d)
  
  # re-scale to make the overall mean 0
  Mean=mean(c(theta,delta))
  theta=theta-Mean
  delta=delta-Mean
  
  # make a matrix with true values at every timepoint
  Theta=matrix(ncol=games,nrow=n)
  Delta=matrix(ncol=games,nrow=m)
  
  for(j in 1:games){
    Theta[,j]=theta
    Delta[,j]=delta
  }

  ##############################################################################
  # Only means and variances are saved in this setting
  ##############################################################################
  if(save_replications==0){
    
    #creating empty containers for the results 
    theta_hat=rep(0,n*reps)
    delta_hat=rep(0,m*reps)
    
    mean_theta<-var_theta<-rep(0,n*games)
    mean_delta<-var_delta<-rep(0,m*games)
    
    ##############################################################################
    # Normal ELO with symmetric updates
    ##############################################################################
    if(fixed_items==0){
      tmp<-.C("elo2",
              as.double(K),
              as.integer(reps),
              as.integer(games),
              as.integer(n),
              as.integer(m),
              as.double(Theta),
              as.double(Delta),
              as.double(theta_hat),
              as.double(delta_hat),
              as.double(mean_theta),
              as.double(mean_delta),
              as.double(var_theta),
              as.double(var_delta),
              as.integer(adaptive),
              as.double(mP),
              as.double(sP),
              as.double(rep(0,m+1)))
      
      mean_theta=matrix(tmp[[10]],ncol=games)
      var_theta=matrix(tmp[[12]],ncol=games)
    }
    
    ##############################################################################
    # ELO with only student updates
    ##############################################################################
    if(fixed_items==1){
      #no measurement error in item difficulties
      if(items_true==1){
        d=delta
      }
      #measurement error in item difficulties
      if(items_true==0){
        d=delta+rnorm(m,0,0.1)
      }
    
      tmp<-.C("elo2_fixed_items",
              as.double(K),
              as.integer(reps),
              as.integer(games),
              as.integer(n),
              as.integer(m),
              as.double(Theta),
              as.double(Delta),
              as.double(theta_hat),
              as.double(d),
              as.double(mean_theta),
              as.double(var_theta),
              as.integer(adaptive),
              as.double(mP),
              as.double(sP),
              as.double(rep(0,m+1)))
      
      mean_theta=matrix(tmp[[10]],ncol=games)
      var_theta=matrix(tmp[[11]],ncol=games)
    }
    #returning limited results
    res=list(true=theta,mean=mean_theta,var=var_theta)
  }
  ##############################################################################
  # All estimates are saved over all iterations and replications
  ##############################################################################
  if(save_replications==1){
    
    theta_hat=rep(0,n*reps*games)
    t=rep(0,n)
    ##############################################################################
    # Normal ELO with symmetric updates
    ##############################################################################
    if(fixed_items==0){
      delta_hat=rep(0,m*reps*games)
      d=rep(0,n)
      
      tmp<-.C("elo",
              as.double(K),
              as.integer(reps),
              as.integer(games),
              as.integer(n),
              as.integer(m),
              as.double(Theta),
              as.double(Delta),
              as.double(t),
              as.double(d),
              as.double(theta_hat),
              as.double(delta_hat),
              as.integer(adaptive),
              as.double(mP),
              as.double(sP),
              as.double(rep(0,m+1)))
      
      theta_hat=array(tmp[[10]],dim=c(n,games,reps))
    }
    ##############################################################################
    # ELO with only player updates
    ##############################################################################
    if(fixed_items==1){
      #items with no measurement error
      if(items_true==1){
        d=delta
      }
      #items with measurement error
      if(items_true==0){
        d=delta+rnorm(m,0,0.1)
      }
      
      tmp<-.C("elo_fixed_items",
              as.double(K),
              as.integer(reps),
              as.integer(games),
              as.integer(n),
              as.integer(m),
              as.double(Theta),
              as.double(Delta),
              as.double(t),
              as.double(d),
              as.double(theta_hat),
              as.integer(adaptive),
              as.double(mP),
              as.double(sP),
              as.double(rep(0,m+1)))
      
      theta_hat=array(tmp[[10]],dim=c(n,games,reps))
    }
    #returning full results
    res=list(true=theta,est=theta_hat)
  }

  #function return
  return(res)
  ##############################################################################
  # Output data format
  ##############################################################################
  
  #-----------------------------------------------------------------------------
  #if replications are saved, then the output is:
  #   [[1]] - true values, 
  #   [[2]] - Elo ratings of each person at each iteration in each replication (array n x games x reps)
  #-----------------------------------------------------------------------------
  # if replication are not saved, then the output is:
  #.  [[1]] - true values, 
  #.  [[2]] - means (across replications) of the Elo ratings of each person at each iteration, 
  #   [[3]] - variances (across replications) of the Elo ratings of each person at each iteration
  
}
##############################################################################
# Example usage
##############################################################################
#-----------------------------------------------------------------------------
#res1=elo(n=200,m=100,reps=100,games=250,K=0.3,adaptive=1,mP=0.5,sP=0.1,save_replications=1)
#res2=elo(n=200,m=100,reps=100,games=250,K=0.3,adaptive=1,mP=0.5,sP=0.1,save_replications=0, OS = "MAC")
#res3=elo(n=200,m=100,reps=100,games=250,K=0.3,adaptive=1,mP=0.5,sP=0.1,save_replications=1,fixed_items=1,items_true=1)
#res4=elo(n=200,m=100,reps=100,games=250,K=0.3,adaptive=1,mP=0.5,sP=0.1,save_replications=0,fixed_items=1,items_true=1)
#res5=elo(n=200,m=100,reps=100,games=250,K=0.3,adaptive=0,mP=0.5,sP=0.1,save_replications=1)
#res6=elo(n=200,m=100,reps=100,games=250,K=0.3,adaptive=0,mP=0.5,sP=0.1,save_replications=0)
#res7=elo(n=200,m=100,reps=100,games=250,K=0.3,adaptive=0,mP=0.5,sP=0.1,save_replications=1,fixed_items=1,items_true=1)
#res8=elo(n=200,m=100,reps=100,games=250,K=0.3,adaptive=0,mP=0.5,sP=0.1,save_replications=0,fixed_items=1,items_true=1)
#-----------------------------------------------------------------------------

##############################################################################
##############################################################################
# Plotting based on the resulting list
##############################################################################
##############################################################################
plot_elo=function(res,add=FALSE, ylim = c(-3.2, 3.2)){
  theta=res$true
  if(length(res)==3){
    mean_theta=apply(res$est,c(1,2),mean)
  }
  if(length(res)==4){
    mean_theta=res$mean
  }
  theta=theta-mean(theta)
  mean_theta=apply(res$mean,2,FUN=function(X){X-mean(X)})
  v=c(-2.25,-1.5,-0.75,0,0.75,1.5,2.25)
  P=c(which.min(abs(theta-v[1])))
  for(i in 2:length(v)){P=c(P,which.min(abs(theta-v[i])))}
  matplot(t(mean_theta[P,]),type='l',add=add, ylim = ylim)
  abline(h=theta[P],col=c(1:length(v)))
}

# compute the mean variance, median variance and variances of persons closest to c(-2.25,-1.5,-0.75,0,0.75,1.5,2.25), calculated and predicted
mean_elo=function(res){
  theta=res$true
  if(length(res)==3){
    mean_theta=apply(res$est,c(1,2),mean)
  }
  if(length(res)==4){
    mean_theta=res$mean
  }
  theta=theta-mean(theta)
  iter=ncol(mean_theta)
  Mean=rowMeans(mean_theta[,(1+iter/2):iter])
  list(mean=mean(Mean),median=median(Mean), bias = (mean(Mean)-mean(theta))^2)
}

##############################################################################
##############################################################################
# TODO: Ask what is this :D
##############################################################################
##############################################################################
bias_correction=function(res){
  theta=res$true
  if(length(res)==3){
    mean_theta=apply(res$est,c(1,2),mean)
  }
  if(length(res)==4){
    mean_theta=res$mean
  }
  iter=ncol(mean_theta)
  theta=theta-mean(theta)
  mean_theta=rowMeans(mean_theta[,(1+iter/2):iter])
  mean_theta=mean_theta-mean(mean_theta)
  coef(lm(theta~0+mean_theta))
}

##############################################################################
##############################################################################
# TODO: Ask what is this :D
##############################################################################
##############################################################################
plot_elo_var=function(res,add=FALSE,points=TRUE,col=1){
  theta=res$true
  if(length(res)==3){
    var_theta=apply(res$est,c(1,2),var)
  }
  if(length(res)==4){
    var_theta=res$var
  }
  theta=theta-mean(theta)
  iter=ncol(var_theta)
  x=theta
  y=rowMeans(var_theta[,(1+iter/2):iter])
  x2=theta^2
  m<-coef(lm(y~x+x2))
  if(add==FALSE){
    if(points==TRUE){plot(x,y)
      curve(m[1]+m[2]*x+m[3]*x^2,add=TRUE,col=col)}
    if(points==FALSE){curve(m[1]+m[2]*x+m[3]*x^2,add=FALSE,col=col,from=-3,to=3)}	
  }
  if(add==TRUE){
    if(points==TRUE){
      points(x,y,col=col)
    }
    curve(m[1]+m[2]*x+m[3]*x^2,add=TRUE,col=col)
  }
}

# compute the mean variance, median variance and variances of persons closest to c(-2.25,-1.5,-0.75,0,0.75,1.5,2.25), calculated and predicted
var_elo=function(res){
  theta=res$true
  if(length(res)==3){
    var_theta=apply(res$est,c(1,2),var)
  }
  if(length(res)==4){
    var_theta=res$var
  }
  theta=theta-mean(theta)
  iter=ncol(var_theta)
  Var=rowMeans(var_theta[,(1+iter/2):iter])
  theta2=theta^2
  m<-coef(lm(Var~theta+theta2))
  v=c(-2.25,-1.5,-0.75,0,0.75,1.5,2.25)
  P=c(which.min(abs(theta-v[1])))
  for(i in 2:length(v)){P=c(P,which.min(abs(theta-v[i])))}
  list(mean=mean(Var),median=median(Var),computed=Var[P],predicted=m[1]+theta[P]*m[2]+theta2[P]*m[3])
}

##############################################################################
##############################################################################
# Creating result containers for the simulations (JUST IN CASE)
##############################################################################
##############################################################################
create_container = function(n,
                            m,
                            reps,
                            games,
                            K,
                            adaptive,
                            mP,
                            sP,
                            fixed_items,
                            m_th,
                            s_th,
                            m_d,
                            s_d,
                            n_){
  container = matrix(0, nrow = n*length(K), ncol = games+games+15)
  pars = c(n,m,reps,games,999,adaptive,mP,sP,fixed_items, items_true, m_th, s_th, m_d, s_d)
  pars = rep(pars, times = n*length(K))
  pars = matrix(pars, nrow = n*length(K), ncol = 14, byrow = TRUE)
  par_names = c("n", "m", "reps", "games", "K", "adaptive", "mP", "sP", "fixed_items", "items_true", "m_th", "s_th", "m_d", "s_d")
  res_names = c(par_names,"true_value", paste0("mean", 1:games), paste0("var", 1:games))
  container[,1:14] = pars
  colnames(container) = res_names
}

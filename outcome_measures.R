################################################################################
# OUTCOME MEASURES
################################################################################
# This script contains functions which are used as outcome measures in the paper
# All of this functions take a res type object which is the result of the 
#simulations. 
# A res object contains:
#                       - res$true: true values of the students in the system
#                       - res$mean: average rating over replications for all student
# for all iterations
#                       - res$var: the variance of ratings over replications
#                       - res$mean_delta: the mean of the item ratings in the last
# iteration

################################################################################
# BIAS CORRECTION
################################################################################
#bias correction takes the mean matrix of the res object and substracts the mean
#of the item ratings from it. Then it regresses the new mean rating estimates on
#theta. The output is the coefficient of this regression.

bias_correction=function(res){
  #select true values
  theta=res$true
  
  #legacy code: (this does the same thing as described, but for results that are not mean over replications)
  if(length(dim(res$mean))==3){
    mean_theta1=mapply(t=as.data.frame(res$mean[,,1]),d=as.data.frame(res$mean_delta[,,1]),FUN=function(t,d){t-mean(d)})
    mean_theta2=mapply(t=as.data.frame(res$mean[,,2]),d=as.data.frame(res$mean_delta[,,2]),FUN=function(t,d){t-mean(d)})
    mean_theta=(mean_theta1+mean_theta2)/2
  }
  
  #this part is the bias correction
  if(length(dim(res$mean))==2){
    mean_theta=res$mean
    if(length(res)==4){
      mean_theta=mapply(t=as.data.frame(res$mean),
                        d=as.data.frame(res$mean_delta),
                        FUN=function(t,d){t-mean(d)})
    }
  }
  iter=ncol(mean_theta)
  mean_theta=rowMeans(mean_theta[,(1+iter/2):iter])
  coef(lm(theta~0+mean_theta))
}

################################################################################
# Variance of ELO ratings
################################################################################
# This computes the: -  mean variance, 
#                     - median variance 
#                      -variances of persons closest to  0 +- 1 and +- 2 logit
#                     - predicted variance with a second order polinomial approx


var_elo=function(res, summary = FALSE){
  #selecting the true values
  theta=res$true
  #legacy code: for not summarised data
  if(length(dim(res$var))==3){
    var_theta=(res$var[,,1]+res$var[,,2])/2
  }
  
  #selecting the var matrix from the res object
  if(length(dim(res$var))==2){
    var_theta=res$var
  }
  
  #average variance for all students based on the second half of the iterations
  #theta=theta-mean(theta)
  iter=ncol(var_theta)
  Var=rowMeans(var_theta[,(1+iter/2):iter])
  #theta2=theta^2
  #m<-coef(lm(Var~theta+theta2))
  #v=c(-2.25,-1.5,-0.75,0,0.75,1.5,2.25)
  #P=c(which.min(abs(theta-v[1])))
  #for(i in 2:length(v)){P=c(P,which.min(abs(theta-v[i])))}
  #list(mean=mean(Var),median=median(Var),computed=Var[P],predicted=m[1]+theta[P]*m[2]+theta2[P]*m[3])
    return(median(Var))
}

var_elo_vec=function(res, summary = FALSE){
  #selecting the true values
  theta=res$true
  #legacy code: for not summarised data
  if(length(dim(res$var))==3){
    var_theta=(res$var[,,1]+res$var[,,2])/2
  }
  
  #selecting the var matrix from the res object
  if(length(dim(res$var))==2){
    var_theta=res$var
  }
  
  #average variance for all students based on the second half of the iterations
  #theta=theta-mean(theta)
  iter=ncol(var_theta)
  Var=rowMeans(var_theta[,(1+iter/2):iter])
  #theta2=theta^2
  #m<-coef(lm(Var~theta+theta2))
  #v=c(-2.25,-1.5,-0.75,0,0.75,1.5,2.25)
  #P=c(which.min(abs(theta-v[1])))
  #for(i in 2:length(v)){P=c(P,which.min(abs(theta-v[i])))}
  #list(mean=mean(Var),median=median(Var),computed=Var[P],predicted=m[1]+theta[P]*m[2]+theta2[P]*m[3])
  return(Var)
}

################################################################################
# HITTING TIME
################################################################################
# This computes the: - The first iteration when the absolute distance between a 
#given chain of ratings and the reference value is smaller than 0.01.

hitting_time=function(X){
  mean_theta=mapply(t=as.data.frame(X$mean),d=as.data.frame(X$mean_delta),FUN=function(t,d){t-mean(d)})
  iter=ncol(mean_theta)
  mean_inv=rowMeans(mean_theta[,(iter-499):iter])  
  ht=mapply(mean_inv=mean_inv,mean=as.data.frame(t(mean_theta)),FUN=function(mean,mean_inv){min(which(abs(mean-mean_inv)<0.01), na.rm = TRUE)})
  return(ht)
}

hitting_time_double=function(X, res_single){
  mean_theta1=mapply(t=as.data.frame(X$mean[,,1]),d=as.data.frame(X$mean_delta[,,1]),FUN=function(t,d){t-mean(d)})  
  mean_theta2=mapply(t=as.data.frame(X$mean[,,2]),d=as.data.frame(X$mean_delta[,,2]),FUN=function(t,d){t-mean(d)})
  mean_theta=(mean_theta1+mean_theta2)/2
  iter=ncol(mean_theta)
  mean_inv=rowMeans(mean_theta[,(iter-499):iter])  
  ht=mapply(mean_inv=mean_inv,mean=as.data.frame(t(mean_theta)),FUN=function(mean,mean_inv){min(which(abs(mean-mean_inv)<0.01))})
  return(ht*2)
}


################################################################################
# LEGACY: Absolute bias (one need to calculate the bias regarding to the diff of theta and the mean of items for identifiability)
################################################################################
absolute_bias = function(res){
  if(length(dim(res$mean))==3){
    res$mean=res$mean[,,1]
  }
  return(rowMeans(abs(res$mean[,501:1000] - res$true)))
}

################################################################################
# revisioned absolute bias
################################################################################
abs_bias_revised = function(X){
  mean_theta1=mapply(t=as.data.frame(X$mean[,,1]),d=as.data.frame(X$mean_delta[,,1]),FUN=function(t,d){t-mean(d)})  
  mean_theta2=mapply(t=as.data.frame(X$mean[,,2]),d=as.data.frame(X$mean_delta[,,2]),FUN=function(t,d){t-mean(d)})
  mean_theta=(mean_theta1+mean_theta2)/2
  iter=ncol(mean_theta)
  mean_theta=rowMeans(mean_theta[,(iter-499):iter])
  return(mean(abs(mean_theta-X$true)))
}

abs_bias_revised_single = function(X){
  mean_theta=mapply(t=as.data.frame(X$mean),d=as.data.frame(X$mean_delta),FUN=function(t,d){t-mean(d)})
  iter=ncol(mean_theta)
  mean_theta=rowMeans(mean_theta[,(iter-499):iter])
  return(mean(abs(mean_theta-X$true)))
}





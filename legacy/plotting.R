################################################################################
# LEGACY: trace plot for elo ratings for a few equally spaced studentsTEST 
################################################################################
plot_elo=function(res,add=FALSE,lwd=1){
  theta=res$true
  if(length(dim(res$mean))==2){
    mean_theta=res$mean
    if(length(res)==4){
      mean_theta=mapply(t=as.data.frame(res$mean),d=as.data.frame(res$mean_delta),FUN=function(t,d){t-mean(d)})
    }
    v=c(-2,-1,0,1,2)+mean(theta)
    P=c(which.min(abs(theta-v[1])))
    for(i in 2:length(v)){P=c(P,which.min(abs(theta-v[i])))}
    matplot(t(mean_theta[P,]),type='l',add=add,lty=1,lwd=lwd,col=c(1:length(v)),xlab='Items answered',ylab='Average Elo rating')
    abline(h=theta[P],col=c(1:length(v)),lty=2)
  }
  if(length(dim(res$mean))==3){
    mean_theta1=mapply(t=as.data.frame(res$mean[,,1]),d=as.data.frame(res$mean_delta[,,1]),FUN=function(t,d){t-mean(d)})
    mean_theta2=mapply(t=as.data.frame(res$mean[,,2]),d=as.data.frame(res$mean_delta[,,2]),FUN=function(t,d){t-mean(d)})
    v=c(-2,-1,0,1,2)+mean(theta)
    P=c(which.min(abs(theta-v[1])))
    for(i in 2:length(v)){P=c(P,which.min(abs(theta-v[i])))}
    matplot(t(mean_theta1[P,]),type='l',add=add,lty=1,lwd=lwd,col=c(1:length(v)),xlab='Item pairs answered',ylab='Average Elo rating')
    matplot(t(mean_theta2[P,]),type='l',add=TRUE,lty=1,lwd=lwd,col=c(1:length(v)),xlab='Items answered',ylab='Average Elo rating')
    abline(h=theta[P],col=c(1:length(v)),lty=2)
  }
}

################################################################################
# LEGACY: Plotting the true ability against the variance of the elo ratings 
# and a second order polinomial approximation
################################################################################

plot_elo_var=function(res,
                      add=FALSE,
                      points=TRUE,
                      col=1,
                      ylab = "Variance of Elo ratings",
                      xlab = "True ability",
                      ylim = c(0.15,0.2)
){
  theta=res$true
  if(length(dim(res$var))==3){
    var_theta=(res$var[,,1]+res$var[,,2])/2
  }
  if(length(dim(res$var))==2){
    var_theta=res$var
  }
  #theta=theta-mean(theta)
  iter=ncol(var_theta)
  x=theta
  y=rowMeans(var_theta[,-c(1:500)])
  x2=theta^2
  m<-coef(lm(y~x+x2))
  if(add==FALSE){
    if(points==TRUE){plot(x,y, ylab = ylab, xlab = xlab,ylim = ylim)
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

################################################################################
# LEGACY: Plotting hitting time values
################################################################################
plot_hitting_time=function(X,main){
  plot(X$true,hitting_time(X),ylab='Hitting time',xlab='True ability',main=main)
}

################################################################################
# LEGACY: Plotting bias 
################################################################################
plot_bias=function(X,main=NULL){
  plot(X$true,rowMeans(X$mean[,-c(1:500)])-X$true,ylab='Bias of Elo ratings',xlab='True ability',main=main)
}

################################################################################
# LEGACY: Plotting bias of the double ELO
################################################################################
plot_bias_double=function(X){
  mean_theta1=mapply(t=as.data.frame(X$mean[,,1]),d=as.data.frame(X$mean_delta[,,1]),FUN=function(t,d){t-mean(d)})
  mean_theta2=mapply(t=as.data.frame(X$mean[,,2]),d=as.data.frame(X$mean_delta[,,2]),FUN=function(t,d){t-mean(d)})
  mean_theta=(mean_theta1+mean_theta2)/2
  iter=ncol(mean_theta)
  mean_theta=rowMeans(mean_theta[,(iter-499):iter])
  plot(X$true,mean_theta-X$true,ylab='Bias of Elo ratings',xlab='True ability')
}  

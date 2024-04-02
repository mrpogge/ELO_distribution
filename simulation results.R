source("elo_functions.R")
source("plotting.R")
source("outcome_measures.R")

hitting_time=function(X){
  if(length(dim(X$mean))==3){
    X$mean=(X$mean[,,1]+X$mean[,,2])/2
  }
  iter=ncol(X$mean)
  mean_inv=rowMeans(X$mean[,(iter-499):iter])  
  ht=mapply(mean_inv=mean_inv,mean=as.data.frame(t(X$mean)),FUN=function(mean,mean_inv){min(which(abs(mean-mean_inv)<0.01))})
  return(ht)
}

plot_hitting_time=function(X,main){
  plot(X$true,hitting_time(X),ylab='Hitting time',xlab='True ability',main=main)
}

plot_bias=function(X,main=NULL){
  plot(X$true,rowMeans(X$mean[,-c(1:500)])-X$true,ylab='Bias of Elo ratings',xlab='True ability',main=main)
}

plot_bias_double=function(X){
  mean_theta1=mapply(t=as.data.frame(X$mean[,,1]),d=as.data.frame(X$mean_delta[,,1]),FUN=function(t,d){t-mean(d)})
  mean_theta2=mapply(t=as.data.frame(X$mean[,,2]),d=as.data.frame(X$mean_delta[,,2]),FUN=function(t,d){t-mean(d)})
  mean_theta=(mean_theta1+mean_theta2)/2
  iter=ncol(mean_theta)
  mean_theta=rowMeans(mean_theta[,(iter-499):iter])
  plot(X$true,mean_theta-X$true,ylab='Bias of Elo ratings',xlab='True ability')
}  

load("ResultsFixedAdaptive.RData")

vars_FA=sapply(res_fixed_adaptive,var_elo)
av_bias_FA=sapply(res_fixed_adaptive,FUN=function(X){mean(rowMeans(X$mean[,501:1000])-X$true)})
ht_FA=sapply(res_fixed_adaptive,FUN=function(X){mean(hitting_time(X))})

png('FA_K.png',width=1000)
par(mfrow=c(1,3))
plot(c(1:5)/10,av_bias_FA[c(2,3,1,4,5)],ylab='Average bias',xlab='K value',cex.lab=2,cex.main=2,pch=19)
plot(c(1:5)/10,vars_FA[c(2,3,1,4,5)],ylab='Average SD',xlab='K value',cex.lab=2,cex.main=2,pch=19)
plot(c(1:5)/10,ht_FA[c(2,3,1,4,5)],ylab='Average hitting time',xlab='K value',cex.lab=2,cex.main=2,pch=19)
dev.off()

png('FA_Adaptivity.png',width=1000)
par(mfrow=c(1,3))
plot(c(5:9)/10,av_bias_FA[c(6,7,1,8,9)],ylab='Average bias',xlab='Probability correct',cex.lab=2,cex.main=2,pch=19)
plot(c(5:9)/10,vars_FA[c(6,7,1,8,9)],ylab='Average SD',xlab='Probability correct',cex.lab=2,cex.main=2,pch=19)
plot(c(5:9)/10,ht_FA[c(6,7,1,8,9)],ylab='Average hitting time',xlab='Probability correct',cex.lab=2,cex.main=2,pch=19)
dev.off()

png('FA_traceplot.png',width=1000)
par(mfrow=c(1,1))
plot_elo(res_fixed_adaptive[[1]])
dev.off()

png('FA_variance.png',width=1000)
plot_elo_var(res_fixed_adaptive[[1]])
dev.off()

png('FA_bias.png',width=1000)
plot_bias(res_fixed_adaptive[[1]])
dev.off()


load("ResultsFixedNonadaptive.RData")

plot_elo(res_fixed_nonadaptive[[3]])
png('FN_variance.png',width=700)
plot_elo_var(res_fixed_nonadaptive[[3]])
dev.off()


png('FN_bias.png',width=700)
plot_bias(res_fixed_nonadaptive[[3]])
dev.off()


vars_FN=sapply(res_fixed_nonadaptive,var_elo)
av_abs_bias_FN=sapply(res_fixed_nonadaptive,FUN=function(X){mean(abs(rowMeans(X$mean[,501:1000])-X$true))})
ht_FN=sapply(res_fixed_nonadaptive,FUN=function(X){mean(hitting_time(X))})


png('FN_K.png',width=1000)
par(mfrow=c(1,3))
plot(c(1:5)/10,av_abs_bias_FN[1:5],ylab='Average absolute bias',xlab='K value',cex.lab=2,cex.main=2,pch=19)
plot(c(1:5)/10,vars_FN[1:5],ylab='Average bias',xlab='K value',cex.lab=2,cex.main=2,pch=19)
plot(c(1:5)/10,ht_FN[1:5],ylab='Average hitting time',xlab='K value',cex.lab=2,cex.main=2,pch=19)
dev.off()

load("ResultsNonadaptive.RData")

png('UN_traceplot.png',width=1000)
par(mfrow=c(1,1))

plot_elo(res_nonadaptive[[3]])
dev.off()
png('UN_variance.png',width=1000)

plot_elo_var(res_nonadaptive[[3]])
dev.off()
png('UN_bias.png',width=1000)

plot_bias(res_nonadaptive[[3]])
dev.off()


vars_N=sapply(res_nonadaptive,var_elo)
av_abs_bias_N=sapply(res_nonadaptive,FUN=function(X){
  mean_theta=mapply(t=as.data.frame(X$mean),d=as.data.frame(X$mean_delta),FUN=function(t,d){t-mean(d)})
  iter=ncol(mean_theta)
  mean_theta=rowMeans(mean_theta[,(iter-499):iter])
  mean(abs(mean_theta-X$true))
})
ht_N=sapply(res_nonadaptive,FUN=function(X){mean(hitting_time(X))})

png('UN_K.png',width=1000)
par(mfrow=c(1,3))
plot(c(1:5)/10,av_abs_bias_N[1:5],ylab='Average absolute bias',xlab='K value',cex.axis=2)
plot(c(1:5)/10,vars_N[1:5],ylab='Average variance',xlab='K value',cex.axis=2)
plot(c(1:5)/10,ht_N[1:5],ylab='Average hitting time',xlab='K value',cex.axis=2)
dev.off()

load("ResultsAdaptive.RData")


ht_A=sapply(res_adaptive,FUN=function(X){mean(hitting_time(X))})
vars_A=sapply(res_adaptive,var_elo)
av_abs_bias_A=sapply(res_adaptive,FUN=function(X){
  mean_theta1=mapply(t=as.data.frame(X$mean[,,1]),d=as.data.frame(X$mean_delta[,,1]),FUN=function(t,d){t-mean(d)})
  mean_theta2=mapply(t=as.data.frame(X$mean[,,2]),d=as.data.frame(X$mean_delta[,,2]),FUN=function(t,d){t-mean(d)})
  mean_theta=(mean_theta1+mean_theta2)/2
  iter=ncol(mean_theta)
  mean_theta=rowMeans(mean_theta[,(iter-499):iter])
  mean(abs(mean_theta-X$true))
  })

png('UA_K.png',width=1000)
par(mfrow=c(1,3))
plot(c(1:5)/10,av_abs_bias_A[c(2,3,1,4,5)],ylab='Average absolute bias',xlab='K value',cex.axis=2,cex.lab=2)
plot(c(1:5)/10,vars_A[c(2,3,1,4,5)],ylab='Average variance',xlab='K value',cex.axis=2,cex.lab=2)
plot(c(1:5)/10,ht_A[c(2,3,1,4,5)],ylab='Average hitting time',xlab='K value',cex.axis=2,cex.lab=2)
dev.off()

png('UA_adaptivity.png',width=1000)
par(mfrow=c(1,3))
plot(c(5:9)/10,av_abs_bias_A[c(6,7,1,8,9)],ylab='Average absolute bias',xlab='Probability correct',cex.axis=2,cex.lab=2)
plot(c(5:9)/10,vars_A[c(6,7,1,8,9)],ylab='Average variance',xlab='Probability correct',cex.axis=2,cex.lab=2)
plot(c(5:9)/10,ht_A[c(6,7,1,8,9)],ylab='Average hitting time',xlab='Probability correct',cex.axis=2,cex.lab=2)
dev.off()




png('UA_traceplot.png',width=1000)

par(mfrow=c(1,1))

plot_elo(res_adaptive[[1]])
dev.off()

png('UA_variance.png',width=1000)

plot_elo_var(res_adaptive[[1]])
dev.off()

png('UA_bias.png',width=1000)


plot_bias_double(res_adaptive[[1]])
dev.off()


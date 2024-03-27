library(tictoc)

plot_elo=function(res,add=FALSE,lwd=1){
	theta=res$true
	mean_theta=res$mean
	theta=theta-mean(theta)
	mean_theta=apply(res$mean,2,FUN=function(X){X-mean(X)})
	v=c(-2.25,-1.5,-0.75,0,0.75,1.5,2.25)
	P=c(which.min(abs(theta-v[1])))
	for(i in 2:length(v)){P=c(P,which.min(abs(theta-v[i])))}
	matplot(t(mean_theta[P,]),type='l',add=add,lty=1,lwd=lwd,col=c(1:7),xlab='Items answered',ylab='Average Elo rating')
	abline(h=theta[P],col=c(1:length(v)),lty=2)
}

elo=function(n,m,reps,games,K,adaptive=0,mP=0,sP=1,fixed_items=0,items_true=0,m_th=0,s_th=1,m_d=0,s_d=2){
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

theta = qnorm(seq(1/(n+1),n/(n+1),length=n),m_th,s_th)
theta=sample(theta,n)

delta = qnorm(seq(1/(m+1),m/(m+1),length=m),m_d,s_d)

A=-1/(2*sP^2)
B=mP/sP^2

#tic()

theta_hat=rep(0,n*reps)
delta_hat=rep(0,m*reps)

mean_theta<-var_theta<-rep(0,n*games)
	

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
		as.double(c(0:m)))

	mean_theta=matrix(tmp[[10]],ncol=games)
	var_theta=matrix(tmp[[11]],ncol=games)
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
}

res=list(true=theta,mean=mean_theta,var=var_theta)

toc()

return(res)
# the output is [[1]] - true values, [[2]] - means (across replications) of the Elo ratings of #each person at each iteration, [[3]] - variances (across replications) of the Elo ratings of #each person at each iteration
}

elo_double=function(n,m,reps,games,K,mP,sP,m_th=0,s_th=1,m_d=0,s_d=1){
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
theta=sample(theta,n)

delta = qnorm(seq(1/(m+1),m/(m+1),length=m),m_d,s_d)

A=-1/(2*sP^2)
B=mP/sP^2

theta_hat=rep(0,n*reps*2)
delta_hat=rep(0,m*reps*2)

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

}


plot_elo_double=function(res,add=FALSE,lwd=1){
theta=res$true
mean_theta=res$mean
theta=theta-mean(theta)
mean_theta1=apply(res$mean[,,1],2,FUN=function(X){X-mean(X)})
mean_theta2=apply(res$mean[,,2],2,FUN=function(X){X-mean(X)})
v=c(-2.25,-1.5,-0.75,0,0.75,1.5,2.25)
P=c(which.min(abs(theta-v[1])))
for(i in 2:length(v)){P=c(P,which.min(abs(theta-v[i])))}
matplot(t(mean_theta1[P,]),type='l',add=add,lty=1,lwd=lwd,col=c(1:7),xlab='Item pairs answered',ylab='Average Elo rating')
matplot(t(mean_theta2[P,]),type='l',add=TRUE,lty=1,lwd=lwd,col=c(1:7),xlab='Items answered',ylab='Average Elo rating')
abline(h=theta[P],col=c(1:length(v)),lty=2)
}

bias_correction=function(res){
theta=res$true
if(length(dim(res$mean))==3){
	mean_theta=(res$mean[,,1]+res$mean[,,2])/2
}
if(length(dim(res$mean))==2){
	mean_theta=res$mean
}
iter=ncol(mean_theta)
theta=theta-mean(theta)
mean_theta=rowMeans(mean_theta[,(1+iter/2):iter])
mean_theta=mean_theta-mean(mean_theta)
coef(lm(theta~0+mean_theta))
}

plot_elo_var=function(res,add=FALSE,points=TRUE,col=1, ylim = c(0,1), ylab = "y", xlab = "x"){
theta=res$true
if(length(dim(res$var))==3){
	var_theta=(res$var[,,1]+res$var[,,2])/2
}
if(length(dim(res$var))==2){
	var_theta=res$var
}
theta=theta-mean(theta)
iter=ncol(var_theta)
x=theta
y=rowMeans(var_theta[,-(1:500)])
x2=theta^2
m<-coef(lm(y~x+x2))
if(add==FALSE){
	if(points==TRUE){plot(x,y, ylim = ylim, ylab = ylab, xlab = xlab)
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
if(length(dim(res$var))==3){
	var_theta=(res$var[,,1]+res$var[,,2])/2
}
if(length(dim(res$var))==2){
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



elo_modifications=function(n,m,reps,games,K,mP=0,sP=0,m_th=0,s_th=1,m_d=0,s_d=2,type,LH=10){

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
		as.double(rep(0,m)))

	mean_theta=matrix(tmp[[10]],ncol=games)
	var_theta=matrix(tmp[[11]],ncol=games)

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
		as.integer(LH))

	mean_theta=matrix(tmp[[12]],ncol=games)
	var_theta=matrix(tmp[[13]],ncol=games)
}
	
res=list(true=theta,mean=mean_theta,var=var_theta)

return(res)

# the output is [[1]] - true values, [[2]] - means (across replications) of the Elo ratings of #each person at each iteration, [[3]] - variances (across replications) of the Elo ratings of #each person at each iteration

}


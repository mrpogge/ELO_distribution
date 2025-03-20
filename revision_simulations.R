#LOAD THE C code after compuling to you system
dyn.load("elo_for_simulations_revision.dll") # load additional code used for the extra analysis (elo vs. urnings comparison)

###############################################################

## This function runs the same simulation with parallel Elo as elo_double, but it additionally computes the reliability and the MSE 
#of the ratings on the [0,1] scale. This is needed to choose an urn size comparable to K=.3 for parallel Elo

elo_double_extras=function(n,
                    m,
                    reps,
                    games,
                    K,
                    mP,sP,
                    m_th=0,s_th=1,
                    m_d=0,s_d=1){
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
  
  # extra objects needed to compute reliability and MSE of the ratings on the [0,1] scale (and also reliability on the logit scale)
  true_logit=1/(1+exp(-theta)) # true values on the [0,1] scale
  mean_theta=mean(theta) # mean on the logit scale
  mean_true_logit=mean(true_logit) # mean on the [0,1] scale
  s3=sum((theta-mean_theta)^2) # sum of squares of the true values on the logit scale (needed to compute reliability)
  s3_logit=sum((true_logit-mean_true_logit)^2) # sum of squares of the true values on the [0,1] scale
  
  delta = qnorm(seq(1/(m+1),m/(m+1),length=m),m_d,s_d)
  
  A=-1/(2*sP^2)
  B=mP/sP^2
  
  theta_hat=rep(m_th,n*reps*2)
  delta_hat=rep(m_d,m*reps*2)
  
  mean_theta<-var_theta<-rep(0,n*games*2)
  mean_delta<-var_delta<-rep(0,m*games*2)
  
  tmp<-.C("elo_double_extras",
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
          as.double(mean_delta),
          #  below are all extra objects compared to elo_double function
          rel1=as.double(rep(0,games)),
          rel1_logit=as.double(rep(0,games)),
          rel2=as.double(rep(0,games)),
          rel2_logit=as.double(rep(0,games)),
          as.double(true_logit),
          as.double(mean_theta),
          as.double(mean_true_logit),
          as.double(s3),
          as.double(s3_logit)
  )
  
  extras=cbind(rel1=tmp$rel1,rel1_logit=tmp$rel1_logit,rel2=tmp$rel2,rel2_logit=tmp$rel2_logit) # additional output
  mean_theta=array(tmp[[10]],dim=c(n,games,2))
  var_theta=array(tmp[[11]],dim=c(n,games,2))
  mean_delta=array(tmp[[15]],dim=c(m,games,2))
  res=list(true=theta,mean=mean_theta,var=var_theta,mean_delta=mean_delta,extras=extras)
  
  return(res)
  
  # the output is [[1]] - true values of the persons, 
  # [[2]] - means (across replications) of the Elo ratings of #each person at each iteration, separately for the two parallel chains, 
  # [[3]] - variances (across replications) of the Elo ratings of #each person at each iteration, separately for the two parallel chains, 
  # [[4]] - means (across replications) of the Elo ratings of each item at each iteration, separately for the two parallel chains, 
  # [[5]] - extra output; 1 - mse per timepoint, 2 - correlation between ratings, 3 - squared correlation between a rating and the true value, 4 - same as 2 but on the logit scale, 5 - same as 5 but on the logit scale
}

################################################################################
# Simulation
################################################################################

# Repeat Elo Double simulation but now computing reliability and mse
double_elo_extras=elo_double_extras(n=1000,
                               m=200,
                               reps=500,
                               games=1100,
                               K=0.3,
                               mP=log(0.7/0.3),sP=0.5,
                               m_d=0,s_d=1.5,
                               m_th=log(0.7/0.3),s_th=1)


# compute reliabilities of the urnings for different urn sizes
p=1/(1+exp(-double_elo_extras$true))
nn=c(10:40)
r=rep(0,31)
for(i in nn){
for(j in 1:10000){
  r[i-9]=r[i-9]+cor(p,rbinom(1000,i,p))^2/10000
}
}

REL=mean(double_elo_extras$extras[601:1100,'rel2_logit'])
nn[which.min(abs(r-REL))]

################################################################################
# saving the result as a list
################################################################################
save(double_elo_extras, file = "ResultsDoubleEloExtras.RData")

# Run the urnings with n=32 to compare to the hitting times to parallel Elo
true=double_elo_extras$true
tic()

n_reps=500 # number of replications
n_students=1000 # number of students
n_items=200 # number of items 
n_games=2200 # number of timepoints
student_starting=rep(22,n_students*n_reps) #starting values
item_starting = rep(16,n_items*n_reps) 
Theta=rep(double_elo_extras$true,n_games) # true values (the same at each timepoint)
Delta=rep(qnorm(seq(1/(n_items+1),n_items/(n_items+1),length=n_items),0,1.5),n_games)
student_urn_size = 32
item_urn_size =32
m_adapt = log(0.7/0.3)
sd_adapt = 0.5

# Precompure the selection probabilities for each combination of urning values
Prob=matrix(1,nrow=student_urn_size+1,ncol=item_urn_size+1)
for(i in 0:(student_urn_size)){
  for(j in 0:(item_urn_size)){
    l=log((i+1)/(student_urn_size-i+1))-log((j+1)/(item_urn_size-j+1))
    Prob[i+1,j+1]=dnorm(l,m_adapt,sd_adapt)
  }
}  


################################################################################
#call the C function to perform the simulation
################################################################################
tic()
tmp=.C("urnings_simple_reps",
       as.integer(rep(student_starting)), #student starting values
       as.integer(rep(item_starting)), #item starting values
       as.double(Theta), #nplayers x niteration matrix of student true values
       as.double(Delta), #n_items x niteration matrix of item true values
       as.integer(n_students), #number of students
       as.integer(n_items), #number of items
       as.integer(n_games), #number of games
       as.integer(student_urn_size), # urn size for students
       as.integer(item_urn_size), #urn sizes for items
       as.double(Prob), #normal kernel matrix
       as.double(rep(0,n_items+1)), #no idea, but probably a helper for calculating the normalising constant
       as.integer(n_reps),
       rel=as.double(rep(0,n_games)),
       as.double(1/(1+exp(-Theta[1:n_students]))),
       as.double(mean(1/(1+exp(-Theta[1:n_students])))),
       as.double(sum((1/(1+exp(-Theta[1:n_students]))-mean(1/(1+exp(-Theta[1:n_students]))))^2)),
       U_mean=as.double(rep(0,n_students*n_games)),
       V_mean=as.double(rep(0,n_items*n_games)))

toc()
# get the means of the persons and the items at different timepoints
U_mean=matrix(tmp$U_mean,nrow=n_students)
V_mean=matrix(tmp$V_mean,nrow=n_items)

# transform back to the logit scale
U_mean=log(U_mean/(1-U_mean))
V_mean=log(V_mean/(1-V_mean))

# re-scale to set the mean of the items to 0
U_mean=apply(U_mean,1,FUN=function(X){X-colMeans(V_mean)})

save(tmp,file='elo_urnings_results500reps.RData')

# computing hitting times for the urnings
HT=apply(U_mean,1,FUN=function(X){abs(X-true)<0.01})
mean(apply(HT,1,FUN=function(X){which(X==1)[1]}))
[1] 456.097



# Repeat the simulation but now with 10000 replication instead of 500, to make the traceplots (like for Elo HTs are computed based on 500 replications to ensure comparability, but more replications are made to make traceplots to reduce noise)
# the rest of the simulation is the same as above

n_reps=10000
n_students=1000
n_items=200
n_games=2200
student_starting=rep(22,n_students*n_reps)
item_starting = rep(16,n_items*n_reps)
Theta=rep(double_elo_extras$true,n_games)
Delta=rep(qnorm(seq(1/(n_items+1),n_items/(n_items+1),length=n_items),0,1.5),n_games)
student_urn_size = 32
item_urn_size =32
m_adapt = log(0.7/0.3)
sd_adapt = 0.5

Prob=matrix(1,nrow=student_urn_size+1,ncol=item_urn_size+1)
for(i in 0:(student_urn_size)){
  for(j in 0:(item_urn_size)){
    l=log((i+1)/(student_urn_size-i+1))-log((j+1)/(item_urn_size-j+1))
    Prob[i+1,j+1]=dnorm(l,m_adapt,sd_adapt)
  }
}  


################################################################################
#call the C function to perform the simulation
################################################################################
tic()
tmp=.C("urnings_simple_reps",
       as.integer(rep(student_starting)), #student starting values
       as.integer(rep(item_starting)), #item starting values
       as.double(Theta), #nplayers x niteration matrix of student true values
       as.double(Delta), #n_items x niteration matrix of item true values
       as.integer(n_students), #number of students
       as.integer(n_items), #number of items
       as.integer(n_games), #number of games
       as.integer(student_urn_size), # urn size for students
       as.integer(item_urn_size), #urn sizes for items
       as.double(Prob), #normal kernel matrix
       as.double(rep(0,n_items+1)), #no idea, but probably a helper for calculating the normalising constant
       as.integer(n_reps),
       rel=as.double(rep(0,n_games)),
       as.double(1/(1+exp(-Theta[1:n_students]))),
       as.double(mean(1/(1+exp(-Theta[1:n_students])))),
       as.double(sum((1/(1+exp(-Theta[1:n_students]))-mean(1/(1+exp(-Theta[1:n_students]))))^2)),
       U_mean=as.double(rep(0,n_students*n_games)),
       V_mean=as.double(rep(0,n_items*n_games)))

true=tmp[[3]][1:n_students]

U_mean=matrix(tmp$U_mean,nrow=n_students)
V_mean=matrix(tmp$V_mean,nrow=n_items)

U_mean=log(U_mean/(1-U_mean))
V_mean=log(V_mean/(1-V_mean))

U_mean=apply(U_mean,1,FUN=function(X){X-colMeans(V_mean)})

v=c(mean(true)-2,mean(true)-1,mean(true),mean(true)+1,mean(true)+2)
for(i in 1:5){
  v[i]=which.min(abs(true-v[i]))
}

# save the traces for 5 persons (same as in the Elo simulations)
Urnings=data_frame(U_mean[,v])
save(Urnings,file='Urnings_plot.RData')


save(tmp,file='elo_urnings_results10000reps.RData')



# Repeat the simulation but now with the items and 80% of the persons starting from their invariant distributions

n_reps=500
n_students=1000

true=qnorm(seq(1/(n_students+1),n_students/(n_students+1),length=n_students),log(0.7/0.3),1)
P=sample(c(1:n_students),n_students)
true=true[P]

n_items=200

delta=qnorm(seq(1/(n_items+1),n_items/(n_items+1),length=n_items),0,1.5)


n_games=1000 # we use a smaller number of timepoints, because they are sufficient for all ratings to converge

new=sample(c(1:n_students),0.2*n_students)
student_starting=rbinom(n_students,32,1/(1+exp(-true)))
student_starting[new]=22 # only new persons start from the average
student_starting=rep(student_starting,n_reps)

item_starting=NULL
for(i in 1:n_reps){
  d=rbinom(n_items/2,32,1/(1+exp(-delta[1:100])))
  d=c(d,32-rev(d))
  item_starting=c(item_starting,d)
}

Theta=rep(true,n_games)
Delta=rep(delta,n_games)
student_urn_size = 32
item_urn_size =32
m_adapt = log(0.7/0.3)
sd_adapt = 0.5

Prob=matrix(1,nrow=student_urn_size+1,ncol=item_urn_size+1)
for(i in 0:(student_urn_size)){
  for(j in 0:(item_urn_size)){
    l=log((i+1)/(student_urn_size-i+1))-log((j+1)/(item_urn_size-j+1))
    Prob[i+1,j+1]=dnorm(l,m_adapt,sd_adapt)
  }
}  


################################################################################
#call the C function to perform the simulation
################################################################################
tic()
tmp_warm=.C("urnings_simple_reps",
       as.integer(rep(student_starting)), #student starting values
       as.integer(rep(item_starting)), #item starting values
       as.double(Theta), #nplayers x niteration matrix of student true values
       as.double(Delta), #n_items x niteration matrix of item true values
       as.integer(n_students), #number of students
       as.integer(n_items), #number of items
       as.integer(n_games), #number of games
       as.integer(student_urn_size), # urn size for students
       as.integer(item_urn_size), #urn sizes for items
       as.double(Prob), #normal kernel matrix
       as.double(rep(0,n_items+1)), #no idea, but probably a helper for calculating the normalising constant
       as.integer(n_reps),
       rel=as.double(rep(0,n_games)),
       as.double(1/(1+exp(-Theta[1:n_students]))),
       as.double(mean(1/(1+exp(-Theta[1:n_students])))),
       as.double(sum((1/(1+exp(-Theta[1:n_students]))-mean(1/(1+exp(-Theta[1:n_students]))))^2)),
       U_mean=as.double(rep(0,n_students*n_games)),
       V_mean=as.double(rep(0,n_items*n_games)))

save(tmp_warm,new,file='elo_urnings_results_warmstart.RData')

U_mean=matrix(tmp_warm$U_mean,nrow=n_students)
V_mean=matrix(tmp_warm$V_mean,nrow=n_items)

U_mean=log(U_mean/(1-U_mean))
V_mean=log(V_mean/(1-V_mean))

U_mean=apply(U_mean,1,FUN=function(X){X-colMeans(V_mean)})

HT=apply(U_mean,1,FUN=function(X){abs(X-true)<0.01})
mean(apply(HT,1,FUN=function(X){which(X==1)[1]})[new])

230.6

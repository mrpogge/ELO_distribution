load("output/ResultsVarianceInflation.RData")

par(mfrow=c(1,1))
png('variance_inflation.png',width=600,height=500)
theta=res_variance_inflation$true
v=c(-2,-1,0,1,2)+mean(theta)
P=c(which.min(abs(theta-v[1])))
for(i in 2:length(v)){P=c(P,which.min(abs(theta-v[i])))}
matplot((res_variance_inflation$exp_bias[1:1000,P]),type='l',lty=1,col=c(1:length(v)),xlab='Items answered',ylab='Expected error in item difficulty',cex.lab=2,cex.axis=2)
dev.off()


VI_data_wrangling = function(res, sim_type){
  #subsetting the result list and identifying the estimates
  theta = res$true
  ngames = nrow(res$exp_bias)
  
  #selecting 5 equally spaced persons
  v=c(-2,-1,0,1,2)+mean(theta)
  P=c(which.min(abs(theta-v[1])))
  for(i in 2:length(v)){P=c(P,which.min(abs(theta-v[i])))}
  dat = as.data.frame(res$exp_bias[1:1000,P]) 
  
  #preparing data for ggplotting
  dat = pivot_longer(dat, cols = everything(), names_to = "Variable", values_to = "Value")
  dat[,3] = rep(1:ngames, each = 5)
  dat[,4] = rep("est", times = 5*ngames)
  dat[,5] = rep(sim_type, times = 5*ngames)
  
  player_dat = as.data.frame(res$ebi[1:1000,P])
  player_dat = player_dat %>% pivot_longer(cols = everything(), names_to = "Variable", values_to = "Value")
  player_dat[,3] = rep(1:ngames, each = 5)
  player_dat[,4] = rep("true", times = 5*ngames)
  player_dat[,5] = rep(sim_type, times = 5*ngames)
  
  dat = rbind(dat, player_dat)
  lab1 = parse(text = paste("\u03B8", " = ", round(theta[P], 2)))
  dat$Variable = factor(dat$Variable, levels = c("V1", "V2", "V3", "V4", "V5"), labels = lab1)
  dat$...4 = factor(dat$...4, levels = c("est", "true"), labels = c("Elo Rating", "True Ability"))
  
  return(dat)
}

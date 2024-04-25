################################################################################
# LEGACY: trace plot for elo ratings for a few equally spaced students
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

#TODO: GGPLOT this!

################################################################################
# GGPLOTTING. traceplots (and the corresponding helper functions )
################################################################################

trace_data_wrangling = function(res, sim_type){
  #subsetting the result list and identifying the estimates
  mean_theta = res$mean
  theta = res$true
  ngames = ncol(res$mean)
  mean_theta=mapply(t=as.data.frame(res$mean),d=as.data.frame(res$mean_delta),FUN=function(t,d){t-mean(d)})
  
  #selecting 5 equally spaced persons
  v=c(-2,-1,0,1,2)+mean(theta)
  P=c(which.min(abs(theta-v[1])))
  for(i in 2:length(v)){P=c(P,which.min(abs(theta-v[i])))}
  dat = as.data.frame(t(mean_theta[P,])) 
  
  #preparing data for ggplotting
  dat = pivot_longer(dat, cols = everything(), names_to = "Variable", values_to = "Value")
  dat[,3] = rep(1:ngames, each = 5)
  dat[,4] = rep("est", times = 5*ngames)
  dat[,5] = rep(sim_type, times = 5*ngames)
  
  true_dat = as.data.frame(matrix(rep(theta[P], each = ngames), nrow = ngames, ncol = 5))
  true_dat = true_dat %>% pivot_longer(cols = everything(), names_to = "Variable", values_to = "Value")
  true_dat[,3] = rep(1:ngames, each = 5)
  true_dat[,4] = rep("true", times = 5*ngames)
  true_dat[,5] = rep(sim_type, times = 5*ngames)
  
  dat = rbind(dat, true_dat)
  lab1 = parse(text = paste("\u03B8", " = ", round(theta[P], 2)))
  dat$Variable = factor(dat$Variable, levels = c("V1", "V2", "V3", "V4", "V5"), labels = lab1)
  dat$...4 = factor(dat$...4, levels = c("est", "true"), labels = c("Elo Rating", "True Ability"))
  
  return(dat)
}

ggplot_elo = function(res_list, sim_types, nrow = 2, ncol = 2){
  
  dat = trace_data_wrangling(res_list[[1]], sim_types[1])
  for(i in 2:length(res_list)){
    dat = rbind(dat, trace_data_wrangling(res_list[[i]], sim_types[i]))
  }
  
  dat = as.data.frame(dat)
  colnames(dat) = c("variable", "value", "iteration", "true_est", "alg_type")
  
  plot_out = ggplot(dat, aes(x = iteration, y = value, color = variable, linetype = true_est)) +
    facet_wrap(~factor(alg_type, 
                       levels=c('fix-random',
                                'fix-adaptive',
                                'updated-random',
                                'updated-adaptive')), scales = "free_y", ncol = ncol,nrow = nrow) +
    geom_line() +
    labs(x = "Items Answered", y = expression(theta), color = "Student") +
    jtools::theme_apa(legend.font.size = 10)
  
  return(plot_out)
}

ggplot_elo_single = function(res){
  dat = trace_data_wrangling(res, "single")
  dat = as.data.frame(dat)
  colnames(dat) = c("variable", "value", "iteration", "true_est", "alg_type")
  plot_out = ggplot(dat, aes(x = iteration, y = value, color = variable, linetype = true_est)) +
    geom_line() +
    labs(x = "Items Answered", y = expression(theta), color = "Student") +
    jtools::theme_apa(legend.font.size = 10)
  
  return(plot_out)
}

################################################################################
# GGPLOTTING. variance plots (and the corresponding helper functions )
################################################################################
ggplot_elo_var_single = function(res){
  #wrangling
  x=res$true
  y=rowMeans(res$var[,-c(1:500)])
  dat = as.data.frame(cbind(x, y))
  
  #plotting
  plot_out = ggplot(dat, aes(x = x,y = y)) + 
    geom_point(alpha = 0.5) +
    geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = FALSE) + 
    labs(x = "\u03B8", y = paste0("Var(","\u03B8", ")")) +
    jtools::theme_apa(legend.font.size = 10)
  return(plot_out)
}

ggplot_elo_var = function(res_list, alg_type, smooth = TRUE){
  #wrangling
  x = res_list[[1]]$true
  y = rowMeans(res_list[[1]]$var[,-c(1:500)])
  dat = as.data.frame(cbind(x, y)) 
  nperson = nrow(dat)
  
  for(i in 2:length(res_list)){
    x = res_list[[i]]$true
    y = rowMeans(res_list[[i]]$var[,-c(1:500)])
    dat = rbind(dat, cbind(x,y))
  }
  
  dat = as.data.frame(cbind(dat, rep(alg_type, each = nperson)))
  colnames(dat) = c("x", "y", "alg_type")
  #plotting
  if(smooth == TRUE){
    plot_out = ggplot(dat, aes(x = x,y = y, color = alg_type)) + 
      geom_point(alpha = 0.5) +
      geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = FALSE) + 
      labs(x = "\u03B8", y = "Variance of Elo ratings") +
      jtools::theme_apa(legend.font.size = 10)
  } else {
    plot_out = ggplot(dat, aes(x = x,y = y, color = alg_type)) + 
      geom_point(alpha = 0.5) +
      labs(x = "\u03B8", y = "Variance of Elo ratings") +
      jtools::theme_apa(legend.font.size = 10)
  }

  return(plot_out)
}

################################################################################
# GGPLOTTING. bias plots
################################################################################
ggplot_bias_single = function(res){
  est = rowMeans(res$mean[,-c(1:500)])
  dat = data.frame("bias" = est - res$true, "true" = res$true)
  
  plot_out = ggplot(dat, aes(x = true, y = bias)) +
    geom_point(alpha = 0.5) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    labs(x = "\u03B8", y = "Bias of Elo ratings") +
    jtools::theme_apa(legend.font.size = 10)
 
  
  return(plot_out)
}

#TODO: double elo bias in ggplot!!
ggplot_bias = function(res_list, alg_type){
  est = rowMeans(res_list[[1]]$mean[,-c(1:500)])
  dat = data.frame("bias" = est - res_list[[1]]$true, "true" = res_list[[1]]$true)
  nperson = nrow(dat)
  
  for(i in 2:length(res_list)){
    est = rowMeans(res_list[[i]]$mean[,-c(1:500)])
    dat_temp = data.frame("bias" = est - res_list[[i]]$true, "true" = res_list[[1]]$true)
    dat = rbind(dat, dat_temp)
  }
  
  dat = as.data.frame(cbind(dat, rep(alg_type, each = nperson)))
  colnames(dat) = c("bias", "true", "alg_type")
  
  plot_out = ggplot(dat, aes(x = true, y = bias)) +
    facet_wrap(~factor(alg_type, 
                       levels=c('fix-random',
                                'fix-adaptive',
                                'updated-random'))) +
    geom_point(alpha = 0.5) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    labs(x = "\u03B8", y = "Bias of Elo ratings") +
    jtools::theme_apa(legend.font.size = 10)
  
  
  return(plot_out)
}


ggplot_box = function(res_K, measure_func, groups, lab_y, lab_x){
  var = measure_func(res_K[[1]])
  npersons = length(var)
  group = rep(groups[1], times = npersons)
  dat = cbind(var, group)
  
  for(i in 2:length(res_K)){
    var = measure_func(res_K[[i]])
    group = rep(groups[i], times = npersons)
    dat = rbind(dat, cbind(var, group))
  }
  
  dat = as.data.frame(dat)
  colnames(dat) = c("var", "k_value")
  
  plot_out = ggplot(dat, aes(x = k_value, y = var, group = k_value)) +
    geom_boxplot(outlier.shape = NA) + 
    labs(x = lab_x, y = lab_y) +
    jtools::theme_apa(legend.font.size = 10)
  
  return(plot_out)
} 

ggplot_point = function(res_K, measure_func, groups, lab_y, lab_x){
  var = measure_func(res_K[[1]])
  npersons = length(var)
  group = rep(groups[1], times = npersons)
  dat = cbind(var, group)
  
  for(i in 2:length(res_K)){
    var = measure_func(res_K[[i]])
    group = rep(groups[i], times = npersons)
    dat = rbind(dat, cbind(var, group))
  }
  
  dat = as.data.frame(dat)
  colnames(dat) = c("var", "group")
  
  dat = dat %>% group_by(group) %>% summarise(mean_group = mean(var, na.rm = TRUE))
  
  plot_out = ggplot(dat, aes(x = group, y = mean_group)) +
    geom_point() + 
    geom_line() +
    labs(x = lab_x, y = lab_y) +
    jtools::theme_apa(legend.font.size = 10)
  
  return(plot_out)
} 

ggplot_double_dots = function(res_K_list, measure_func, lab_y){
  groupsK = c(0.1,0.2,0.4,0.5,0.3)
  
  var1 = measure_func(res_K_list[[1]][[1]])
  var2 = measure_func(res_K_list[[2]][[1]])
  npersons = length(var)
  group1 = rep(groupsK[1], times = npersons)
  dat1 = cbind(var1, group1, rep(1, times = npersons))
  dat2 = cbind(var2, group1, rep(2, times = npersons))
  
  for(i in 2:5){
    var1 = measure_func(res_K_list[[1]][[i]])
    var2 = measure_func(res_K_list[[2]][[i]])
    group1 = rep(groupsK[i], times = npersons)
    dat1 = rbind(dat1, cbind(var1, group1, rep(1, times = npersons)))
    dat2 = rbind(dat2, cbind(var2, group1, rep(2, times = npersons)))
  }
  
  dat = as.data.frame(rbind(dat1, dat2))
  colnames(dat) = c("var", "groups", "type")
  
  dat$type = factor(dat$type, levels = c(1,2), labels = c("K", "Probability Correct"))
  
  dat = dat %>% 
    group_by(groups, type) %>% 
    summarise(mean_group = mean(var, na.rm = TRUE))
  
  print(dat)
  
  plot_out = ggplot(dat, aes(x = groups, y = mean_group, color = type)) +
    geom_point() + 
    geom_line(aes(color = type)) +
    scale_color_manual(values = c("red", "blue")) +
    scale_x_continuous(sec.axis = sec_axis(~.+0.4, name="Probability Correct")) +
    labs(x = "K", y = lab_y) + 
    guides(color = "none") +
    jtools::theme_apa() + 
    theme(
      axis.title.x = element_text(color = "blue"),
      axis.title.x.top = element_text(color = "red")
    )
  
  
  return(plot_out)
}




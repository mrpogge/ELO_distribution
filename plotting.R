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
                       levels=c('fixed-random',
                                'fixed-adaptive',
                                'updated-random',
                                'updated-adaptive')), scales = "free_y", ncol = ncol,nrow = nrow) +
    geom_line() +
    labs(x = "Items Answered", y = expression(theta), color = "Student") +
    jtools::theme_apa(legend.font.size = 15) +
    theme(axis.line = element_line(colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank()) 
  
  return(plot_out)
}

ggplot_elo_both = function(res_list, sim_types, nrow = 1, ncol = 2){
  
  dat = trace_data_wrangling(res_list[[1]], sim_types[1])
  for(i in 2:length(res_list)){
    dat = rbind(dat, trace_data_wrangling(res_list[[i]], sim_types[i]))
  }
  colnames(dat) = c("variable", "value", "iteration", "true_est", "alg_type")
  dat = as.data.frame(dat) %>% 
    mutate(double = ifelse(true_est == "True Ability", "True Ability", alg_type))
  
  
  plot_out = ggplot(dat, aes(x = iteration, y = value, color = variable, linetype = double)) +    geom_line() +
    labs(x = "Item Pairs Answered", y = expression(theta), color = "Student") +
    jtools::theme_apa(legend.font.size = 10)+
    theme(axis.line = element_line(colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank()) 
  
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

ggplot_elo_var = function(res_list, alg_type, smooth = TRUE, dELO = FALSE){
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
  
  colors = factor(rep(1:length(alg_type), each = nperson), levels = 1:length(alg_type), labels = alg_type)
  dat = as.data.frame(cbind(dat, colors))
  colnames(dat) = c("x", "y", "alg_type")
  #plotting
  if(smooth == TRUE){
    plot_out = ggplot(dat, aes(x = x,y = y, color = alg_type)) + 
      geom_point(alpha = 0.5) +
      geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = FALSE) + 
      labs(x = "True \u03B8", y = "Variance of Elo ratings") +
      jtools::theme_apa(legend.font.size = 10)+
      theme(axis.line = element_line(colour = "black"),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            panel.background = element_blank()) 
  } else if(dELO == FALSE) {
    plot_out = ggplot(dat, aes(x = x,y = y, color = alg_type)) + 
      geom_point(alpha = 0.5) +
      labs(x = "True \u03B8", y = "Variance of Elo ratings") +
      jtools::theme_apa(legend.font.size = 10)+
      theme(axis.line = element_line(colour = "black"),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            panel.background = element_blank()) 
  } else if(dELO == TRUE){
    dat = dat %>% mutate(alg_type = ifelse(alg_type == "double", "1", "2"))
    plot_out = ggplot(dat, aes(x = x,y = y, color = alg_type)) + 
      geom_point() +
      scale_alpha_identity() +
      scale_color_manual(values = c("mediumpurple4", "grey")) +
      labs(x = "True \u03B8", y = "Variance of Elo ratings") +
      jtools::theme_apa(legend.font.size = 10)+
      theme(axis.line = element_line(colour = "black"),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            panel.background = element_blank(),
            legend.position = "none") 
  }

  return(plot_out)
}

################################################################################
# GGPLOTTING. bias plots
################################################################################
ggplot_bias_single = function(res){
  est = res$mean[,-c(1:900)]
  corrected_est =mapply(t=as.data.frame(est),d=as.data.frame(res$mean_delta),FUN=function(t,d){t-mean(d)})
  corrected_est = rowMeans(corrected_est)
  dat = data.frame("bias" = corrected_est - res$true, "true" = res$true)
  
  plot_out = ggplot(dat, aes(x = true, y = bias)) +
    geom_point(alpha = 0.5) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    labs(x = "\u03B8", y = "Bias of Elo ratings") +
    jtools::theme_apa(legend.font.size = 10)+
    theme(axis.line = element_line(colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank()) 
 
  
  return(plot_out)
}

ggplot_bias = function(res_list, alg_type){
  est = res_list[[1]]$mean[,-c(1:900)]
  corrected_est =mapply(t=as.data.frame(est),d=as.data.frame(res_list[[1]]$mean_delta),FUN=function(t,d){t-mean(d)})
  corrected_est = rowMeans(corrected_est)
  dat = data.frame("bias" = corrected_est - res_list[[1]]$true, "true" = res_list[[1]]$true)
  nperson = nrow(dat)
  
  for(i in 2:length(res_list)){
    if(alg_type[i] != "double"){
      est = res_list[[i]]$mean[,-c(1:900)]
      corrected_est = mapply(t=as.data.frame(est),d=as.data.frame(res_list[[i]]$mean_delta),FUN=function(t,d){t-mean(d)})
      corrected_est = rowMeans(corrected_est)
      dat_temp = data.frame("bias" = corrected_est - res_list[[i]]$true, "true" = res_list[[i]]$true)
    } else {
      mean_theta1=mapply(t=as.data.frame(res_list[[i]]$mean[,,1]),d=as.data.frame(res_list[[i]]$mean_delta[,,1]),FUN=function(t,d){t-mean(d)})
      mean_theta2=mapply(t=as.data.frame(res_list[[i]]$mean[,,2]),d=as.data.frame(res_list[[i]]$mean_delta[,,2]),FUN=function(t,d){t-mean(d)})
      mean_theta=(mean_theta1+mean_theta2)/2
      iter=ncol(mean_theta)
      est = rowMeans(mean_theta[,(iter-899):iter])
      dat_temp = data.frame("bias" = est - res_list[[i]]$true, "true" = res_list[[i]]$true)
    }
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
    jtools::theme_apa(legend.font.size = 10)+
    theme(axis.line = element_line(colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank()) 
  
  
  return(plot_out)
}

ggplot_bias_colored = function(res_list, alg_type){
  est = res_list[[1]]$mean
  corrected_est =mapply(t=as.data.frame(est),d=as.data.frame(res_list[[1]]$mean_delta),FUN=function(t,d){t-mean(d)})
  corrected_est = rowMeans(corrected_est[,-c(1:900)])
  dat = data.frame("bias" = corrected_est - res_list[[1]]$true, "true" = res_list[[1]]$true)
  nperson = nrow(dat)
  
  for(i in 2:length(res_list)){
    if(alg_type[i] != "double"){
      est = res_list[[i]]$mean
      corrected_est = mapply(t=as.data.frame(est),d=as.data.frame(res_list[[i]]$mean_delta),FUN=function(t,d){t-mean(d)})
      corrected_est = rowMeans(corrected_est[,-c(1:900)])
      dat_temp = data.frame("bias" = corrected_est - res_list[[i]]$true, "true" = res_list[[i]]$true)
    } else {
      mean_theta1=mapply(t=as.data.frame(res_list[[i]]$mean[,,1]),d=as.data.frame(res_list[[i]]$mean_delta[,,1]),FUN=function(t,d){t-mean(d)})
      mean_theta2=mapply(t=as.data.frame(res_list[[i]]$mean[,,2]),d=as.data.frame(res_list[[i]]$mean_delta[,,2]),FUN=function(t,d){t-mean(d)})
      mean_theta=(mean_theta1+mean_theta2)/2
      iter=ncol(mean_theta)
      est = rowMeans(mean_theta[,-c(1:900)])
      dat_temp = data.frame("bias" = est - res_list[[i]]$true, "true" = res_list[[i]]$true)
    }
     dat = rbind(dat, dat_temp)
  }
  
  colors = factor(rep(1:length(alg_type), each = nperson), levels = 1:length(alg_type), labels = alg_type)
  dat = as.data.frame(cbind(dat, colors))
  colnames(dat) = c("bias", "true", "alg_type")
  
  if("double" %in% alg_type){
    dat = dat %>% mutate(alg_type = ifelse(alg_type == "double", "1", "2"))
    plot_out = ggplot(dat, aes(x = true, y = bias, color = alg_type)) +
      geom_point() +
      scale_alpha_identity() +
      geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
      labs(x = "True \u03B8", y = "Bias of Elo ratings") +
      scale_color_manual(values = c("mediumpurple4", "grey")) +
      jtools::theme_apa(legend.font.size = 10)+
      theme(axis.line = element_line(colour = "black"),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            panel.background = element_blank(),
            legend.position = "none") 
  } else {
    plot_out = ggplot(dat, aes(x = true, y = bias, color = alg_type)) +
      geom_point(alpha = 0.5) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
      labs(x = "True \u03B8", y = "Bias of Elo ratings") +
      jtools::theme_apa(legend.font.size = 10)+
      theme(axis.line = element_line(colour = "black"),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            panel.background = element_blank()) 
  }
  return(plot_out)
}

ggplot_bias_double_single = function(res){
  mean_theta1=mapply(t=as.data.frame(res$mean[,,1]),d=as.data.frame(res$mean_delta[,,1]),FUN=function(t,d){t-mean(d)})
  mean_theta2=mapply(t=as.data.frame(res$mean[,,2]),d=as.data.frame(res$mean_delta[,,2]),FUN=function(t,d){t-mean(d)})
  mean_theta=(mean_theta1+mean_theta2)/2
  iter=ncol(mean_theta)
  mean_theta=rowMeans(mean_theta[,(iter-899):iter])
  dat = data.frame("bias" =  mean_theta-res$true, "true" = res$true)
  
  plot_out = ggplot(dat, aes(x = true, y = bias)) +
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

ggplot_point = function(res_K, measure_func, groups, lab_y, lab_x, y_min, y_max){
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
  
  #TODO:generate a plot with points for mean groups connected by lines and make them
  #in a way that the y axis can be added as a parameter
  
  plot_out = ggplot(dat, aes(x = group, y = mean_group)) +
    geom_point() + 
    geom_line() +
    labs(x = lab_x, y = lab_y) + 
    scale_y_continuous(limits = c(y_min, y_max)) +
    jtools::theme_apa(legend.font.size = 10) +
    theme(axis.line = element_line(colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank()) 
  

  
  return(plot_out)
} 

#this takes multiple res_k as alist and and prints the mean of the measure_func for each group
#with different colours corresponding to a value in a vector called alg_type
ggplot_multiple_point = function(res_K_list, measure_func, groups, lab_y, lab_x, y_min, y_max, alg_type){
  var = measure_func(res_K_list[[1]][[1]])
  npersons = length(var)
  group = rep(groups[1], times = npersons)
  dat = cbind(var, group, alg_type[1])
  
  for(a in 2:length(res_K_list[[1]])){
    var = measure_func(res_K_list[[1]][[a]])
    group = rep(groups[a], times = npersons)
    dat = rbind(dat, cbind(var, group, alg_type[1]))
  }
  for(t in 2:length(res_K_list)){
    for(i in 1:length(res_K_list[[t]])){
      var = measure_func(res_K_list[[t]][[i]])
      group = rep(groups[i], times = npersons)
      dat = rbind(dat, cbind(var, group, rep(alg_type[t], times = npersons)))
    }
  }
  
  
  dat = as.data.frame(dat) %>% mutate(var = as.numeric(var))
  colnames(dat) = c("var", "group", "alg_type")
  
  dat$alg_type = factor(dat$alg_type, levels = c("fixed-random", "fixed-adaptive", "updated-random"))
  dat = dat %>% group_by(group, alg_type) %>% summarise(mean_group = mean(var, na.rm = TRUE))
  
  plot_out = ggplot(dat, aes(x = group, y = mean_group, color = alg_type, group = factor(alg_type))) +
    geom_point() + 
    geom_line() +
    labs(x = lab_x, y = lab_y) + 
    scale_y_continuous(limits = c(y_min, y_max)) +
#    scale_color_manual(values = c("#00BA38", "#F8766D","#619CFF")) +
    jtools::theme_apa(legend.font.size = 10)+
    theme(axis.line = element_line(colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank()) 
  
  return(plot_out)
}

ggplot_double_dots = function(res_K_list, measure_func, lab_y, ht = NULL, y_min, y_max){
  groupsK = c(0.1,0.2,0.3,0.4,0.5)
  

  if(!is.null(ht)){
    var1 = measure_func(res_K_list[[1]][[1]], ht[[1]][[1]])
    var2 = measure_func(res_K_list[[2]][[1]], ht[[2]][[1]])
  } else {
    var1 = measure_func(res_K_list[[1]][[1]])
    var2 = measure_func(res_K_list[[2]][[1]])
  }
  npersons = length(var)
  group1 = rep(groupsK[1], times = npersons)
  dat1 = cbind(var1, group1, rep(1, times = npersons))
  dat2 = cbind(var2, group1, rep(2, times = npersons))
  
  for(i in 2:5){
    if(!is.null(ht)){
      var1 = measure_func(res_K_list[[1]][[i]], ht[[1]][[i]])
      var2 = measure_func(res_K_list[[2]][[i]], ht[[2]][[i]])
    } else {
      var1 = measure_func(res_K_list[[1]][[i]])
      var2 = measure_func(res_K_list[[2]][[i]])
    }
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
  
  plot_out = ggplot(dat, aes(x = groups, y = mean_group, color = type, linetype = type)) +
    geom_point() + 
    geom_line(aes(color = type)) +
    scale_color_manual(values = c("mediumpurple4", "red")) +
    scale_x_continuous(sec.axis = sec_axis(~.+0.4, name="Probability Correct (dashed)")) +
    scale_y_continuous(limits = c(y_min, y_max)) +
    labs(x = "K (solid)", y = lab_y) + 
    guides(color = "none") +
    jtools::theme_apa() + 
    theme(
      axis.title.x = element_text(color = "mediumpurple4"),
      axis.title.x.top = element_text(color = "red")
    ) +
    theme(axis.line = element_line(colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank(),
          legend.position = "none")
  
  
  return(plot_out)
}




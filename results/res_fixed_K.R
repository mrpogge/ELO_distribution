source("elo_function.R")

##############################################################################
# Reading the files matching this criteria
##############################################################################
matching_files = paste0("output/",list.files("output", pattern = "^sim_fixed_K"))
output_list = lapply(matching_files, readRDS)

##############################################################################
# plotting
##############################################################################
#ploting mean changes compared to the true values
K=c(0.01,seq(0.05,0.5,by=0.05))

means = numeric(length(output_list))
for(i in 1:length(output_list)){
  plot_elo(output_list[[i]])
  means[i] = mean_elo(output_list[[i]])$bias
}

bias_correction(output_list[[4]])
plot(K, means)



#plotting variances 
vars = numeric(length(output_list))
for(i in 1:length(output_list)){
  plot_elo_var(output_list[[i]])
  vars[i] = var_elo(output_list[[i]])$median
}
plot(K, vars, ylab = "Median Variance")





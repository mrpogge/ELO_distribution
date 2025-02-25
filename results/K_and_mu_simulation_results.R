################################################################################
# dependencies
################################################################################
library(tidyverse)
library(patchwork)
source("elo_functions.R")
source("outcome_measures.R")
source("plotting.R")

################################################################################
# Loading sim results
################################################################################
load("output/ResultsDoubleElo_K.RData")
load("output/ResultsDoubleElo_mu.RData")

chain_mean_K = list()
chain_mean_mu = list()
for(i in 1:5){
  chain_mean_K[[i]] = list()
  chain_mean_mu[[i]] = list()
  for(j in 1:3){
    if(j == 1){
      chain_mean_K[[i]][[j]] =  res_double_elo_K[[i]]$true
      chain_mean_mu[[i]][[j]] =  res_double_elo_mu[[i]]$true
    } else {
      chain_mean_K[[i]][[j]] = apply(res_double_elo_K[[i]][[j]],1:2, mean)
      chain_mean_mu[[i]][[j]] = apply(res_double_elo_mu[[i]][[j]], 1:2, mean)
    }
  }
  names(chain_mean_K[[i]]) = names(chain_mean_mu[[i]]) = names(res_double_elo_K[[i]][1:3])
}


################################################################################
# setting up plots with double x axis
################################################################################
ggplot_point(res_double_elo_mu, abs_bias_revised, c(0.5,0.6,0.7,0.8,0.9), "mu", "h", 0,0.3)
res_K_list = list(chain_mean_K,chain_mean_mu)
ht_list = list(res_double_elo_K, res_double_elo_mu)

p_var = ggplot_double_dots(res_K_list, var_elo_vec, "Average Variance", y_min = 0, y_max = 0.32)
p_HT = ggplot_double_dots(ht_list, hitting_time_double, "Average Hitting Time", ht = ht_list, y_min = 0, y_max = 1600)
p_bias = ggplot_double_dots(ht_list, abs_bias_revised, "Average Absolute Bias", y_min = 0, y_max = 0.13)

patch_double_axis = p_bias + p_var + p_HT  + plot_layout(ncol = 3, nrow = 1, widths = c(5, 5, 5)) 
ggsave("figures/figure_14_K_mu.png", patch_double_axis, units = "mm", dpi = 300, height = 100, width = 200)

################################################################################
# average bias, ht and variance
################################################################################
baseline_500 = res_double_elo_K[[3]]

# average bias
print(abs_bias_revised(baseline_500))
print(var_elo(baseline_500))
print(mean(hitting_time_double(baseline_500,baseline_500)))

v = numeric(5)
for(i in 1:5){
  v[i] = (mean(hitting_time_double(res_double_elo_K[[i]],res_double_elo_K[[i]])))
}

plot(v, type = "l",ylim=c(0,800))


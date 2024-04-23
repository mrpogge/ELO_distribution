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
load("output/ResultsDoubleEloBaseline.RData")
#load the k value results
load("output/ResultsDoubleElo_K.RData")
load("output/ResultsDoubleElo_mu.RData")

res_double_elo_K[[5]] = double_elo_baseline
res_double_elo_mu[[5]] = double_elo_baseline

rm(double_elo_baseline)

for(i in 1:5){
  for(j in 2:4){
    res_double_elo_K[[i]][[j]] = apply(res_double_elo_K[[i]][[j]],1:2,mean)
    res_double_elo_mu[[i]][[j]] = apply(res_double_elo_mu[[i]][[j]],1:2,mean)
  }
}

################################################################################
# setting up plots with double x axis
################################################################################
res_K_list = list(res_double_elo_K,res_double_elo_mu)

p_var = ggplot_double_dots(res_K_list, var_elo_vec, "Variance")
p_HT = ggplot_double_dots(res_K_list, hitting_time, "Hitting Time")
p_bias = ggplot_double_dots(res_K_list, absolute_bias, "Absolute Bias")

patch_double_axis = p_bias + p_var + p_HT + plot_annotation(title = "double Elo" ,theme=theme(plot.title=element_text(hjust=0.5)))

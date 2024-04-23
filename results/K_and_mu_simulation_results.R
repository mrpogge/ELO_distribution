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
load("output/ResultsFixAdaptiveBaseline.RData")
#load the k value results
load("output/ResultsFixAdaptive_K.RData")

res_fix_items_adaptive_K[[5]] = fixed_items_adaptive_baseline

rm(fixed_items_adaptive_baseline)

################################################################################
# Loading sim results
################################################################################
load("output/ResultsFixAdaptiveBaseline.RData")

#load the k value results
load("output/ResultsFixAdaptive_mu.RData")


res_fix_items_adaptive_mu[[5]] = fixed_items_adaptive_baseline

rm(fixed_items_adaptive_baseline )

################################################################################
# setting up plots with double x axis
################################################################################
res_K_list = list(res_fix_items_adaptive_K,res_fix_items_adaptive_mu)
measure_func = var_elo_vec

p_var = ggplot_double_dots(res_K_list, var_elo_vec, "Variance")
p_HT = ggplot_double_dots(res_K_list, hitting_time, "Hitting Time")
p_bias = ggplot_double_dots(res_K_list, absolute_bias, "Absolute Bias")

patch_double_axis = p_bias + p_var + p_HT + plot_annotation(title = "fixed-adaptive" ,theme=theme(plot.title=element_text(hjust=0.5)))

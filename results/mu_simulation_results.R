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
load("output/UpdatedAdaptiveBaseline.RData")

#load the k value results
load("output/ResultsFixAdaptive_mu.RData")
load("output/ResultsUpdatedAdaptive_mu.RData")

res_fix_items_adaptive_mu[[5]] = fixed_items_adaptive_baseline
res_updated_items_adaptive_mu[[5]] = updated_items_adaptive_baseline

rm(fixed_items_adaptive_baseline,updated_items_adaptive_baseline )
################################################################################
# Calculating outcome measures K
################################################################################
p11_mu = ggplot_point(res_fix_items_adaptive_mu, absolute_bias,c(0.5,0.6,0.8,0.9,0.7), lab_y = "Absolute Bias", lab_x = "Probability Correct")
p12_mu = ggplot_point(res_fix_items_adaptive_mu, hitting_time, c(0.5,0.6,0.8,0.9,0.7), lab_y = "Hitting Time", lab_x = "Probability Correct")
p13_mu = ggplot_point(res_fix_items_adaptive_mu, var_elo_vec, c(0.5,0.6,0.8,0.9,0.7), lab_y = "Variance", lab_x = "Probability Correct")

p21_mu = ggplot_box(res_updated_items_adaptive_mu, absolute_bias,c(0.5,0.6,0.8,0.9,0.7), lab_y = "Absolute Bias", lab_x = "Probability Correct")
p22_mu = ggplot_box(res_updated_items_adaptive_mu, hitting_time, c(0.5,0.6,0.8,0.9,0.7), lab_y = "Hitting Time", lab_x = "Probability Correct")
p23_mu = ggplot_box(res_updated_items_adaptive_mu, var_elo_vec, c(0.5,0.6,0.8,0.9,0.7), lab_y = "Variance", lab_x = "Probability Correct")

patch1 = p11_mu + p12_mu + p13_mu
patch2 = p21_mu + p22_mu + p23_mu
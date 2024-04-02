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
load("output/ResultsFixRandomBaseline.RData")
load("output/ResultsUpdatedRandomBaseline.RData")
#load the k value results
load("output/ResultsFixRandom.RData")
load("output/ResultsUpdatedRandom.RData")
load("output/ResultsFixAdaptive_K.RData")
load("output/ResultsUpdatedAdaptive_K.RData")

res_fix_items_adaptive_K[[5]] = fixed_items_adaptive_baseline
res_updated_items_adaptive_K[[5]] = updated_items_adaptive_baseline
res_fix_items_random[[5]] = fixed_items_random_baseline
res_updated_items_random[[5]] = updated_items_random_baseline

rm(fixed_items_adaptive_baseline,
   updated_items_adaptive_baseline, 
   fixed_items_random_baseline, 
   updated_items_random_baseline)
################################################################################
# Calculating outcome measures K
################################################################################
p11 = ggplot_box(res_fix_items_adaptive_K, absolute_bias,c(0.1,0.2,0.4,0.5,0.3), lab_y = "Absolute Bias", lab_x = "K")
p12 = ggplot_box(res_fix_items_adaptive_K, hitting_time, c(0.1,0.2,0.4,0.5,0.3), lab_y = "Hitting Time", lab_x = "K")
p13 = ggplot_box(res_fix_items_adaptive_K, var_elo_vec, c(0.1,0.2,0.4,0.5,0.3), lab_y = "Variance", lab_x = "K")

p21 = ggplot_box(res_updated_items_adaptive_K, absolute_bias,c(0.1,0.2,0.4,0.5,0.3), lab_y = "Absolute Bias", lab_x = "K")
p22 = ggplot_box(res_updated_items_adaptive_K, hitting_time, c(0.1,0.2,0.4,0.5,0.3), lab_y = "Hitting Time", lab_x = "K")
p23 = ggplot_box(res_updated_items_adaptive_K, var_elo_vec, c(0.1,0.2,0.4,0.5,0.3), lab_y = "Variance", lab_x = "K")

p31 = ggplot_box(res_fix_items_random, absolute_bias,c(0.1,0.2,0.4,0.5,0.3), lab_y = "Absolute Bias", lab_x = "K")
p32 = ggplot_box(res_fix_items_random, hitting_time, c(0.1,0.2,0.4,0.5,0.3), lab_y = "Hitting Time", lab_x = "K")
p33 = ggplot_box(res_fix_items_random, var_elo_vec, c(0.1,0.2,0.4,0.5,0.3), lab_y = "Variance", lab_x = "K")

p41 = ggplot_box(res_updated_items_random, absolute_bias,c(0.1,0.2,0.4,0.5,0.3), lab_y = "Absolute Bias", lab_x = "K")
p42 = ggplot_box(res_updated_items_random, hitting_time, c(0.1,0.2,0.4,0.5,0.3), lab_y = "Hitting Time", lab_x = "K")
p43 = ggplot_box(res_updated_items_random, var_elo_vec, c(0.1,0.2,0.4,0.5,0.3), lab_y = "Variance", lab_x = "K")


patch1 = p11 + p12 + p13 
patch2 = p21 + p22 + p23
patch3 = p31 + p32 + p33 
patch4 = p41 + p42 + p43
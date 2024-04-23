################################################################################
# dependencies
################################################################################
library(tidyverse)
library(patchwork)
library(grid)
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
p11 = ggplot_point(res_fix_items_adaptive_K, absolute_bias,c(0.1,0.2,0.4,0.5,0.3), lab_y = "Average Absolute Bias", lab_x = "K")
p12 = ggplot_point(res_fix_items_adaptive_K, hitting_time, c(0.1,0.2,0.4,0.5,0.3), lab_y = "Average Hitting Time", lab_x = "K")
p13 = ggplot_point(res_fix_items_adaptive_K, var_elo_vec, c(0.1,0.2,0.4,0.5,0.3), lab_y = "Average Variance", lab_x = "K")

p21 = ggplot_point(res_updated_items_adaptive_K, absolute_bias,c(0.1,0.2,0.4,0.5,0.3), lab_y = "Average Absolute Bias", lab_x = "K")
p22 = ggplot_point(res_updated_items_adaptive_K, hitting_time, c(0.1,0.2,0.4,0.5,0.3), lab_y = "Average Hitting Time", lab_x = "K")
p23 = ggplot_point(res_updated_items_adaptive_K, var_elo_vec, c(0.1,0.2,0.4,0.5,0.3), lab_y = "Average Variance", lab_x = "K")

p31 = ggplot_point(res_fix_items_random, absolute_bias,c(0.1,0.2,0.4,0.5,0.3), lab_y = "Average Absolute Bias", lab_x = "K")
p32 = ggplot_point(res_fix_items_random, hitting_time, c(0.1,0.2,0.4,0.5,0.3), lab_y = "Average Hitting Time", lab_x = "K")
p33 = ggplot_point(res_fix_items_random, var_elo_vec, c(0.1,0.2,0.4,0.5,0.3), lab_y = "Average Variance", lab_x = "K")

p41 = ggplot_point(res_updated_items_random, absolute_bias,c(0.1,0.2,0.4,0.5,0.3), lab_y = "Average Absolute Bias", lab_x = "K")
p42 = ggplot_point(res_updated_items_random, hitting_time, c(0.1,0.2,0.4,0.5,0.3), lab_y = "Average Hitting Time", lab_x = "K")
p43 = ggplot_point(res_updated_items_random, var_elo_vec, c(0.1,0.2,0.4,0.5,0.3), lab_y = "Average Variance", lab_x = "K")

################################################################################
# Patches (need the other scripts objects !!!!)
################################################################################
patch_fr = p31 + p33 + p32 + plot_annotation(title = "fixed-random" ,theme=theme(plot.title=element_text(hjust=0.5)))
patch_ur = p41 + p43 + p42 + plot_annotation(title = "updated-random" ,theme=theme(plot.title=element_text(hjust=0.5)))
#patch_fa = p11 + p12 + p13 + p11_mu + p12_mu + p13_mu
#patch_ua = p21 + p22 + p23 + p21_mu + p22_mu + p23_mu
title1 <- grid::textGrob(label = "fixed_random")
title2 <- grid::textGrob(label = "fixed_adaptive")
title3 <- grid::textGrob(label = "updated_random")

combined_1 = (wrap_elements(panel = title1) / patch_fr) + plot_layout(heights = c(1, 40))
combined_2 = (wrap_elements(panel = title2) / patch_double_axis) + plot_layout(heights = c(1, 40))
combined_3 = (wrap_elements(panel = title3) / patch_ur) + plot_layout(heights = c(1, 40))

patch_K_and_mu = wrap_elements(combined_1) / wrap_elements(combined_2) / wrap_elements(combined_3)
patch_K_and_mu = patch_fr / patch_double_axis / patch_ur


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
#load the k value results
load("output/ResultsFixRandom.RData")
load("output/ResultsUpdatedRandom.RData")
load("output/ResultsFixAdaptive_K.RData")
load("output/ResultsUpdatedAdaptive_K.RData")

################################################################################
# Calculating outcome measures K
################################################################################
K_s = c(0.1,0.2, 0.3, 0.4,0.5)
p11 = ggplot_point(res_fix_items_adaptive_K, absolute_bias,K_s, 
                   lab_y = "Average Absolute Bias",lab_x = "K",
                   y_min = 0.009, y_max = 0.039)
p12 = ggplot_point(res_fix_items_adaptive_K, hitting_time, K_s,
                   lab_y = "Average Hitting Time", lab_x = "K",
                   y_min = 35, y_max = 260)
p13 = ggplot_point(res_fix_items_adaptive_K, var_elo_vec, K_s,
                   lab_y = "Average Variance", lab_x = "K",
                   y_min = 0.001, y_max = 0.3)

p21 = ggplot_point(res_updated_items_random, absolute_bias,K_s,
                   lab_y = "Average Absolute Bias", lab_x = "K",
                   y_min = 0.009, y_max = 0.039)
p22 = ggplot_point(res_updated_items_random, hitting_time, K_s,
                   lab_y = "Average Hitting Time", lab_x = "K",
                   y_min = 35, y_max = 260)
p23 = ggplot_point(res_updated_items_random, var_elo_vec, K_s,
                   lab_y = "Average Variance", lab_x = "K",
                   y_min = 0.001, y_max = 0.3)

p31 = ggplot_point(res_fix_items_random, absolute_bias,K_s,
                   lab_y = "Average Absolute Bias", lab_x = "K",
                   y_min = 0.009, y_max = 0.039)
p32 = ggplot_point(res_fix_items_random, hitting_time, K_s,
                   lab_y = "Average Hitting Time", lab_x = "K",
                   y_min = 35, y_max = 260)
p33 = ggplot_point(res_fix_items_random, var_elo_vec, K_s, 
                   lab_y = "Average Variance", lab_x = "K",
                   y_min = 0.001, y_max = 0.3)

ua_vec = numeric()
for(i in 1:length(res_updated_items_adaptive_K)){
  ht_l = hitting_time(res_updated_items_adaptive_K[[i]])
  ht_l[ht_l == Inf] = 1000
  mean_ht = mean(ht_l)
  ua_vec[i] = mean_ht
}
ur_vec = numeric()
for(i in 1:length(res_updated_items_random)){
  ht_l = hitting_time(res_updated_items_random[[i]])
  ht_l[ht_l == Inf] = 1000
  mean_ht = mean(ht_l)
  ur_vec[i] = mean_ht
}

ua_vec / ur_vec

################################################################################
#multiple lines plot
################################################################################
res_K_list = list(res_fix_items_random,res_fix_items_adaptive_K, res_updated_items_random)
algo_list = c("fixed-random", "fixed-adaptive", "updated-random")
p_multi_bias = ggplot_multiple_point(res_K_list, abs_bias_revised_single, K_s, algo_list,
                                  lab_y = "Average Absolute Bias", lab_x = "K",
                                  y_min = 0, y_max = 0.12)

p_multi_HT = ggplot_multiple_point(res_K_list, hitting_time, K_s, algo_list,
                                  lab_y = "Average Hitting Time", lab_x = "K",
                                  y_min = 0, y_max = 350)
p_multi_var = ggplot_multiple_point(res_K_list, var_elo_vec, K_s, algo_list,
                                  lab_y = "Average Variance", lab_x = "K",
                                  y_min = 0.05, y_max = 0.32)


################################################################################
# Patches (need the other scripts objects !!!!)
################################################################################
patch_fr = p31 + p33 + p32 
patch_fa = p11 + p13 + p12 
patch_ur = p31 + p33 + p32
title1 <- grid::textGrob(label = "fixed-random", rot = -90)
title2 <- grid::textGrob(label = "fixed-adaptive", rot = -90)
title3 <- grid::textGrob(label = "updated-random", rot = -90)

combined_1 = (patch_fr + wrap_elements(panel = title1)) + plot_layout(nrow = 1, ncol = 4, widths = c(5, 5, 5, 1))
combined_2 = (patch_fa + wrap_elements(panel = title2)) + plot_layout(nrow = 1, ncol = 4, widths = c(5, 5, 5, 1))
combined_3 = (patch_ur + wrap_elements(panel = title3)) + plot_layout(nrow = 1, ncol = 4, widths = c(5, 5, 5, 1))

patch_K = wrap_elements(combined_1) / wrap_elements(combined_2) / wrap_elements(combined_3)
patch_K = patch_fr / patch_fa / patch_ur

multi_patch_K = p_multi_bias + p_multi_var + p_multi_HT + plot_layout(nrow = 1, ncol = 3, guides = "collect") &
  theme(legend.position='bottom')

ggsave("figures/figure_5_K.png", multi_patch_K, units = "mm", dpi = 300, height = 100, width = 200)

ur = numeric(5)
for(i in 1:5){
  ur[i] = mean(hitting_time(res_updated_items_random[[i]]))
}



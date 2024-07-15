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
#load the k value results
load("output/ResultsFixAdaptive_mu.RData")

################################################################################
# Calculating outcome measures K
################################################################################
mu_s = c(0.5,0.6,0.7, 0.8,0.9)
p11_mu = ggplot_point(res_fix_items_adaptive_mu, abs_bias_revised_single,mu_s, lab_y = "Average Absolute Bias", lab_x = "Probability Correct", 
                      y_min = 0, y_max = 0.125)
p12_mu = ggplot_point(res_fix_items_adaptive_mu, hitting_time, mu_s, lab_y = "Average Hitting Time", lab_x = "Probability Correct",
                      y_min = 0, y_max = 350)
p13_mu = ggplot_point(res_fix_items_adaptive_mu, var_elo_vec, mu_s, lab_y = "Average Variance", lab_x = "Probability Correct",
                      y_min = 0.1525, y_max = 0.1555)

patch1 = p11_mu + p13_mu + p12_mu 
ggsave("figures/figure_6_mu.png", patch1, units = "mm", dpi = 300, height = 100, width = 200)

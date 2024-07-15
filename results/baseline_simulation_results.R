################################################################################
# dependencies
################################################################################
library(tidyverse)
library(patchwork)
source("elo_functions.R")
source("plotting.R")
source("outcome_measures.R")
################################################################################
# Loading sim results
################################################################################
load("output/ResultsFixRandomBaseline.RData")
load("output/ResultsFixAdaptiveBaseline.RData")
load("output/UpdatedAdaptiveBaseline.RData")
load("output/ResultsUpdatedRandomBaseline.RData")

################################################################################
# Traceplots
################################################################################
alg_type = c("fixed-random", "fixed-adaptive", "updated-random", "updated-adaptive")
res_list = list(fixed_items_random_baseline,
                fixed_items_adaptive_baseline,
                updated_items_random_baseline,
                updated_items_adaptive_baseline)

#version 1 (facet wrapped)
facet_wrapped_trace = ggplot_elo(res_list = res_list, alg_type)

#version 2 (separate plots)
fr_trace = ggplot_elo_single(fixed_items_random_baseline)
fa_trace = ggplot_elo_single(fixed_items_adaptive_baseline)
ur_trace = ggplot_elo_single(updated_items_random_baseline)
ua_trace = ggplot_elo_single(updated_items_adaptive_baseline)

#version 3 (no rating inflation)
alg_type_noinf = c("fixed-random", "fixed-adaptive", "updated-random")
res_list_noinf = list(fixed_items_random_baseline, 
                      fixed_items_adaptive_baseline,
                      updated_items_random_baseline)
ggplot_elo(res_list = res_list_noinf, alg_type_noinf, nrow = 1, ncol = 3)

ggsave("figures/figure_1_trace.png",facet_wrapped_trace,  units = "mm", dpi = 300 , height = 100, width = 200)


################################################################################
# Variance plots
################################################################################
#version 1 single plot multiple lines
ggplot_elo_var(res_list, alg_type)

#version 2 separate plots
fr_var = ggplot_elo_var_single(fixed_items_random_baseline)
fa_var = ggplot_elo_var_single(fixed_items_adaptive_baseline)
ur_var = ggplot_elo_var_single(updated_items_random_baseline)
ua_var = ggplot_elo_var_single(updated_items_adaptive_baseline)

#version 3 no invariance
alg_type_noinf = c("fixed-random", "fixed-adaptive", "updated-random")
res_list_noinf = list(fixed_items_random_baseline,
                      fixed_items_adaptive_baseline,
                      updated_items_random_baseline)
no_inflation_var = ggplot_elo_var(res_list_noinf, alg_type_noinf, smooth = FALSE)



################################################################################
# Bias plots
################################################################################
#version 1 one plot
ggplot_bias(res_list, alg_type)

#version 2 separate plots
fr_bias = ggplot_bias_single(fixed_items_random_baseline)
fa_bias = ggplot_bias_single(fixed_items_adaptive_baseline)
ur_bias = ggplot_bias_single(updated_items_random_baseline)
ua_bias = ggplot_bias_single(updated_items_adaptive_baseline)

# version 3 no variance inflation
no_inflation_bias = ggplot_bias_colored(res_list_noinf, alg_type_noinf)

p_bias_var = no_inflation_var + no_inflation_bias + plot_layout(nrow = 1, ncol = 2, guides = "collect")
p_bias_var

ggsave("figures/figure_2_3_bias_var.png", p_bias_var,  units = "mm", dpi = 300, height = 100, width = 200)
################################################################################
# Patched plots
################################################################################
patch_fr = fr_bias + fr_var
patch_fa = fa_bias + fa_var
patch_ur = ur_bias + ur_var

patch_total = patch_fr / patch_fa / patch_ur

################################################################################
# ht for the updated adaptive and the updated random
################################################################################
ua = mean(hitting_time(updated_items_adaptive_baseline))
ur = mean(hitting_time(updated_items_random_baseline))

print(ua/ur)

################################################################################
# read double elo results
################################################################################
load("output/ResultsDoubleEloBaseline.RData")

#merge the two chains of elo ratings in the results
#collapse the double_elo_baseline$mean based on the last dimension of the array
chain_one = list()
chain_two = list()
chain_mean = list()
for(i in 1:4){
  if(i == 1){
    chain_one$true = chain_two$true = chain_mean$true = double_elo_baseline$true
  } else {
    chain_one[[i]] = double_elo_baseline[[i]][,,1]
    chain_two[[i]] = double_elo_baseline[[i]][,,2]
    chain_mean[[i]] = apply(double_elo_baseline[[i]], 1:2, mean)
  }
}
names(chain_one) = names(chain_two) = names(chain_mean) = names(double_elo_baseline)

################################################################################
# traceplot
################################################################################
p1_double = ggplot_elo_single(chain_mean)

res_list_both = list(chain_one, chain_two)
alg_type_both = c("Elo rating 1", "Elo rating 2")
p1_double_both = ggplot_elo_both(res_list_both, alg_type_both)

ggsave("figures/figure_11_trace_parallel.png", p1_double_both, units = "mm", dpi = 300,height = 100, width = 200)
################################################################################
#plot variance
################################################################################
alg_type_double = c("fixed-random", "fixed-adaptive", "updated-random", "double")
res_double = list(fixed_items_random_baseline,
                  fixed_items_adaptive_baseline,
                  updated_items_random_baseline,
                  chain_mean)
p3_var = ggplot_elo_var(res_double, alg_type_double, smooth = FALSE, dELO = TRUE)

ggsave("figures/figure_13_var_parallel.png", p3_var, units = "mm", dpi = 600)

################################################################################
# plot bias
################################################################################
res_double = list(fixed_items_random_baseline,
                  fixed_items_adaptive_baseline,
                  updated_items_random_baseline,
                  double_elo_baseline)
pTotal_bias = ggplot_bias_colored(res_double, alg_type_double)

ggsave("figures/figure_12_bias_parallel.png", pTotal_bias, units = "mm", dpi = 600)


p_paralell_bias_var = p3_var + pTotal_bias + plot_layout(nrow = 1, ncol = 2, guides = "collect")
ggsave("figures/figure_12_13_bias_var_parallel.png", p_paralell_bias_var, units = "mm", dpi = 300, height = 100, width = 200)


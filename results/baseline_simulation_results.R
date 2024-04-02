################################################################################
# dependencies
################################################################################
library(tidyverse)
library(patchwork)
source("elo_functions.R")
source("plotting.R")

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
alg_type = c("fix random", "fix adaptive", "updated random", "updated adaptive")
res_list = list(fixed_items_random_baseline,
                fixed_items_adaptive_baseline,
                updated_items_random_baseline,
                updated_items_adaptive_baseline)

#version 1 (facet wrapped)
ggplot_elo(res_list = res_list, alg_type)

#version 2 (separate plots)
fr_trace = ggplot_elo_single(fixed_items_random_baseline)
fa_trace = ggplot_elo_single(fixed_items_adaptive_baseline)
ur_trace = ggplot_elo_single(updated_items_random_baseline)
ua_trace = ggplot_elo_single(updated_items_adaptive_baseline)

#version 3 (no rating inflation)
alg_type_noinf = c("fix random", "fix adaptive", "updated random")
res_list_noinf = list(fixed_items_random_baseline, 
                      fixed_items_adaptive_baseline,
                      updated_items_random_baseline)
ggplot_elo(res_list = res_list_noinf, alg_type_noinf, nrow = 1, ncol = 3)


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
res_list_noinf = list(fixed_items_random_baseline,
                      fixed_items_adaptive_baseline,
                      updated_items_random_baseline)
ggplot_elo_var(res_list_noinf, alg_type_noinf)

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
ggplot_bias(res_list_noinf, alg_type_noinf)

################################################################################
# Patched plots
################################################################################
patch_fr = fr_trace / (fr_var + fr_bias)
patch_fa = fa_trace / (fa_var + fa_bias)
patch_ur = ur_trace / (ur_var + ur_bias)
patch_ua = ua_trace / (ua_var + ua_bias)

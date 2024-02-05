source("elo functions.R")
################################################################################
# loading the corresponding results
################################################################################
load("output/ResultsFixedAdaptive.RData")
res_fixed_nonadaptive = res_fixed_nonadaptive[[3]]

load("output/ResultsFixedAdaptive_1.RData")
res_fixed_adaptive = res_fixed_adaptive[[3]]

load("output/ResultsNonadaptive.RData")
res_nonadaptive = res_nonadaptive[[3]]

load("output/ResultsDoubleElo_2.RData")

################################################################################
# plotting variance against theta and quadratic approx
################################################################################
plot_elo_var(res_fixed_nonadaptive, add = FALSE, points = TRUE, 
             ylim = c(0.14,0.18), ylab = "Variance", xlab = "True Ability")
plot_elo_var(res_fixed_adaptive, add = TRUE, points = TRUE, col = 2)
plot_elo_var(res_nonadaptive, add = TRUE, points = TRUE, col = 3)
plot_elo_var(res_double, add = TRUE, points = TRUE, col = 4)

################################################################################
# plotting each section per per person means
################################################################################
plot_elo(res_fixed_nonadaptive)
plot_elo(res_fixed_adaptive)
plot_elo(res_nonadaptive)
plot_elo(res_double)

################################################################################
# loading the corresponding results
################################################################################
load("output/ResultsAdaptive_3.RData")
res_adaptive = res_adaptive[[2]]
load("output/ResultsFixedAdaptive_3.RData")
res_fixed_adaptive = res_fixed_adaptive[[2]]
load("output/ResultsDoubleElo_3.RData")
res_double = res_double[[4]]

plot_elo(res_adaptive)
plot_elo(res_fixed_adaptive)
plot_elo(res_double)

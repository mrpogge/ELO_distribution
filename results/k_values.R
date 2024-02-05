source("elo functions.R")
################################################################################
# loading the corresponding results
################################################################################
load("output/ResultsFixedAdaptive.RData")

load("output/ResultsFixedAdaptive_1.RData")
res_fixed_adaptive_1 = res_fixed_adaptive
load("output/ResultsFixedAdaptive_2.RData")
res_fixed_adaptive = c(res_fixed_adaptive_1, res_fixed_adaptive)
load("output/ResultsNonadaptive.RData")

load("output/ResultsDoubleElo_1.RData")
res_double_1 = res_double
load("output/ResultsDoubleElo_2.RData")
res_double_2 = res_double
load("output/ResultsDoubleElo_3.RData")
res_double_3 = res_double

res_double_1[[4]] = res_double_1[[3]]
res_double_1[[3]] = res_double_2
res_double = c(res_double_1, res_double_3)
################################################################################
# calculating bias correction
################################################################################
bias_corrections = matrix(0, nrow = 4, ncol = 5)
for(i in 1:5){
  bias_corrections[1, i] = bias_correction(res_fixed_nonadaptive[[i]])
  bias_corrections[2, i] = bias_correction(res_fixed_adaptive[[i]])
  bias_corrections[3, i] = bias_correction(res_nonadaptive[[i]])
  bias_corrections[4, i] = bias_correction(res_double[[i]])
}

K_values = c(0.1,0.2,0.3,0.4,0.5)

plot(K_values, bias_corrections[1,], type = "b", ylim = c(0.87, 1.02), ylab = "Bias Correction", xlab = "K")
segments(0.1, bias_corrections[1,1], 0.5, bias_corrections[1,5], col = 1, lty = 2)
for(i in 2:4){
  lines(K_values, bias_corrections[i, ], type = "b", col = i)
  segments(0.1, bias_corrections[i,1], 0.5, bias_corrections[i,5], col = i, lty = 2)
}

################################################################################
# median variance
################################################################################
median_variance = matrix(0, nrow = 4, ncol = 5)
for(i in 1:5){
  median_variance[1, i] = var_elo(res_fixed_nonadaptive[[i]])$median
  median_variance[2, i] = var_elo(res_fixed_adaptive[[i]])$median
  median_variance[3, i] = var_elo(res_nonadaptive[[i]])$median
  median_variance[4, i] = var_elo(res_double[[i]])$median
}

plot(K_values, median_variance[1,], type = "b", ylim = c(0.05, 0.35), ylab = "Median Variance", xlab = "K")
segments(0.1, median_variance[1,1], 0.5, median_variance[1,5], col = 1, lty = 2)
for(i in 2:4){
  lines(K_values, median_variance[i, ], type = "b", col = i)
  segments(0.1, median_variance[i,1], 0.5, median_variance[i,5], col = i, lty = 2)
}

################################################################################
# rating variance vs iteration
################################################################################
rating_variance = matrix(0, nrow = 5, ncol = 1000)
for(i in 1:5){
  rating_variance[i,] = apply(res_fixed_adaptive[[i]]$mean, 1, mean)[1:1000]
}

plot(rating_variance[1,], type = "l")
lines(rating_variance[2,], col = 2)

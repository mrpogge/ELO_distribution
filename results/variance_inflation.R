source("elo functions.R")
################################################################################
# loading the corresponding results
################################################################################
load("output/ResultsAdaptive_1.RData")
res_adaptive_1 = res_adaptive
load("output/ResultsAdaptive_2.RData")
res_adaptive_2 = res_adaptive
load("output/ResultsAdaptive_3.RData")

res_adaptive = c(res_adaptive_1, res_adaptive_2, res_adaptive)
rm(res_adaptive_1, res_adaptive_2)

################################################################################
# rating variance vs iteration
################################################################################
rating_variance = matrix(0, nrow = 5, ncol = 1000)
for(i in 1:5){
  rating_variance[i,] = apply(res_adaptive[[i]]$mean, 2, var)[1:1000]
}

plot(rating_variance[1,], type = "l", ylim = c(0,40), ylab = "Variance of Mean Ratings", xlab = "Number of Games")
for(i in 2:4){
  lines(rating_variance[i,], col = i)
}

rating_variance_adapt = matrix(0, nrow = 3, ncol = 1000)
counter = 1
for(i in c(6,8,9)){
  rating_variance_adapt[counter,] = apply(res_adaptive[[i]]$mean, 2, var)[1:1000]
  counter = counter + 1
}

plot(rating_variance_adapt[1,], type = "l", ylim = c(0,40), ylab = "Variance of Mean Ratings", xlab = "Number of Games")
for(i in 2:3){
  lines(rating_variance_adapt[i,], col = i)
}

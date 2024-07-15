load("output/ResultsDoubleElo_mu.RData")
################################################################################
# dependencies
################################################################################
library(tidyverse)
library(patchwork)
source("elo_functions.R")
source("plotting.R")

for(cs in 1:length(res_double_elo_mu)){
  #merge the two chains of elo ratings in the results
  #collapse the double_elo_baseline$mean based on the last dimension of the array
  chain_one = list()
  chain_two = list()
  chain_mean = list()
  for(i in 1:4){
    if(i == 1){
      chain_one$true = chain_two$true = chain_mean$true = res_double_elo_mu[[cs]]$true
    } else {
      chain_one[[i]] = res_double_elo_mu[[cs]][[i]][,,1]
      chain_two[[i]] = res_double_elo_mu[[cs]][[i]][,,2]
      chain_mean[[i]] = apply(res_double_elo_mu[[cs]][[i]], 1:2, mean)
    }
  }
  names(chain_one) = names(chain_two) = names(chain_mean) = names(res_double_elo_mu[[cs]])
  
  ################################################################################
  # traceplot
  ################################################################################
  a = ggplot_elo_single(chain_mean)
  a

}

################################################################################
# Dependencies
################################################################################
library(tidyverse)
library(patchwork)
source("elo_functions.R")
source("plotting.R")

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
p1 = ggplot_elo_single(chain_one)
p2 = ggplot_elo_single(chain_two)
p3 = ggplot_elo_single(chain_mean)

p123 = ggplot_elo(list(chain_one, chain_two), c("chain 1", "chain 2"))

################################################################################
#plot variance
################################################################################
p1_var = ggplot_elo_var_single(chain_one)
p2_var = ggplot_elo_var_single(chain_two)
p3_var = ggplot_elo_var_single(chain_mean)

################################################################################
# plot bias
################################################################################
p1_bias = ggplot_bias_single(chain_one)
p2_bias = ggplot_bias_single(chain_two)
p3_bias = ggplot_bias_single(chain_mean)
#TODO: double elo ggplot for bias




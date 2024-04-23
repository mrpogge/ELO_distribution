################################################################################
# Dependencies
################################################################################
source('elo_functions.R')
source('simulations/fix_params.R')

################################################################################
# simulation
################################################################################
niters_dELO = c(2000,1300,1000,1000)
res_double_elo_K = list()

for(i in 1:length(K)){
  temp = elo_double(n=nstudents,
                   m=nitems,
                   reps=nreps,
                   games=niters_dELO[i],
                   K=K[i],
                   m_th = mu_theta_baseline,
                   s_th = sigma_theta,
                   m_d=mu_delta,
                   s_d=sigma_delta,
                   mP = mu_P_baseline,
                   sP = sigma_P)
  res_double_elo_K[[i]] = temp
}

################################################################################
# Saving the results as a list of lists
################################################################################
save(res_double_elo_K,file="output/ResultsDoubleElo_K.RData")
################################################################################
# Dependencies
################################################################################
source('elo_functions.R')
source('simulations/fix_params.R')

################################################################################
# simulation
################################################################################
niters_dELO = c(1000,1000,1100,1600)
res_double_elo_mu= list()

for(i in 1:length(mu_P)){
  temp = elo_double(n=nstudents,
                     m=nitems,
                     reps=nreps,
                     games=niters_dELO[i],
                     K=K_baseline,
                     m_th = mu_theta[i],
                     s_th = sigma_theta,
                     m_d=mu_delta,
                     s_d=sigma_delta,
                     mP = mu_P[i],
                     sP = sigma_P)
  res_double_elo_mu[[i]] = temp
}

################################################################################
# Saving the results as a list of lists
################################################################################
save(res_double_elo_mu,file="output/ResultsDoubleElo_mu.RData")
################################################################################
# Dependencies
################################################################################
source('elo functions.R')
source('simulations/fix_params.R')

################################################################################
# simulation
################################################################################
res_updated_items_adaptive_mu= list()
for(i in 1:length(mu_P)){
  temp = elo(n=nstudents,
             m=nitems,
             reps=nreps,
             games=ngames,
             K=K_baseline,
             m_th = mu_theta_baseline,
             s_th = sigma_theta,
             m_d=mu_delta,
             s_d=sigma_delta,
             adaptive = 1,
             mP = mu_P[i],
             sP = sigma_P,
             fixed_items=0,
             items_true=items_true)
  res_updated_items_adaptive_mu[[i]] = temp
}

################################################################################
# Saving the results as a list of lists
################################################################################
save(res_updated_items_adaptive_mu,file="output/ResultsUpdatedAdaptive_mu.RData")
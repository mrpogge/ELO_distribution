################################################################################
# Dependencies
################################################################################
source('elo functions.R')
source('simulations/fix_params.R')

################################################################################
# simulation
################################################################################
res_fix_items_random = list()
for(i in 1:length(K)){
  temp = elo(n=nstudents,
             m=nitems,
             reps=nreps,
             games=ngames,
             K=K[i],
             m_th = mu_theta_baseline,
             s_th = sigma_theta,
             m_d=mu_delta,
             s_d=sigma_delta,
             fixed_items=1, #fix items
             items_true=items_true)
  res_fix_items_random[[i]] = temp
}

################################################################################
# Saving the results as a list of lists
################################################################################
save(res_fix_items_random,file="output/ResultsFixRandom.RData")
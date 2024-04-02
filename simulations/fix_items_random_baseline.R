################################################################################
# Dependencies
################################################################################
source('elo functions.R')
source('simulations/fix_params.R')

################################################################################
# Simulation
################################################################################
fixed_items_random_baseline = elo(n=nstudents,
                                  m=nitems,
                                  reps=nreps_baseline,
                                  games=ngames,
                                  K=K_baseline,
                                  m_th = mu_theta_baseline,
                                  s_th = sigma_theta,
                                  m_d=mu_delta,
                                  s_d=sigma_delta,
                                  fixed_items=1, #fix items
                                  items_true=items_true)
################################################################################
# saving the result as a list
################################################################################
save(fixed_items_random_baseline, file = "output/ResultsFixRandomBaseline.RData")
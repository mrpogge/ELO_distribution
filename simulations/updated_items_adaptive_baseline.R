################################################################################
# Dependencies
################################################################################
source('elo functions.R')
source('simulations/fix_params.R')

################################################################################
# Simulation
################################################################################
updated_items_adaptive_baseline = elo(n=nstudents,
                                      m=nitems,
                                      reps=nreps_baseline,
                                      games=ngames,
                                      K=K_baseline,
                                      m_th = mu_theta_baseline,
                                      s_th = sigma_theta,
                                      m_d=mu_delta,
                                      s_d=sigma_delta,
                                      adaptive = 1,
                                      mP = mu_P_baseline,
                                      sP = sigma_P,
                                      fixed_items=0, #updated items
                                      items_true=items_true)
################################################################################
# saving the result as a list
################################################################################
save(updated_items_adaptive_baseline, file = "output/UpdatedAdaptiveBaseline.RData")
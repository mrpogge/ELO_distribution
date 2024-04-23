################################################################################
# Dependencies
################################################################################
source('elo_functions.R')
source('simulations/fix_params.R')

################################################################################
# Simulation
################################################################################
double_elo_baseline=elo_double(n=nstudents,
                               m=nitems,
                               reps=nreps_baseline,
                               games=1100,
                               K=K_baseline,
                               mP=mu_P_baseline,sP=sigma_P,
                               m_d=mu_delta,s_d=sigma_delta,
                               m_th=mu_theta_baseline,s_th=sigma_theta)

################################################################################
# saving the result as a list
################################################################################
save(double_elo_baseline, file = "output/ResultsDoubleEloBaseline.RData")
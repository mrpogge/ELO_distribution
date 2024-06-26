################################################################################
# Dependencies
################################################################################
source('elo_functions.R')

################################################################################
# Simulation
################################################################################
  K=0.3
  mP=m_th=log(0.7/0.3)
  res_lagged_baseline = elo_lag(n=1000,
                                m=200,
                                games=1500,
                                reps=500,
                                K=K,
                                mP=mP,
                                m_th=m_th,
                                LH=500)

# For the paper n=1000, m=200, 

################################################################################
# saving the result as a list
################################################################################
save(res_lagged_baseline, file = "output/ResultsEloLagBaseline.RData")
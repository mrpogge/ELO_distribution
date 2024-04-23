################################################################################
# Dependencies
################################################################################
source('elo_functions.R')

################################################################################
# simulation
################################################################################
res_elo_true = elo_true(n=500,
                         m=100,
                         reps=1000,
                         games=1500,
                         K=0.3,mP=log(0.7/0.3),m_th=log(0.7/0.3))
# For the paper we need n=500, m=100, games=1500 and reps=500. 
#If the first plot is too noisy with reps=500, we would need to try more reps (but 10000 is an overkill here)

################################################################################
# Saving the results as a list 
################################################################################
save(res_elo_true,file="output/ResultsEloTrue.RData")
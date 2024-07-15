################################################################################
# Fix parameters through the simulations
################################################################################

#number of students, items, games and repetitions of the systems
nstudents = 1000
nitems = 200
ngames = 1000
ngames_delo = 2000
nreps = 500
nreps_baseline = 10000

#latent parameters of the simulated systems 
mu_delta = 0
sigma_delta = 1.5

mu_theta_baseline = log(0.7/0.3)
mu_theta= c(0, log(0.6/0.4), log(0.7/0.3), log(0.8/0.2), log(0.9/0.1))
sigma_theta = 1


#step size parameters
K_baseline = 0.3
K = c(0.1, 0.2, 0.3, 0.4, 0.5)

#no error added 
items_true = 1

#adaptive item selection
mu_P_baseline = log(0.7/0.3)
mu_P= c(0, log(0.6/0.4), log(0.7/0.3), log(0.8/0.2), log(0.9/0.1))
sigma_P = 0.5

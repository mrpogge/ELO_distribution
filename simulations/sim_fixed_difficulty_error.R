##############################################################################
# Reading the base settings and the functions to run ELO
##############################################################################
source("simulations/base_settings.R")
source("elo_function.R")

set.seed(seed_vector[1])

##############################################################################
# fixed parameters
##############################################################################
adaptive=0 #non-adaptive
mP = sP = 999 #placeholder for the normal kernel params
fixed_items=1 #fixed items 
items_true=0 #error is added to the items
m_d=0 #mean of the item difficulty distribution is 0
s_d=1 #the standard deviation of the item difficulty distribution is 1
K = 0.25

##############################################################################
# running the simulations
##############################################################################
res = elo(n,
            m,
            reps,
            games,
            K,
            adaptive, mP, sP,
            fixed_items, items_true,
            save_replications,
            m_th,
            s_th,
            m_d,
            s_d,
            OS = OS)

res$pars = list("adaptive" = adaptive,
                "fixed_items" = fixed_items,
                "items_true" = items_true,
                "m_d" = m_d,
                "s_d" = s_d,
                "K" = K)

##############################################################################
# saving the results
##############################################################################
saveRDS(res, "output/sim_fixed_difficulty_error.rds")

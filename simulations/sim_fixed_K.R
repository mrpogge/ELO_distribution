##############################################################################
# Reading the base settings and the functions to run ELO
##############################################################################
source("simulations/base_settings.R")
source("elo_function.R")

##############################################################################
# fixed parameters
##############################################################################
adaptive=0 #non-adaptive
mP = sP = 999 #placeholder for the normal kernel params
fixed_items=1 #fixed items 
items_true=1 #error is added to the items
m_d=0 #mean of the item difficulty distribution is 0
s_d=1 #the standard deviation of the item difficulty distribution is 1

##############################################################################
# changing parameters
##############################################################################
# 10 different K values
K=c(0.01,seq(0.05,0.5,by=0.05))

##############################################################################
# running the simulations
##############################################################################
counter = 1
for(iK in K){
  print(paste0("We are running K = ", iK, ", remaining iterations =  ", length(K)-counter))
  res = elo(n,
            m,
            reps,
            games,
            iK,#this is what we are iterating
            adaptive, mP, sP,
            fixed_items, items_true,
            save_replications,
            m_th,
            s_th,
            m_d,
            s_d,
            OS = OS)
  curr_pars = list("adaptive" = adaptive,
                   "fixed_items" = fixed_items,
                   "items_true" = items_true,
                   "m_d" = m_d,
                   "s_d" = s_d,
                   "K" = iK)
  res$pars = curr_pars

  counter = counter + 1
  ##############################################################################
  # saving the results
  ##############################################################################
  saveRDS(res, paste0("output/sim_fixed_K_",iK,".rds"))
}



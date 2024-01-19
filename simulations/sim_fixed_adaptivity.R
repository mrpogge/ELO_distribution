##############################################################################
# Reading the base settings and the functions to run ELO
##############################################################################
source("simulations/base_settings.R")
source("elo_function.R")

##############################################################################
# fixed parameters
##############################################################################
adaptive=1 #adaptive
fixed_items=1 #fixed items 
items_true=1 #error is added to the items
m_d = 0
s_d = 1
K=0.25

##############################################################################
# changing parameters
##############################################################################
# 3 kernels
adapt_props = matrix(0, nrow = 3, ncol = 2)
adapt_props[1, ] = c(0.5,0.1) #N(0.5,0.1)
adapt_props[2, ] = c(0.75,0.1) #N(0.75,0.1)
adapt_props[3, ] = c(0.5,0.05) #N(0.5,0.05)


adapt_names = c("N(0.5,0.1)", "N(0.75,0.1)", "N(0.5,0.05)")


##############################################################################
# running the simulations
##############################################################################
counter = 1
for(iA in 1:nrow(adapt_props)){
  print(paste0("We are running kernel = ", adapt_names[iA] ,
               ", remaining iterations =  ", nrow(adapt_props)-counter))
  res = elo(n,
            m,
            reps,
            games,
            K,
            adaptive, adapt_props[iA,1], adapt_props[iA,2],#we are iterating these
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
                  "K" = K,
                  "mP" = adapt_props[iA,1],
                  "sP" = adapt_props[iA, 2])

  ##############################################################################
  # saving the results
  ##############################################################################
  saveRDS(res, paste0("output/sim_fixed_adaptivity_",adapt_names[iA],".rds"))
  counter = counter + 1
}



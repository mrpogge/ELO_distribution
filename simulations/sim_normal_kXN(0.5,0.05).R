##############################################################################
# Reading the base settings and the functions to run ELO
##############################################################################
source("simulations/base_settings.R")
source("elo_function.R")

##############################################################################
# fixed parameters
##############################################################################
adaptive=1 #adaptive
fixed_items=0 #non-fixed items 
items_true=0 #error is added to the items
m_d = 0
s_d = 1

##############################################################################
# changing parameters
##############################################################################
#11 K values
K = c(seq(0.05,0.5,by=0.05))
# 3 kernels
adapt_props = matrix(0, nrow = 3, ncol = 2)
adapt_props[1, ] = c(0.5,0.1) #N(0.5,0.1)
adapt_props[2, ] = c(0.75,0.1) #N(0.75,0.1)
adapt_props[3, ] = c(0.5,0.05) #N(0.5,0.05)

adapt_names = c("N(0.5,0.1)", "N(0.75,0.1)", "N(0.5,0.05)")


##############################################################################
# running the simulations
##############################################################################
iA = 3
counter = 1
for(iK in K){
  set.seed(seed_vector[counter])
  print(paste0("We are running kernel = ", adapt_names[iA] ,
               " with K = ", iK,
               ", remaining iterations =  ", (nrow(adapt_props) * length(K))-counter))
  res = elo(n,
            m,
            reps,
            games,
            iK, #we are iterating this
            adaptive, adapt_props[iA,1], adapt_props[iA,2], #we are iterating these
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
                  "K" = iK,
                  "mP" = adapt_props[iA,1],
                  "sP" = adapt_props[iA, 2])
  
  ##############################################################################
  # saving the results
  ##############################################################################
  saveRDS(res, paste0("output/sim_normal_kXadaptivity_",adapt_names[iA],"_",iK,".rds"))
  counter = counter + 1
}



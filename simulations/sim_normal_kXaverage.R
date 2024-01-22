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
fixed_items=0 #non-fixed items 
items_true=0 

##############################################################################
# changing parameters
##############################################################################
#11 K values
K = c(seq(0.05,0.5,by=0.05))
# 3 different item pools
item_props = matrix(0, nrow = 4, ncol = 2)
item_props[1, ] = c(0,1) #difficult 
item_props[2, ] = c(2,1) #difficult 
item_props[3, ] = c(-2,1) #easy
item_props[4, ] = c(0,2) #more variance
item_pool_names = c("average","difficult", "easy", "more_variance")

##############################################################################
# running the simulations
##############################################################################
counter = 1
iP = 1
for(iK in K){
  set.seed(seed_vector[counter])
    print(paste0("We are running item pool = ", item_pool_names[iP] ,
                 " with K = ", iK,
                 ", remaining iterations =  ", (nrow(item_props) * length(K))-counter))
    res = elo(n,
              m,
              reps,
              games,
              iK, #we are iterating this
              adaptive, mP, sP,
              fixed_items, items_true,
              save_replications,
              m_th,
              s_th,
              item_props[iP,1], #we are iterating this
              item_props[iP,2], #we are iterating this
              OS = OS)
    
    curr_pars = list("adaptive" = adaptive,
                     "fixed_items" = fixed_items,
                     "items_true" = items_true,
                     "m_d" = item_props[iP,1],
                     "s_d" = item_props[iP,2],
                     "K" = iK)
    res$pars = curr_pars
    counter = counter + 1
    ##############################################################################
    # saving the results
    ##############################################################################
    saveRDS(res, paste0("output/sim_normal_kXitem_pool_",item_pool_names[iP],"_",iK,".rds"))
  }




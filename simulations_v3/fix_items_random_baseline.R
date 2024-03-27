source('elo functions.R')
source('simulations_v3/fix_params.R')

fixed_items_random_baseline = elo(n=nstudents,
                                  m=nitems,
                                  reps=,
                                  games=ngames,
                                  K=K_baseline,
                                  m_d=mu_delta,
                                  s_d=sigma_delta,
                                  fixed_items=1, #fix items
                                  items_true=items_true)
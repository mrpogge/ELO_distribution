source('elo functions.R')
source('simulations_v3/fix_params.R')

fixed_items_random_baseline = elo(n=nstudents,
                                  m=nitems,
                                  reps=10,
                                  games=ngames,
                                  K=K_baseline,
                                  m_d=mu_delta,
                                  s_d=s_d,
                                  fixed_items=1,
                                  items_true=true_items)
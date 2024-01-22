library(microbenchmark)

source("elo_function.R")


##############################################################################
# #different values for rep
##############################################################################
#setting up the values
n=500
m = 100
reps_vec = c(2, 10, 20, 30, 40)
games = 50
K = 0.25
adaptive = 0
mP = sP = 999
fixed_items = 1
items_true = 1
save_replications = 0
m_th = m_d =  0
s_th = s_d =  1
OS = "MAC"

bench_reps = microbenchmark(times = 10, 
               elo(n,
                   m,
                   reps_vec[1],#this is what we are iterating
                   games,
                   K,
                   adaptive, mP, sP,
                   fixed_items, items_true,
                   save_replications,
                   m_th,
                   s_th,
                   m_d,
                   s_d,
                   OS = OS), 
               elo(n,
                   m,
                   reps_vec[2],#this is what we are iterating
                   games,
                   K,
                   adaptive, mP, sP,
                   fixed_items, items_true,
                   save_replications,
                   m_th,
                   s_th,
                   m_d,
                   s_d,
                   OS = OS),
               elo(n,
                   m,
                   reps_vec[3],#this is what we are iterating
                   games,
                   K,
                   adaptive, mP, sP,
                   fixed_items, items_true,
                   save_replications,
                   m_th,
                   s_th,
                   m_d,
                   s_d,
                   OS = OS),
               elo(n,
                   m,
                   reps_vec[4],#this is what we are iterating
                   games,
                   K,
                   adaptive, mP, sP,
                   fixed_items, items_true,
                   save_replications,
                   m_th,
                   s_th,
                   m_d,
                   s_d,
                   OS = OS),
               elo(n,
                   m,
                   reps_vec[5],#this is what we are iterating
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
                   )

print(bench_reps)
boxplot(bench_reps, log = FALSE, xlab = reps_vec)
##############################################################################
# #different values for adaptive
##############################################################################
#setting up the values
n=500
m = 100
reps = 10
games = 50
K = 0.25
adaptive_vec = c(0,1)
mP = 0.5
sP = 0.1
fixed_items = 1
items_true = 1
save_replications = 0
m_th = m_d =  0
s_th = s_d =  1
OS = "MAC"

bench_adapt = microbenchmark(times = 10, 
                            elo(n,
                                m,
                                reps,#this is what we are iterating
                                games,
                                K,
                                adaptive_vec[1], mP, sP,
                                fixed_items, items_true,
                                save_replications,
                                m_th,
                                s_th,
                                m_d,
                                s_d,
                                OS = OS), 
                            elo(n,
                                m,
                                reps,#this is what we are iterating
                                games,
                                K,
                                adaptive_vec[2], mP, sP,
                                fixed_items, items_true,
                                save_replications,
                                m_th,
                                s_th,
                                m_d,
                                s_d,
                                OS = OS))


print(bench_adapt)
boxplot(bench_adapt, log = FALSE, xlab = reps_vec)

##############################################################################
# #different values for game
##############################################################################
#setting up the values
n=500
m = 100
reps = 10
games = c(10,20,40,80,160)
K = 0.25
adaptive = 0
mP = 0.5
sP = 0.1
fixed_items = 1
items_true = 1
save_replications = 0
m_th = m_d =  0
s_th = s_d =  1
OS = "MAC"

bench_game = microbenchmark(times = 10, 
                             elo(n,
                                 m,
                                 reps,#this is what we are iterating
                                 games[1],
                                 K,
                                 adaptive, mP, sP,
                                 fixed_items, items_true,
                                 save_replications,
                                 m_th,
                                 s_th,
                                 m_d,
                                 s_d,
                                 OS = OS), 
                             elo(n,
                                 m,
                                 reps,#this is what we are iterating
                                 games[2],
                                 K,
                                 adaptive, mP, sP,
                                 fixed_items, items_true,
                                 save_replications,
                                 m_th,
                                 s_th,
                                 m_d,
                                 s_d,
                                 OS = OS),
                             elo(n,
                                 m,
                                 reps,#this is what we are iterating
                                 games[3],
                                 K,
                                 adaptive, mP, sP,
                                 fixed_items, items_true,
                                 save_replications,
                                 m_th,
                                 s_th,
                                 m_d,
                                 s_d,
                                 OS = OS),
                             elo(n,
                                 m,
                                 reps,#this is what we are iterating
                                 games[4],
                                 K,
                                 adaptive, mP, sP,
                                 fixed_items, items_true,
                                 save_replications,
                                 m_th,
                                 s_th,
                                 m_d,
                                 s_d,
                                 OS = OS),
                             elo(n,
                                 m,
                                 reps,#this is what we are iterating
                                 games[5],
                                 K,
                                 adaptive, mP, sP,
                                 fixed_items, items_true,
                                 save_replications,
                                 m_th,
                                 s_th,
                                 m_d,
                                 s_d,
                                 OS = OS))


print(bench_game)
boxplot(bench_game, log = FALSE, xlab = games)

##############################################################################
# #different values for n
##############################################################################
#setting up the values
n=c(500,1000,2000)
m = 100
reps_vec = 10
games = 50
K = 0.25
adaptive = 0
mP = sP = 999
fixed_items = 1
items_true = 1
save_replications = 0
m_th = m_d =  0
s_th = s_d =  1
OS = "MAC"

bench_n = microbenchmark(times = 10, 
                            elo(n[1],
                                m,
                                reps,#this is what we are iterating
                                games,
                                K,
                                adaptive, mP, sP,
                                fixed_items, items_true,
                                save_replications,
                                m_th,
                                s_th,
                                m_d,
                                s_d,
                                OS = OS), 
                            elo(n[2],
                                m,
                                reps,#this is what we are iterating
                                games,
                                K,
                                adaptive, mP, sP,
                                fixed_items, items_true,
                                save_replications,
                                m_th,
                                s_th,
                                m_d,
                                s_d,
                                OS = OS),
                            elo(n[3],
                                m,
                                reps,#this is what we are iterating
                                games,
                                K,
                                adaptive, mP, sP,
                                fixed_items, items_true,
                                save_replications,
                                m_th,
                                s_th,
                                m_d,
                                s_d,
                                OS = OS))
print(bench_n)
boxplot(bench_n, log = FALSE, xlab = games)

##############################################################################
# #different values for m and adapt
##############################################################################
#setting up the values
n=500
m = c(50, 100, 200, 400)
reps_vec = 10
games = 50
K = 0.25
adaptive = 0
mP = 0.5
sP = 0.1
fixed_items = 1
items_true = 1
save_replications = 0
m_th = m_d =  0
s_th = s_d =  1
OS = "MAC"

bench_m_non_adapt = microbenchmark(times = 10, 
                         elo(n,
                             m[1],
                             reps,#this is what we are iterating
                             games,
                             K,
                             adaptive, mP, sP,
                             fixed_items, items_true,
                             save_replications,
                             m_th,
                             s_th,
                             m_d,
                             s_d,
                             OS = OS), 
                         elo(n,
                             m[2],
                             reps,#this is what we are iterating
                             games,
                             K,
                             adaptive, mP, sP,
                             fixed_items, items_true,
                             save_replications,
                             m_th,
                             s_th,
                             m_d,
                             s_d,
                             OS = OS),
                         elo(n,
                             m[3],
                             reps,#this is what we are iterating
                             games,
                             K,
                             adaptive, mP, sP,
                             fixed_items, items_true,
                             save_replications,
                             m_th,
                             s_th,
                             m_d,
                             s_d,
                             OS = OS),
                         elo(n,
                             m[4],
                             reps,#this is what we are iterating
                             games,
                             K,
                             adaptive, mP, sP,
                             fixed_items, items_true,
                             save_replications,
                             m_th,
                             s_th,
                             m_d,
                             s_d,
                             OS = OS))
print(bench_m_non_adapt)
boxplot(bench_m_non_adapt, log = FALSE, xlab = games)

#setting up the values
adaptive = 1


bench_m_adapt = microbenchmark(times = 10, 
                                   elo(n,
                                       m[1],
                                       reps,#this is what we are iterating
                                       games,
                                       K,
                                       adaptive, mP, sP,
                                       fixed_items, items_true,
                                       save_replications,
                                       m_th,
                                       s_th,
                                       m_d,
                                       s_d,
                                       OS = OS), 
                                   elo(n,
                                       m[2],
                                       reps,#this is what we are iterating
                                       games,
                                       K,
                                       adaptive, mP, sP,
                                       fixed_items, items_true,
                                       save_replications,
                                       m_th,
                                       s_th,
                                       m_d,
                                       s_d,
                                       OS = OS),
                                   elo(n,
                                       m[3],
                                       reps,#this is what we are iterating
                                       games,
                                       K,
                                       adaptive, mP, sP,
                                       fixed_items, items_true,
                                       save_replications,
                                       m_th,
                                       s_th,
                                       m_d,
                                       s_d,
                                       OS = OS),
                                   elo(n,
                                       m[4],
                                       reps,#this is what we are iterating
                                       games,
                                       K,
                                       adaptive, mP, sP,
                                       fixed_items, items_true,
                                       save_replications,
                                       m_th,
                                       s_th,
                                       m_d,
                                       s_d,
                                       OS = OS))

print(bench_m_adapt)
boxplot(bench_m_adapt, log = FALSE, xlab = games)

##############################################################################
# estimating the run time of one full simulation with and without adaptivity
#############################################################################
#setting up the values
n=c(500,1000,2000)
m = 100
reps_vec = 10
games = 50
K = 0.25
adaptive = 0
mP = sP = 999
fixed_items = 1
items_true = 1
save_replications = 0
m_th = m_d =  0
s_th = s_d =  1
OS = "MAC"

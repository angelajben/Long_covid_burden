# Markov model for QALY losses, DALYs, and productivity losses

# References: 
# Analysis based on R code from https://gist.github.com/tristansnowsill
# Kenya life expectancy table: https://apps.who.int/gho/data/view.searo.60850?lang=en
# Kenya productivity losses dues to COVID-19 from Kenya National Bureau of Statistics: https://www.knbs.or.ke/?s=COVID-19 

n_cy <- 40   # number of cycles. 1 cycle = 3 months (12 weeks) = 4 cycles per year in 10 years
n_hs <- 9    # number of health states
n_co <- 1000 # cohort size

v_hs_names <- c("asymp", "mild.mod_COV","sev_COV","crit_COV","mild_LCOV","mod_LCOV","sev_LCOV","Recovery","Death")

trans_mat <- array(NA_real_,
                   dim = c(n_hs, n_hs, n_cy),
                   dimnames = list(from  = v_hs_names,
                                   to    = v_hs_names,
                                   cycle = 1:n_cy))
# from asymptomatic to other health states
trans_mat[1, 1, ] <- 0
trans_mat[1, 2, ] <- 0
trans_mat[1, 3, ] <- 0
trans_mat[1, 4, ] <- 0
trans_mat[1, 5, ] <- 0.1
trans_mat[1, 6, ] <- 0.01
trans_mat[1, 7, ] <- 0.001

# from mild/moderate COVID-19 to other states
trans_mat[2, 1, ] <- 0
trans_mat[2, 2, ] <- 0
trans_mat[2, 3, ] <- 0.1
trans_mat[2, 4, ] <- 0
trans_mat[2, 5, ] <- 0.15
trans_mat[2, 6, ] <- 0.05
trans_mat[2, 7, ] <- 0.001

# from severe COVID-19 to other states
trans_mat[3, 1, ] <- 0
trans_mat[3, 2, ] <- 0
trans_mat[3, 3, ] <- 0
trans_mat[3, 4, ] <- 0.03
trans_mat[3, 5, ] <- 0.1
trans_mat[3, 6, ] <- 0.2
trans_mat[3, 7, ] <- 0.15

# from critical COVID-19 to other states
trans_mat[4, 1, ] <- 0
trans_mat[4, 2, ] <- 0
trans_mat[4, 3, ] <- 0
trans_mat[4, 4, ] <- 0
trans_mat[4, 5, ] <- 0.05
trans_mat[4, 6, ] <- 0.2
trans_mat[4, 7, ] <- 0.25

# from mild LCOVID to other states
trans_mat[5, 1, ] <- 0
trans_mat[5, 2, ] <- 0
trans_mat[5, 3, ] <- 0
trans_mat[5, 4, ] <- 0
trans_mat[5, 5, ] <- 0.3
trans_mat[5, 6, ] <- 0.01
trans_mat[5, 7, ] <- 0.001

# from moderate LCOVID to other states
trans_mat[6, 1, ] <- 0
trans_mat[6, 2, ] <- 0
trans_mat[6, 3, ] <- 0
trans_mat[6, 4, ] <- 0
trans_mat[6, 5, ] <- 0.4
trans_mat[6, 6, ] <- 0.2
trans_mat[6, 7, ] <- 0.02

# from severe LCOVID to other states
trans_mat[7, 1, ] <- 0
trans_mat[7, 2, ] <- 0
trans_mat[7, 3, ] <- 0
trans_mat[7, 4, ] <- 0
trans_mat[7, 5, ] <- 0.2
trans_mat[7, 6, ] <- 0.4
trans_mat[7, 7, ] <- 0.1

# from recover to other states
trans_mat[8, 1, ] <- 0
trans_mat[8, 2, ] <- 0
trans_mat[8, 3, ] <- 0
trans_mat[8, 4, ] <- 0
trans_mat[8, 5, ] <- 0.1
trans_mat[8, 6, ] <- 0.01
trans_mat[8, 7, ] <- 0.001

# death
trans_mat[9, 1:8, ] <- 0
trans_mat[9, 9, ] <- 1

# time-varying probability of death
trans_mat[1:8, 9, 1:10] <- 0.01
trans_mat[1:8, 9, 11:20] <- 0.02
trans_mat[1:8, 9, 21:30] <- 0.03
trans_mat[1:8, 9, 31:40] <- 0.04

# the prob. of dying in severe and critical COVID-19 is higher than the other states
trans_mat[3, 9, ] <- trans_mat[3, 9, ] + 0.01
trans_mat[4, 9, ] <- trans_mat[4, 9, ] + 0.02

# Recovery should be 1 - the sum of the other states
trans_mat[1, 8, ] <- 1 - apply(trans_mat[1, , ], 2, sum, na.rm =TRUE)
trans_mat[2, 8, ] <- 1 - apply(trans_mat[2, , ], 2, sum, na.rm =TRUE)
trans_mat[3, 8, ] <- 1 - apply(trans_mat[3, , ], 2, sum, na.rm =TRUE)
trans_mat[4, 8, ] <- 1 - apply(trans_mat[4, , ], 2, sum, na.rm =TRUE)
trans_mat[5, 8, ] <- 1 - apply(trans_mat[5, , ], 2, sum, na.rm =TRUE)
trans_mat[6, 8, ] <- 1 - apply(trans_mat[6, , ], 2, sum, na.rm =TRUE)
trans_mat[7, 8, ] <- 1 - apply(trans_mat[7, , ], 2, sum, na.rm =TRUE)
trans_mat[8, 8, ] <- 1 - apply(trans_mat[8, , ], 2, sum, na.rm =TRUE)

hs_membership <- array(NA_real_,
                          dim = c(n_cy, n_hs),
                          dimnames = list(cycle = 1:n_cy,
                                          state = v_hs_names))

hs_membership[1, ] <- c(n_co*0.3, n_co*0.6, n_co*0.09, n_co*0.01, 0, 0, 0, 0, 0)
sum(hs_membership[1, ])

for (i in 2:n_cy) {
  hs_membership[i, ] <- hs_membership[i - 1, ] %*% trans_mat[, , i - 1]
}

# plot hs_membership
matplot(1:n_cy, hs_membership)
matplot(1:n_cy, hs_membership, type = 'l', pch = 15:18, col = c(1:4, 6))
legend("topright", legend =  v_hs_names, pch = 15:18, col = c(1:4, 6), cex = 0.5)


payoffs <- array(NA_real_,
                 dim = c(n_hs, 2, n_cy),
                 dimnames = list(state  = v_hs_names,
                                 payoff = c("Cost Productivity losses", "QALY"),
                                 cycle  = 1:n_cy))

payoffs[, ,  1:40] <- c(10,  800, 1500, 3000, 800, 1500, 3000, 0, 0, 0.95, 0.65, 0.45, 0.10, 0.65, 0.45, 0.10, 0.95, 0)

payoff_trace <- array(NA_real_,
                      dim = c(n_cy, 2),
                      dimnames = list(cycle  = 1:n_cy,
                                      payoff = c("Cost Productivity losses", "QALY")))

for (i in 1:n_cy) {
  payoff_trace[i, ] <- hs_membership[i, ] %*% payoffs[, , i]
}

colSums(payoff_trace) / n_c

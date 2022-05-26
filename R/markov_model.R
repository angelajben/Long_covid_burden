# Markov model for costs and QALY losses

# References: 
# Analysis based on R code from https://gist.github.com/tristansnowsill
# Kenya life expectancy table: https://apps.who.int/gho/data/view.searo.60850?lang=en
# Kenya productivity losses dues to COVID-19 from Kenya National Bureau of Statistics: https://www.knbs.or.ke/?s=COVID-19 

n_t <- 25
n_s <- 4
n_c <- 1000

v_state_names <- c("Long COVID", "Recovery", "COVID-19", "Dead")

trans_mat <- array(NA_real_,
                   dim = c(n_s, n_s, n_t),
                   dimnames = list(from  = v_state_names,
                                   to    = v_state_names,
                                   cycle = 1:n_t))

trans_mat[2, 1, ] <- 0.03
trans_mat[3, 1, ] <- 0
trans_mat[3, 2, ] <- 0

trans_mat[1, 2, ] <- 0.03
trans_mat[3, 3, ] <- 1

trans_mat[1, 3,  1:5] <- 0.01
trans_mat[1, 3,  6:10] <- 0.02
trans_mat[1, 3, 11:15] <- 0.04
trans_mat[1, 3, 16:20] <- 0.08

trans_mat[2, 3, ] <- trans_mat[1, 3, ] + 0.04

trans_mat[1, 1, ] <- 1 - apply(trans_mat[1, , ], 2, sum, na.rm = TRUE)
trans_mat[2, 2, ] <- 1 - apply(trans_mat[2, , ], 2, sum, na.rm = TRUE)

state_membership <- array(NA_real_,
                          dim = c(n_t, n_s),
                          dimnames = list(cycle = 1:n_t,
                                          state = v_state_names))

state_membership[1, ] <- c(n_c, 0, 0)

for (i in 2:n_t) {
  state_membership[i, ] <- state_membership[i - 1, ] %*% trans_mat[, , i - 1]
}

payoffs <- array(NA_real_,
                 dim = c(n_s, 2, n_t),
                 dimnames = list(state  = v_state_names,
                                 payoff = c("Cost", "QALY"),
                                 cycle  = 1:n_t))

payoffs[, ,  1:5] <- c(10,  800, 0, 0.95, 0.65, 0)
payoffs[, ,  6:10] <- c(25, 1000, 0, 0.92, 0.60, 0)
payoffs[, , 11:15] <- c(40, 1200, 0, 0.88, 0.55, 0)
payoffs[, , 16:20] <- c(80, 1000, 0, 0.85, 0.50, 0)

payoff_trace <- array(NA_real_,
                      dim = c(n_t, 2),
                      dimnames = list(cycle  = 1:n_t,
                                      payoff = c("Cost", "QALY")))

for (i in 1:n_t) {
  payoff_trace[i, ] <- state_membership[i, ] %*% payoffs[, , i]
}

colSums(payoff_trace) / n_c

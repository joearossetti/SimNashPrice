set.seed(1234)
alpha <- 4
beta <- c(2.0, 1.0, 1.5, 0.5, 1.75)
gamma <- c(0.2, 0.1, 0.05, 0.15, 0.25)

## Make a list of factor level
char_levels <- list(char1 = c(0,1), char2 = c(0,1), char3 = c(0,1) , char4 = c(0,1), char5 = c(0,1))

## Make list of all potential products
XP <- expand.grid(char_levels)
J <- dim(XP)[1]

XP <- data.frame(XP, prodname=1:J)

my_Tastes <- cbind(XP$char1, XP$char2, XP$char3, XP$char4, XP$char5) %*% beta
my_Costs <- cbind(XP$char1, XP$char2, XP$char3, XP$char4, XP$char5) %*% gamma

my_Pot_prod_df <- make_potential_prods_df(Prod_ids = XP$prodname, Tastes = my_Tastes, Costs = my_Costs)

a1 <- vector('logical', dim(XP)[1])
a2 <- vector('logical', dim(XP)[1])
a1[sample(1:(2^5), size = 7)] <- TRUE
a2[sample(1:(2^5), size = 7)] <- TRUE

activity_matrix <- cbind(a1, a2)

num_prods <- sum(a1)+sum(a2)

beta_is <- cbind(rnorm(50, 0, 0.25), rnorm(50, 0, 0.25), rnorm(50, 0, 0.25), rnorm(50, 0, 0.25), rnorm(50, 0, 0.25))

my_muis <- as.matrix(XP[,1:5]) %*% t(beta_is)

my_alphais <- t(rnorm(50, 0, 0.25))

my_draws <- 100

my_mc_error_mat <- matrix(rnorm(num_prods*my_draws, 0, 0.05), my_draws, num_prods)
my_struct_error_mat <- matrix(rnorm(num_prods*my_draws, 0, 0.25), my_draws, num_prods)
my_error_mats <- list('mc_error_mat' = my_mc_error_mat, 'struct_error_mat' = my_struct_error_mat)
my_ZFP_opts <- list(tol = .Machine$double.neg.eps, Max_iter = 2000, rel_tol = .Machine$double.neg.eps)

my_test_cf <- MarkeCF$new(Firm_id = 1, Firm_Names=c(1,2), fixed_cost_per_prod = 0.05,
                          A_mat = activity_matrix,
                          ZFP_opts = my_ZFP_opts,
                          Error_mats = my_error_mats,
                          market_type = 'rlmkt',
                          Potential_products = my_Pot_prod_df,
                          Firms = c(1,2),
                          Muij_mat = my_muis,
                          Alpha = alpha,
                          Aij_mat = my_alphais)

my_test_cf$getMkt()

my_test_cf$exec_cf(prod_state = a2, firm_to_change = 1, MEM = TRUE)


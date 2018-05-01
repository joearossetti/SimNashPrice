set.seed(1234)
alpha <- 5
beta <- c(2.0, 1.0, 1.5)
gamma <- c(0.2, 0.1, 0.05)

## Make a list of factor level
char_levels <- list(char1 = c(0,1), char2 = c(0,1), char3 = c(0,1))

## Make list of all potential products
XP <- expand.grid(char_levels)
J <- dim(XP)[1]

XP <- data.frame(XP, prodname=1:J)

my_Tastes <- cbind(XP$char1, XP$char2, XP$char3) %*% beta
my_Costs <- cbind(XP$char1, XP$char2, XP$char3) %*% gamma

my_Pot_prod_df <- make_potential_prods_df(Prod_ids = XP$prodname, Tastes = my_Tastes, Costs = my_Costs)

a1 <- vector('logical', dim(XP)[1])
a2 <- vector('logical', dim(XP)[1])
a1[sample(1:(2^3), size = 4)] <- TRUE
a2[sample(1:(2^3), size = 4)] <- TRUE

activity_matrix <- cbind(a1, a2)

num_prods <- sum(a1)+sum(a2)

beta_is <- cbind(rnorm(50, 0, 0.25), rnorm(50, 0, 0.25), rnorm(50, 0, 0.25))

my_muis <- as.matrix(XP[,1:3]) %*% t(beta_is)

my_alphais <- t(rnorm(50, 0, 0.25))

my_draws <- 100

my_mc_error_mat <- matrix(rnorm(num_prods*my_draws, 0, 0.025), my_draws, 8*2)
my_struct_error_mat <- matrix(rnorm(num_prods*my_draws, 0, 0.1), my_draws, 8*2)
my_error_mats <- list('mc_error_mat' = my_mc_error_mat, 'struct_error_mat' = my_struct_error_mat)
my_ZFP_opts <- list(tol = .Machine$double.neg.eps, Max_iter = 10000, rel_tol = .Machine$double.neg.eps)

my_test_cf <- MarketCF$new(Firm_id = 1, Firm_Names=c(1,2), fixed_cost_per_prod = 0.0,
                          A_mat = activity_matrix,
                          ZFP_opts = my_ZFP_opts,
                          Error_mats = my_error_mats,
                          market_type = 'rlmkt',hash_size = 1000,
                          Potential_products = my_Pot_prod_df,
                          Firms = c(1,2),
                          Muij_mat = my_muis,
                          Alpha = alpha,
                          Aij_mat = my_alphais)

my_test_cf$getMkt()

# my_test_cf$exec_cf(prod_state = a2, firm_to_change = 1, MEM = TRUE)
#
# my_test_cf$exec_cf(prod_state = a2, firm_to_change = 1, MEM = TRUE)
#
# my_test_cf$exec_cf(prod_state = a1, firm_to_change = 1, MEM = TRUE)
#
# a1[2] <- !a1[2]
# my_test_cf$exec_cf(prod_state = a1, firm_to_change = 1, MEM = TRUE)

# test_hash <- copy(my_test_cf$getTp_cache())
#
# hash::del(x = names(purrr::discard(as.list(test_hash), .p=~.x[['count']]<5)), hash = test_hash)

# test_prod_ids <- sample(1:(2^5), size = 9)
# test_prod_sets <- purrr::rerun(100, sample(c(TRUE, FALSE), replace = TRUE, size = 9))
#
# system.time(
#   for(i in 1:100){
#     a1 <- vector('logical', 32)
#     a1[test_prod_ids] <- test_prod_sets[[i]]
#     my_test_cf$exec_cf(prod_state = a1, firm_to_change = 1, MEM = TRUE)
#   }
# )

# my_test_cf$gradient(base_set = a1, prod_index = 6, firm_to_change = 1, set_grad = FALSE)
#
# my_test_cf$gradients(base_set = a1,firm_to_change = 1, set_grad = FALSE)
#
# my_test_cf$curvature(base_set = rep(TRUE, length(a1)), firm_to_change = 1, set_grad = FALSE)

# library(foreach)
# my_cluster <- parallel::makeCluster(2, type = 'FORK')
# doParallel::registerDoParallel(my_cluster)
# system.time(
#   foreach(i=1:100) %dopar% {
#     a1 <- vector('logical', 32)
#     a1[test_prod_ids] <- test_prod_sets[[i]]
#     print(my_test_cf$exec_cf(prod_state = a1, firm_to_change = 1, MEM = TRUE))
#   }
# )


#test_prod_ids <- sample(1:(2^5), size = 9)
#test_prod_sets <- purrr::rerun(100, sample(c(TRUE, FALSE), replace = TRUE, size = 9))
#test_prod_states <- purrr::map(test_prod_sets, .f=function(X){a <- vector('logical', 32); a[test_prod_ids] <- X; return(a)})
# test_prod_states <- purrr::rerun(100, sample(c(TRUE, FALSE), replace = TRUE, size = 8))
#
# system.time(
# my_test_cf$batch_exec_cf(prod_states = test_prod_states, firm_to_change = 1, CLUSTER = 2)
# )

system.time(
test_hmc_sample <- my_test_cf$hmc_sampler(L = 500, Time = (ceiling(8)+0.5)*pi, Sigma = diag(1, nrow = 8, ncol=8), firm_to_change = 1, temp = 1e5)
)


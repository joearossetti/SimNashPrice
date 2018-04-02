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

my_test_mkt <- LMarket$new(Potential_products = my_Pot_prod_df, Firms = c(1,2), Deriv_price = alpha)

a1 <- vector('logical', dim(XP)[1])
a2 <- vector('logical', dim(XP)[1])
a1[sample(1:(2^5), size = 7)] <- TRUE
a2[sample(1:(2^5), size = 7)] <- TRUE

activity_matrix <- cbind(a1, a2)

my_test_mkt$setActiveProds(Activity_matrix = activity_matrix)

num_prods <- sum(a1)+sum(a2)

my_test_mkt$setPrices(Prices = rep(0, num_prods))
my_test_mkt$setShares(Shares = rep(0, num_prods))
my_test_mkt$setShocks(Cost_shocks = rnorm(num_prods, mean = 0, sd = 0.05),
                      Taste_shocks = rnorm(num_prods, mean = 0, sd = 1))

my_test_mkt$getMarket()
my_test_mkt$share_fun()
my_test_mkt$getMarket()
my_test_mkt$Ownership()
my_test_mkt$Ds_fun()
my_test_mkt$getDs_list()

microbenchmark::microbenchmark({
  my_test_mkt$setPrices(rep(0, num_prods))
  my_test_mkt$setShares(Shares = rep(0, num_prods))
  my_test_mkt$ZFP(tol = .Machine$double.neg.eps, max_iter = 2000, rel_tol = .Machine$double.neg.eps, quietly = FALSE)
})

my_test_mkt$getMarket()
my_test_mkt$getProfits()
my_test_mkt$getIncVal()

my_draws <- 100

my_mc_error_mat <- matrix(rnorm(num_prods*my_draws, 0, 0.05), my_draws, num_prods)
my_struct_error_mat <- matrix(rnorm(num_prods*my_draws, 0, 0.25), my_draws, num_prods)

#my_mc_error_mat <- matrix(rep(0, num_prods*my_draws), my_draws, num_prods)
#my_struct_error_mat <- matrix(rep(0, num_prods*my_draws), my_draws, num_prods)

test_exp_profs <- my_test_mkt$exp_profs(mc_error_mat = my_mc_error_mat,
                      struct_error_mat = my_struct_error_mat,
                      draws = my_draws,
                      tol = .Machine$double.neg.eps, Max_iter = 2000, rel_tol = .Machine$double.neg.eps)



library(SimNashPrice)
set.seed(1234)

test_mkt_maker <- function(){
  ## Parameters of demand function
  alpha <- 4
  beta <- c(2.5, 1, 1.5, 0.5, 2.75, 0.75, 1.75, 0.25)
  gamma <- c(0.2, 0.1, 0.05, 0.15, 0.25, 0.175, 0.0575, 0.157)

  ## Make a list of factor level
  char_levels <- list(char1 = c(0,1), char2 = c(0,1), char3 = c(0,1) , char4 = c(0,1), char5 = c(0,1), char6 = c(0,1), char7 = c(0,1) , char8 = c(0,1))

  ## Make list of all potential products
  XP <- expand.grid(char_levels)
  J <- dim(XP)[1]

  XP <- data.frame(XP, prodname=1:J)

  a1 <- vector('numeric', dim(XP)[1])
  a2 <- vector('numeric', dim(XP)[1])
  a1[sample(1:(2^8), size = 20)] <- 1
  a2[sample(1:(2^8), size = 15)] <- 1

  Xi <- rnorm(1, 0, 1)
  Omega <- rnorm(1, 0, 0.05)

  my_prod_table <- data.frame(j = XP$prodname, delta = (cbind(XP$char1, XP$char2, XP$char3, XP$char4, XP$char5, XP$char6, XP$char7, XP$char8) %*% beta),
                              mc = (cbind(XP$char1, XP$char2, XP$char3, XP$char4, XP$char5, XP$char6, XP$char7, XP$char8) %*% gamma))

  beta_is <- cbind(rnorm(50, 0, 1), rnorm(50, 0, 1), rnorm(50, 0, 1), rnorm(50, 0, 1), rnorm(50, 0, 1), rnorm(50, 0, 1), rnorm(50, 0, 1), rnorm(50, 0, 1))

  my_muis <- as.matrix(XP[,1:8]) %*% t(beta_is)

  my_alphais <- t(rnorm(50, 0, 1))

  my_mkt_prods <- rbind(my_prod_table[which(a1==1),], my_prod_table[which(a2==1),])
  my_mkt_prods <- data.frame(my_mkt_prods, firm = c(rep(1, sum(a1)), rep(2, sum(a2))))

  my_mkt_muis <- my_muis[my_mkt_prods$j,]

  my_rldmkt_obj <- rLogit_Demand_Market$new(Firms = my_mkt_prods$firm,
                                            Muij_mat = my_mkt_muis,
                                            Aij_mat = my_alphais,
                                            Prod_ids = my_mkt_prods$j,
                                            Delta = my_mkt_prods$delta,
                                            Mc_fixed = my_mkt_prods$mc,
                                            Mc_error = 0,
                                            Struct_error = 0,
                                            U_out_opt = 0,
                                            Alpha = alpha)

  #my_rldmkt_obj$zeta_fixed_point(tol = 1e-18, max_iter = 1e5)

  # vec_of_tols <- 1/(10^(6:20))
  # counts_per_tol <- purrr::map(vec_of_tols, .f=~my_rldmkt_obj$clone()$zeta_fixed_point(tol = .x, max_iter = 2000, rel_tol=.Machine$double.eps))

  my_rldmkt_obj$zeta_fixed_point(tol = .Machine$double.eps, max_iter = 2000, rel_tol=.Machine$double.eps, quietly=FALSE)
}

my_test_market_obj <- test_mkt_maker()

# my_test_market_obj$markups()
#
# test_DDD <- my_test_market_obj$computeJacobians()
#
# test_Dv_p <- my_test_market_obj$Dv_reaction_fun()
#
# test_grad <- my_test_market_obj$grad_eq_prof(1)
#
# test_DvDv_p <- my_test_market_obj$DvDv_reaction_fun()
#
# test_hessian_prof <- my_test_market_obj$hessian_eq_prof(1)

#source('inst/rlogit_test.R')
#hessian_prof_orig <- hessian_fun(my_test_market_obj$clone())

# microbenchmark::microbenchmark({
#   my_test_market_obj$
#     markups()$
#     computeJacobians()$
#     Dv_reaction_fun()$
#     DvDv_reaction_fun()$
#     hessian_eq_prof(1)
# }, times = 500)



# microbenchmark::microbenchmark( my_test_market_obj$exp_prof_apprx(name_of_firm = 1,
#                                   mean_vec = my_means,
#                                   cov_mat = my_cov_mat,
#                                   tol = 1e-8,
#                                   Max_iter = 2000) )

#Omega <- my_test_market_obj$getMarket()[['Mc_error']]

#x_norm <- rnorm(500, 0, 2)
# my_struct_error_fun <- function(N){sample(x = x_norm, size = N)}
#
# old_exp_profs <- my_test_market_obj$exp_profits(mc_error_fun = function(N){Omega}, struct_error_fun = my_struct_error_fun, draws = 10, tol = 1e-8, Max_iter = 2000)

my_draws <- 5
num_prods <- length(my_test_market_obj$getMarket()[['Mc_error']])

#my_mc_error_mat <- matrix(Omega, my_draws, num_prods)
my_mc_error_mat <- matrix(rnorm(num_prods*my_draws, 0, 0.05), my_draws, num_prods)
my_struct_error_mat <- matrix(rnorm(num_prods*my_draws, 0, 2), my_draws, num_prods)

old_exp_profs2 <- my_test_market_obj$exp_profits_mat(mc_error_mat = my_mc_error_mat, struct_error_mat = my_struct_error_mat,
                                                     draws = my_draws, tol = .Machine$double.eps, Max_iter = 2000, rel_tol=.Machine$double.eps, compute_inc_value = TRUE, quietly=FALSE)


old_exp_profs_mat <- matrix(NA, 500, 2)
for(i in 1:100){
my_draws <- 50
num_prods <- length(my_test_market_obj$getMarket()[['Mc_error']])

#my_mc_error_mat <- matrix(Omega, my_draws, num_prods)
my_mc_error_mat <- matrix(rnorm(num_prods*my_draws, 0, 0.05), my_draws, num_prods)
my_struct_error_mat <- matrix(rnorm(num_prods*my_draws, 0, 0.5), my_draws, num_prods)

old_exp_profs50 <- my_test_market_obj$exp_profits_mat(mc_error_mat = my_mc_error_mat, struct_error_mat = my_struct_error_mat,
                                                     draws = my_draws, tol = .Machine$double.eps, Max_iter = 2000, rel_tol=.Machine$double.eps, compute_inc_value = FALSE, quietly=TRUE)

old_exp_profs_mat[i,] <- old_exp_profs50
}


my_means <- rep(0, num_prods*2)

my_cov_mat <- diag(c(rep(0.5, num_prods), rep(0.05, num_prods)), num_prods*2, num_prods*2)

test_exp_profits <- my_test_market_obj$exp_prof_apprx(name_of_firm = 1,
                                                      mean_vec = my_means,
                                                      cov_mat = my_cov_mat,
                                                      tol = .Machine$double.eps,
                                                      rel_tol = .Machine$double.eps,
                                                      Max_iter = 2000, quietly=TRUE, costs=TRUE)

microbenchmark::microbenchmark(my_test_market_obj$exp_prof_apprx(name_of_firm = 1,
                                                                 mean_vec = my_means,
                                                                 cov_mat = my_cov_mat,
                                                                 tol = .Machine$double.eps,
                                                                 rel_tol = .Machine$double.eps,
                                                                 Max_iter = 2000, quietly=TRUE))

microbenchmark::microbenchmark(my_test_market_obj$exp_profits_mat(mc_error_mat = matrix(Omega, 100, num_prods), struct_error_mat = matrix(rnorm(num_prods*100, 0, 1), 100, num_prods),
                                                                  draws = 100, tol = .Machine$double.eps, Max_iter = 2000, rel_tol=.Machine$double.eps, compute_inc_value = FALSE, quietly=TRUE))

test_hess <- my_test_market_obj$
    markups()$
    computeJacobians()$
    Dv_reaction_fun()$
    DvDv_reaction_fun()$
    hessian_eq_prof(1)

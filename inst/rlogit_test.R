library(SimNashPrice)
set.seed(1234)

## Parameters of demand function
alpha <- 4
beta <- c(-5, -4)
gamma <- c(1, 0.5)

## Make a list of factor level
char_levels <- list(char1 = c(0,1), char2 = c(0,1))

## Make list of all potential products
XP <- expand.grid(char_levels)
J <- dim(XP)[1]

XP <- data.frame(XP, prodname=1:J)

a1 <- sample(c(0,1), 4, replace = TRUE)
a2 <- sample(c(0,1), 4, replace = TRUE)

Xi <- rnorm(1, 0, 1)
Omega <- rnorm(1, 0, 1)

my_prod_table <- data.frame(j = XP$prodname, delta = (cbind(XP$char1, XP$char2) %*% beta), mc = (cbind(XP$char1, XP$char2) %*% gamma))

beta_is <- cbind(rnorm(500, 0, 1), rnorm(500, 0, 1))

my_muis <- as.matrix(XP[,1:2]) %*% t(beta_is)

my_alphais <- t(rnorm(500, 0, 1))

my_mkt_prods <- rbind(my_prod_table[which(a1==1),], my_prod_table[which(a2==1),])
my_mkt_prods <- data.frame(my_mkt_prods, firm = c(1,1,1,1,1))

my_mkt_muis <- my_muis[my_mkt_prods$j,]

my_rldmkt_obj <- rLogit_Demand_Market$new(Firms = my_mkt_prods$firm,
                                          Muij_mat = my_mkt_muis,
                                          Aij_mat = my_alphais,
                                          Prod_ids = my_mkt_prods$j,
                                          Delta = my_mkt_prods$delta,
                                          Mc_fixed = my_mkt_prods$mc,
                                          Mc_error = Xi,
                                          Struct_error = Xi,
                                          U_out_opt = 0,
                                          Alpha = alpha)

my_rldmkt_obj$getMarket()
my_rldmkt_obj$getDs()
my_rldmkt_obj$getAlpha()
my_rldmkt_obj$getAijs()
my_rldmkt_obj$getDerivPrice()
my_rldmkt_obj$getUjs()
my_rldmkt_obj$getMuijs()
my_rldmkt_obj$getUijs()

## test share method
# Market <- my_rldmkt_obj$getMarket()
# Price <- Market[['Price']]
# Deriv_price <- my_rldmkt_obj$getDerivPrice()
# Uijs <- my_rldmkt_obj$getUijs()
# Index <- Uijs + Price %*% Deriv_price
# S <- exp(Index)
# Denom <- exp(0) + colSums(S)
# rowMeans(S / Denom)
#
# Si <- S / Denom
#
# G <- matrix(0, nrow = dim(Si)[1], ncol = dim(Si)[1])
# for(i in 1:dim(Si)[2]){
#   G <- G + ((Deriv_price[i] * Si[,i] %*% t(Si[,i])) * my_rldmkt_obj$getOwnership())
# }
#
# G <- G * (1/dim(Si)[2])
#
# G_cpp <- gamma_helper(Sr=Si, ar=t(Deriv_price), Or=my_rldmkt_obj$getOwnership())

my_rldmkt_obj$share()
my_rldmkt_obj$getMarket()[['Share']]

my_rldmkt_obj$Ds_fun()
my_rldmkt_obj$getDs()

my_rldmkt_obj$zeta_fixed_point(tol = 1e-8, max_iter = 1e5)

my_rldmkt_obj$markups()
my_rldmkt_obj$getMarket()

my_struct_error_fun <- function(N){
  rnorm(N)
}

my_rldmkt_obj$inc_value(mc_error_fun = my_struct_error_fun, struct_error_fun = my_struct_error_fun, draws = 100, tol = 1e-8, Max_iter = 1e5)

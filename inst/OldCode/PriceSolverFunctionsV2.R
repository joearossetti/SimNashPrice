#' Zeta Fixed Point assuming Logit demand
#'
#' @param x object of class 'Logit_Demand_Market' (looks for Mc at x$Market$Mc)
#' @param tol what tolerance to use for convergence in the fixed point iterations
#'
#' @return x updated with prices that solve the zeta fixed point problem
#' @export
#'
#' @examples
#' set.seed(1234)
#'
#'## Parameters of demand function
#'alpha <- 4
#'beta <- c(2, 3.5)
#'gamma <- c(0.5, 0.3)
#'
#'## Make a list of factor level
#'char_levels <- list(char1 = c(0,1), char2 = c(0,1))
#'
#'## Make list of all potential products
#'XP <- expand.grid(char_levels)
#'J <- dim(XP)[1]
#'
#'XP <- data.frame(XP, prodname=1:J)
#'
#'a1 <- sample(c(0,1), 4, replace = TRUE)
#'a2 <- sample(c(0,1), 4, replace = TRUE)
#'
#'Xi <- rnorm(1, 0, 1)
#'Omega <- rnorm(1, 0, 1)
#'
#'my_prod_table <- data.frame(j = XP$prodname, delta = (cbind(XP$char1, XP$char2) %*% beta), mc = (cbind(XP$char1, XP$char2) %*% gamma))
#'
#'my_mkt_prods <- rbind(my_prod_table[which(a1==1),], my_prod_table[which(a2==1),])
#'my_mkt_prods <- data.frame(my_mkt_prods, firm = c(1,1,1,0,0))
#'
#'my_ldm_obj <- logit_demand_market(my_mkt_prods$firm, my_mkt_prods$delta, Mc = my_mkt_prods$mc, Struct_error = Xi, Deriv_price = alpha)
#'my_ldm_obj <- share(my_ldm_obj)
#'P_star <- zeta_fixed_point(my_ldm_obj, tol = 1e-6)
zeta_fixed_point.Logit_Demand_Market <- function(x, tol){
  Jt <- x$Jt
  O_mat <- x$O
  MC <- as.numeric(x$Market$Mc)

  error <- 1e6
  p <- as.numeric(x$Market$Price)
  x <- share(x) # initial shares
  s <- x$Market$Share
  Ds <- Ds_fun(x)$Ds # first derivative matrix
  Lambda_inv <- solve(Ds$Lambda_p) # first inverse
  while(error > tol){
    ## Update the price using the fixed point equation
    P1 <- Lambda_inv %*% (-s) - Lambda_inv %*% Ds$Gamma_p %*% (p - MC) + MC
    x$Market$Price <- P1
    ## use the new prices to calculate new shares
    x <- share(x) # initial shares
    s <- x$Market$Share
    Ds <- Ds_fun(x)$Ds # new derivative matrix
    Lambda_inv <- solve(Ds$Lambda_p) # new inverse
    foc <- t(Ds$D_p) %*% (P1 - MC) + s # compute the first order condition
    #print(foc)
    error <- max(abs(foc)) # check for whether the first order condition is 0 for this iteration
    #print(error)
    #print(P1-p)
    p <- P1
  }
  ## update x and return
  x$Market$Price <- p
  x <- share(x)
  x <- Ds_fun(x)
  return(x)
}

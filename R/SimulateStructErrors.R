#' Title Simulate Markets with different Structural Errors
#'
#'  This function takes a 'Logit_Demand_Market' object
#'  and a function that generates draws from th structural errors of the model (either estimates from GMM or parametric assumption)
#'  Calculates nash prices and returns a list of 'Logit_Demand_Market's based on each draw from the errors
#'
#' @param logit_market 'Logit_Demand_Market' object
#' @param struct_error_draw_fun function that returns a list of market objects length draws evaluated at each draw from the errors
#' @param draws numer of draws to take (passed to struct_error_draw_fun)
#' @param tol tolerance for the fixed point
#' @param logit_market_method a method for the class 'Logit_Demand_Market' to apply to each draw (for example computing firm profits)
#' @param ... additional arguments to struct_error_draw_fun
#'
#' @return list of 'Logit_Demand_Market' object with the NE corresponding to the draws from the structural errors
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
#'
#'my_struct_error_fun <- function(draws, ...){
#' rnorm(n=draws, ...)
#'}
#'
#'my_markets <- sim_struct_errors(logit_market=my_ldm_obj, struct_error_draw_fun=my_struct_error_fun, logit_market_method=firm_profits,draws=500, tol=1e-6, mean=0, sd=1)
sim_struct_errors <- function(logit_market, struct_error_draw_fun, logit_market_method, draws, tol, ...){
  errors <- as.list(struct_error_draw_fun(draws, ...))
  markets <- vector('list', 500)
  for(i in 1:500){
    my_ldm_obj$Market$Struct_Err <- errors[[i]]
    markets[[i]] <- zeta_fixed_point(my_ldm_obj, tol = tol)
    markets[[i]] <- logit_market_method(markets[[i]])
  }
  return(markets)
}

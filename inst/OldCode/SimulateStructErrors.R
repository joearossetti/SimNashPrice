#' Simulate Markets with different Structural Errors
#'
#'  This function takes a 'Logit_Demand_Market' object
#'  and a function that generates draws from th structural errors of the model (either estimates from GMM or parametric assumption)
#'  Calculates nash prices and returns a list of 'Logit_Demand_Market's based on each draw from the errors
#'
#' @param logit_market 'Logit_Demand_Market' object
#' @param struct_error_draw_fun function that takes the number of products as its only arguement and returns a sample fromr the dist. of structural errors (use a closure)
#' @param draws numer of draws to take (passed to struct_error_draw_fun)
#' @param tol tolerance for the fixed point
#' @param mc_error_draw_fun function that takes the number of products as its only arguement and returns a sample fromr the dist. of mc errors (use a closure)
#' @param logit_market_method a method for the class 'Logit_Demand_Market' to apply to each draw (for example computing firm profits)
#'
#' @return list of 'Logit_Demand_Market' object with the NE corresponding to the draws from the structural errors
#' @export
#'
#' @examples
#' #NA
sim_struct_errors <- function(logit_market, struct_error_draw_fun, mc_error_draw_fun, logit_market_method, draws, tol){
  markets <- vector('list', draws)
  for(i in 1:draws){
    number_of_products <- length(logit_market$Market)
    logit_market$Market$Struct_Err <- struct_error_draw_fun(number_of_products)
    logit_market$Market$Mc_error <- mc_draw_fun(number_of_products)
    logit_market <- Mc_method(logit_market)
    markets[[i]] <- zeta_fixed_point(logit_market, tol = tol)
    markets[[i]] <- logit_market_method(markets[[i]])
  }
  return(markets)
}

#' Expected Variable Profits
#'
#'  Given a pair of functions for drawing from the distribution of the econometric errors,
#'  draw from the distribution, run the Zeta Fixed Point to re-compute prices and profits for each draw,
#'  and find the expected variable profits.
#'
#' @param mc_error_fun function for drawing from the marginal cost errors
#' @param struct_error_fun function for drawing from the utility errors
#' @param draws the number of draws to take
#' @param tol when should the zeta fixed point converge
#'
#' @return vector of each firms expected profits
#' @export
#'
#' @examples #NA
ldmkt_exp_profits <- function(mc_error_fun, struct_error_fun, draws, tol){
  var_prof_mat <- matrix(nrow = draws, ncol = private$num_firms)
  for(i in 1:draws){
    private$ujs <- private$Market[['Delta']] + struct_error_fun(private$Jt)
    private$cjs <- private$Market[['Mc_fixed']]*exp(mc_error_fun(private$Jt))

    self$zeta_fixed_point(tol=tol)

    for(j in 1:private$num_firms){
      f <- private$firm_names[j]
      Mkt <- private$Market
      which_prods <- which(Mkt[['Firms']]==f)
      var_prof_mat[i,j] <- sum((as.numeric(Mkt[['Price']])[which_prods]-private$cjs[which_prods])*as.numeric(Mkt[['Share']])[which_prods])
    }
  }
  return(colSums(var_prof_mat)/draws)
}

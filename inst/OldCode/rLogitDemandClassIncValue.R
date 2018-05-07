#' Consumer Welfare: Inclusive Value Rand. Coef.
#'
#'  Given a pair of functions for drawing from the distribution of the econometric errors,
#'  draw from the distribution, run the Zeta Fixed Point to re-compute prices and profits for each draw,
#'  and find the inclusive value (which is the consumer's expected welfare).
#'
#' @param mc_error_fun function for drawing from the marginal cost errors
#' @param struct_error_fun function for drawing from the utility errors
#' @param draws the number of draws to take
#' @param tol when should the zeta fixed point converge
#'
#' @return numeric consumer welfare
#' @export
#'
#' @examples #NA
rldmkt_inc_value <- function(mc_error_fun, struct_error_fun, draws, tol, Max_iter){
  #var_prof_mat <- matrix(nrow = draws, ncol = private$num_firms)
  inc_val_vec <- vector('numeric', length = draws)
  for(i in 1:draws){
    private$ujs <- private$Market[['Delta']] + struct_error_fun(private$Jt)
    private$cjs <- exp(private$Market[['Mc_fixed']])*exp(mc_error_fun(private$Jt))

    self$zeta_fixed_point(tol=tol, max_iter=Max_iter)

    Index <- private$uijs - private$Market[['Price']] %*% private$Deriv_price
    S <- exp(Index)
    Denom <- exp(private$U_out_opt) + colSums(S)
    inc_val_vec[i] <- sum(log(Denom))

    private$cjs <- 0
  }
  return(sum(inc_val_vec)/draws)
}

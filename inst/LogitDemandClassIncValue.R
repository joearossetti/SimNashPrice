#' Consumer Welfare: Inclusive Value
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
ldmkt_inc_value <- function(mc_error_fun, struct_error_fun, draws, tol, Max_iter){
  #var_prof_mat <- matrix(nrow = draws, ncol = private$num_firms)
  inc_val_vec <- vector('numeric', length = draws)
  for(i in 1:draws){
    private$ujs <- private$Market[['Delta']] + struct_error_fun(private$Jt)
    private$cjs <- private$Market[['Mc_fixed']]*exp(mc_error_fun(private$Jt))

    self$zeta_fixed_point(tol=tol, max_iter=Max_iter)

    delta <- private$ujs - private$Deriv_price * private$Market[['Price']]
    S <- exp(delta)
    inc_val_vec[i] <- log(exp(private$U_out_opt) + sum(S))
  }
  return(sum(inc_val_vec)/draws)
}

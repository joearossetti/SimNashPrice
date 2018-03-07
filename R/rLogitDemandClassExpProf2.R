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
rldmkt_exp_profits_mat <- function(mc_error_mat, struct_error_mat, draws,tol, Max_iter, rel_tol=1e-16, compute_inc_value=FALSE, quietly=FALSE){
  assertthat::assert_that(all(dim(mc_error_mat)==dim(struct_error_mat)))
  assertthat::assert_that(dim(mc_error_mat)[1]==draws)
  assertthat::assert_that(dim(mc_error_mat)[2]==private$Jt)

  var_prof_mat <- matrix(nrow = draws, ncol = private$num_firms)
  if(compute_inc_value==TRUE){
    inc_val_vec <- vector('numeric', length = draws)
  }

  initial_ujs <- private$ujs
  initial_cjs <- private$cjs

  for(i in 1:draws){
    private$ujs <- private$Market[['Delta']] + t(as.numeric(struct_error_mat[i,]))

    private$uijs <- as.numeric(private$ujs) + private$muijs

    private$cjs <- exp(private$Market[['Mc_fixed']])*exp(t(as.numeric(mc_error_mat[i,])))

    private$Market[['Price']] <- private$cjs + private$Market[['Markup']]

    self$zeta_fixed_point(tol=tol, max_iter=Max_iter, rel_tol=rel_tol, quietly=quietly)
    self$markupsb()
    #Mkt <- private$Market


    for(j in 1:private$num_firms){
      f <- private$firm_names[j]
      which_prods <- which(private$Market[['Firms']]==f)
      var_prof_mat[i,j] <- sum(private$Market[['Markup']][which_prods]*private$Market[['Share']][which_prods])
    }

    if(compute_inc_value==TRUE){
      Index <- private$uijs - private$Market[['Price']] %*% private$Deriv_price
      S <- exp(Index)
      Denom <- exp(private$U_out_opt) + colSums(S)
      inc_val_vec[i] <- mean(log(Denom) / private$Deriv_price)
    }


  }

  private$cjs <- initial_cjs
  private$ujs <- initial_ujs
  private$uijs <- private$ujs + private$muijs

  profits <- colSums(var_prof_mat)/draws
  if(compute_inc_value==TRUE){
    attr(profits, "inc_value") <- sum(inc_val_vec)/draws
  }
  return(profits)
}

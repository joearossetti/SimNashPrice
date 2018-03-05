#' Apprx Expected Eq. Profits
#'
#' @return expected eq. profits
#' @export
#'
#' @examples #NA
rldmkt_exp_prof_apprx <- function(name_of_firm, mean_vec, cov_mat, tol, rel_tol, Max_iter, quietly, costs){
  firm_number_id <- which(private$firm_names==name_of_firm)
  which_is_firm <- which(private$Market[['Firms']]==name_of_firm)
  which_is_not_firm <- which(private$Market[['Firms']]!=name_of_firm)

  self$zeta_fixed_point(tol=tol, rel_tol= rel_tol, max_iter=Max_iter, quietly=quietly)
  self$markups()
  self$computeJacobians()
  self$Dv_reaction_fun(costs=costs)
  self$DvDv_reaction_fun(costs=costs)

  hessian <- self$hessian_eq_prof(name_of_firm, costs=costs)
  mean_profs <- sum(private$Market[['Markup']][which_is_firm] * private$Market[['Share']][which_is_firm])

  exp_prof <- mean_profs + 0.5 * (sum(diag(hessian %*% cov_mat)) - (t(mean_vec) %*% hessian %*% mean_vec))

  return(list(exp_prof, mean_profs, hessian))
  #invisible(self)
}

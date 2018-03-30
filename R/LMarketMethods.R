lmkt_share_fun <- function(){
  delta <- private$deltas - private$deriv_price * private$prices
  S <- exp(delta)
  private$shares <- S / (private$u_opt_out + sum(S))
  invisible(self)
}

lmkt_Ds_fun <- function(){
  S <- private$shares
  private$Lambda <- -private$deriv_price * diag(S)
  private$Gamma <- private$deriv_price * (S %*% t(S)) * private$O_mat
  private$Ds <- private$Lambda + private$Gamma
  invisible(self)
}

lmkt_getIncVal <- function(){
  delta <- private$deltas - private$deriv_price * private$prices
  S <- exp(delta)
  inc_value <- (log(private$u_opt_out + sum(S)))/private$deriv_price

  return(inc_value)
}

lmkt_exp_profs <- function(mc_error_mat, struct_error_mat, draws,tol, Max_iter, rel_tol, compute_inc_value=TRUE){
  assertthat::assert_that(all(dim(mc_error_mat)==dim(struct_error_mat)))
  assertthat::assert_that(dim(mc_error_mat)[1]==draws)
  assertthat::assert_that(dim(mc_error_mat)[2]==private$Jt)

  var_prof_mat <- matrix(nrow = draws, ncol = private$num_firms)
  if(compute_inc_value==TRUE){
    inc_val_vec <- vector('numeric', length = draws)
  }

  initial_ujs <- private$deltas
  initial_cjs <- private$costs

  for(i in 1:draws){

    self$setShocks(Cost_shocks=mc_error_mat[i,], Taste_shocks=struct_error_mat[i,])


    private$prices <- private$costs

    self$ZFP(tol = tol, max_iter = Max_iter, rel_tol = rel_tol, quietly = TRUE)

    var_prof_mat[i,] <- self$getProfits()

    if(compute_inc_value==TRUE){
      inc_val_vec[i] <- self$getIncVal()
    }
  }

  private$costs <- initial_cjs
  private$deltas <- initial_ujs

  profits <- colMeans(var_prof_mat)
  if(compute_inc_value==TRUE){
    attr(profits, "inc_value") <- mean(inc_val_vec)
  }
  return(profits)
}

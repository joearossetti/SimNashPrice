rlmkt_updateUijs <- function(){
  private$muijs <- private$Muij_all[private$active_products,]
  private$uijs <- matrix(private$deltas, nrow = private$Jt, ncol = dim(private$Muij_all)[2]) + private$muijs
  private$Index <- exp(private$uijs)
}

rlmkt_share_fun <- function(){
  #Index <- private$uijs - private$prices %*% private$deriv_price
  #S <- exp(Index)
  S <- private$Index * exp(-private$prices %*% private$deriv_price)
  Denom <- private$u_opt_out + colSums(S)
  private$Si <- S / matrix(rep(Denom, private$Jt), nrow = private$Jt, ncol = length(Denom), byrow = TRUE)
  private$shares <- rowMeans(private$Si)
  invisible(self)
}

rlmkt_Ds_fun <- function(){
  Jt <- private$Jt
  private$Lambda <- -diag(rowMeans(private$Si %*% private$diag_deriv_price), nrow = Jt, ncol = Jt)
  
  # print('Si')
  # print(private$Si)
  # print('ar')
  # print(as.numeric(private$deriv_price))
  # print('Or')
  # print(private$O_mat)
  
  private$Gamma <- gamma_helper(Sr = private$Si, ar=as.numeric(private$deriv_price), Or=private$O_mat)
  private$Ds <- private$Lambda + private$Gamma
  invisible(self)
}

rlmkt_getIncVal <- function(){
  S <- private$Index * exp(-private$prices %*% private$deriv_price)
  Denom <- private$u_opt_out + colSums(S)
  inc_value <- mean(log(Denom) / private$deriv_price)
  return(inc_value)
}

rlmkt_exp_profs <- function(mc_error_mat, struct_error_mat, draws,tol, Max_iter, rel_tol, compute_inc_value=TRUE){
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
    #print(i)

    self$setShocks(Cost_shocks=mc_error_mat[i,], Taste_shocks=struct_error_mat[i,])
    self$updateUijs()

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

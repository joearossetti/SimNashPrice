Ownership <- function(){
  private$O_mat <- matrix(ncol = private$Jt, nrow = private$Jt, data = 0)
  for(j in 1:private$Jt){
    for(k in 1:private$Jt){
      private$O_mat[j,k] <- private$own_vec[j] == private$own_vec[k]
    }
  }
}

inferMarkups <- function(){
  private$costs <- private$shares %*% solve(private$Ds) + private$prices
}

ZFP <- function(tol, max_iter, rel_tol=1e-12, quietly){
  Jt <- private$Jt
  MC <- private$costs
  self$share_fun() # initial shares
  self$Ds_fun() # first derivative matrix

  error <- 1e6
  counter <- 1

  while(TRUE){
    #print(private$Ds[['Lambda_p']])
    ## Update the price from the last iteration using the fixed point equation
    p_prev <- private$prices

    #Lambda_inv <- diag(1/diag(private$Lambda), nrow = Jt, ncol = Jt)

    ## invert lambda temporarily
    diag(private$Lambda) <- 1/diag(private$Lambda)

    private$prices <- -private$shares %*% private$Lambda - (private$prices - MC) %*% private$Gamma  %*% private$Lambda + MC

    private$prices <- as.numeric(private$prices)

    ## use the new prices to calculate new shares and stuff
    self$share_fun()
    self$Ds_fun() # first derivative matrix

    ## should the loop stop? have we converged?
    foc <- private$shares + (private$prices - MC) %*% private$Ds
    error <- max(abs(foc)) # check for whether the first order condition is 0 for this iteration
    rel_error <- max(abs(private$prices - p_prev)/p_prev)
    if(is.na(error)){
      stop('error is na')
    }
    if(is.nan(error)){
      stop('error is NaN')
    }


    if(error < tol){
      if(quietly==FALSE){
        message(paste('tol reached for foc, counter:', counter))
      }

      break
    }
    if(rel_error<rel_tol){
      if(quietly==FALSE){
        message(paste('rel_tol reached for prices, counter:', counter))
      }
      break
    }
    counter <- counter + 1
    if(counter > max_iter){
      stop('hit max iterations')
    }

  }

  if(any(private$prices - MC < 0)){
    stop('price < mc')
  }

  ## reset Lambda from being inverted
  diag(private$Lambda) <- 1/diag(private$Lambda)

  #print(counter)
  invisible(self)
}

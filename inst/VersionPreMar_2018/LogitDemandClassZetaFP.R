#' Zeta Fixed Point
#'
#' Method for the Logit Demand Class that computes the NE prices given marginal cost and information about consumer utility.
#' This method uses the Zeta Fixed Point of Morrow and Skelos 2010.
#'
#' @param tol when should the fixed point converge
#'
#' @return invisibly returns the object (the method updates the prices, market shares, and derivative matrices directly)
#' @export
#'
#' @examples #NA
ldmkt_zeta_fixed_point <- function(tol, max_iter, rel_tol=1e-12, quietly){
  Jt <- private$Jt
  MC <- as.numeric(private$cjs)
  self$share() # initial shares
  self$Ds_fun() # first derivative matrix

  error <- 1e6
  counter <- 1

  while(TRUE){
    #print(private$Ds[['Lambda_p']])
    ## Update the price from the last iteration using the fixed point equation
    p_prev <- private$Market[['Price']]

    Lambda_inv <- diag(1/diag(private$Ds[['Lambda_p']]), nrow = Jt, ncol = Jt)

    private$Market[['Price']] <- Lambda_inv %*% as.numeric(-private$Market[['Share']]) -
      Lambda_inv %*% private$Ds[['Gamma_p']] %*% (as.numeric(private$Market[['Price']]) - MC) + MC


    ## use the new prices to calculate new shares and stuff
    self$share()
    self$Ds_fun() # first derivative matrix

    ## should the loop stop? have we converged?
    foc <- t(private$Ds[['D_p']]) %*% (as.numeric(private$Market[['Price']]) - MC) + as.numeric(private$Market[['Share']])
    error <- max(abs(foc)) # check for whether the first order condition is 0 for this iteration
    rel_error <- max(abs(private$Market[['Price']] - p_prev)/p_prev)
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

  if(any(as.numeric(private$Market[['Price']]) - MC < 0)){
    stop('price < mc')
  }

  #print(counter)
  invisible(self)
}







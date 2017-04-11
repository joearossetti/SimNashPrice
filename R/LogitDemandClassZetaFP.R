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
ldmkt_zeta_fixed_point <- function(tol){
  Jt <- private$Jt
  MC <- as.numeric(private$cjs)
  self$share() # initial shares
  self$Ds_fun() # first derivative matrix

  error <- 1e6
  while(TRUE){
    #print(private$Ds[['Lambda_p']])
    ## Update the price from the last iteration using the fixed point equation


    Lambda_inv <- diag(1/diag(private$Ds[['Lambda_p']]))

    private$Market[['Price']] <- Lambda_inv %*% as.numeric(-private$Market[['Share']]) -
      Lambda_inv %*% private$Ds[['Gamma_p']] %*% (as.numeric(private$Market[['Price']]) - MC) + MC


    ## use the new prices to calculate new shares and stuff
    self$share()
    self$Ds_fun() # first derivative matrix

    ## should the loop stop? have we converged?
    foc <- t(private$Ds[['D_p']]) %*% (as.numeric(private$Market[['Price']]) - MC) + as.numeric(private$Market[['Share']])
    error <- max(abs(foc)) # check for whether the first order condition is 0 for this iteration
    if(is.na(error)){
     print('error is na')
     print(foc)
    }
    if(error < tol){
      break
    }
  }

  invisible(self)
}







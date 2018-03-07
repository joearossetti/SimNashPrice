#' Share Method
#'
#' Computes the market shares of products using the sum of delta and struct error, the utility of the outside option,
#' the prices, and the derivative of the utility function wrt prices.
#'
#' @return invisible return self
#' @export
#'
#' @examples #NA
ldmkt_share <- function(){
  delta <- private$ujs - private$Deriv_price * private$Market[['Price']]
  S <- exp(delta)
  private$Market[['Share']] <- S / (exp(private$U_out_opt) + sum(S))
  invisible(self)
}


#' Jacobian of market shares
#'
#' compute matrices related to the jacobian of market shares with respect to prices
#'
#' @return invisibly return self
#' @export
#'
#' @examples #NA
ldmkt_Ds_fun <- function(){
  S <- as.numeric(private$Market[['Share']])
  Lambda_p <- -private$Deriv_price * diag(S)
  Gamma_p <- private$Deriv_price * (S %*% t(S)) * private$O
  D_p <- Lambda_p + Gamma_p
  Ds <- list(Lambda_p, Gamma_p, D_p)
  names(Ds) <- c("Lambda_p", "Gamma_p", "D_p")
  class(Ds) <- "LogitShareJacobian"
  private$Ds <- Ds
  invisible(self)
}

#' Firm Profits
#'
#' Compute the profits of the firms, set this private attribute, and return the profits.
#'  If there are no values for the fixed part of marginal cost then
#'
#' @return return the firm profits
#' @export
#'
#' @examples #NA
ldmkt_firm_profits <- function(){

  if(any(private$MarketMkt[['Markups']]==0)){
    if(any(private$MarketMkt[['Mc_fixed']]==0)){
      self$markups
    }else{
      self$markupsb
    }
  }

  var_profs <- vector('numeric', length(private$num_firms))
  for(j in 1:private$num_firms){
    f <- private$firm_names[j]
    which_prods <- which(private$MarketMkt[['Firms']]==f)
    var_prof[j] <- sum(as.numeric(private$MarketMkt[['Markups']])*as.numeric(Mkt[['Share']])[which_prods])
  }
  private$firm_profits <- var_profs
  return(var_profs)
}

#' Inclusive Value Logit (Fixed)
#'
#' Compute the inclusive value of consumers (re-computes market shares, does not compute prices)
#'
#' @return return the inclusive value of consumers
#' @export
#'
#' @examples #NA
ldmkt_CVEV <- function(){

  self$share()

  delta <- private$ujs - private$Deriv_price * private$Market[['Price']]
  S <- exp(delta)
  inc_value <- (log(exp(private$U_out_opt) + sum(S)))/private$Deriv_price

  return(inc_value)
}

#' Markup method
#'
#' Compute the markups over marginal cost using the first order condition for NE prices, and current market shares.
#'
#' @return invisible return self
#' @export
#'
#' @examples #NA
ldmkt_markups <- function(){
  private$Market[['Markup']] <- solve(private$Ds[['D_p']]) %*% (-private$Market[['Share']])
  invisible(self)
}

#' Markup Additive
#'
#' Compute markups when marginal cost is known
#'
#' @return invisible return self
#' @export
#'
#' @examples #NA
ldmkt_markupsb <- function(){
  if(is.null(dim(private$Market[['Markup']]))){
    private$Market[['Markup']] <- as.numeric(private$Market[['Price']]) - private$cjs
  }else{
    private$Market[['Markup']] <- t(as.numeric(private$Market[['Price']]) - private$cjs)
  }

  invisible(self)
}

#' Marginal Cost method
#'
#' Compute marginal costs by subtracting the markup from the price
#'
#' @return invisible return self
#' @export
#'
#' @examples #NA
ldmkt_marginal_cost <- function(){
  private$cjs <- private$Market[['Price']] - private$Market[['Markup']]
  invisible(self)
}

#' Get Marginal Costs
#'
#' @return Marginal Costs
#' @export
#'
#' @examples #NA
ldmkt_getCjs<- function(){
  return(private$cjs)
}

#' Get Utilities
#'
#' @return Utilities
#' @export
#'
#' @examples #NA
ldmkt_getUjs<- function(){
  return(private$ujs)
}

#' Get Market data.frame
#'
#' @return Market data.frame
#' @export
#'
#' @examples #NA
ldmkt_getMarket <- function(){
  return(private$Market)
}

#' Get Ownership matrix
#'
#' @return Ownership matrix
#' @export
#'
#' @examples #NA
ldmkt_getOwnership <- function(){
  return(private$O)
}

#' Get Jacobian of shares
#'
#' @return Jacobian of shares
#' @export
#'
#' @examples #NA
ldmkt_getDs <- function(){
  return(private$Ds)
}


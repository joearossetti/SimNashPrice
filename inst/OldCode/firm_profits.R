#' Title Compute Firm Profits
#'
#' @param x a 'Logit_Demand_Market' object
#'
#' @return x with the component 'firm_profits' added, a vector of the profits of each firm given the data in x
#' @export
#'
#' @examples
#' #NA
firm_profits <- function(x){
  profits <- vector('numeric', length=length(unique(x$Market$Firms)))
  for(j in 1:length(unique(x$Market$Firms))){
    f <- unique(x$Market$Firms)[j]
    profits[j] <- sum((x$Market$Price[which(x$Market$Firms==f)]-x$Market$Mc[which(x$Market$Firms==f)])*x$Market$Share[which(x$Market$Firms==f)])
  }
  names(profits) <- unique(x$Market$Firms)
  x$firm_profits <- profits
  return(x)
}


#' Compute Eq. Markups from BLP Markup equation
#'
#' @param x a 'Logit_Demand_Market' object
#'
#' @return x with equilibrium markups implied by the firms first order condition
#' @export
#'
#' @examples #NA
markups <- function(x){
  x$Market$Markup <- solve(x$Ds$D_p) %*% (-x$Market$Share)
  return(x)
}

#' Compute Marginal Costs
#'
#' @param x a 'Logit_Demand_Market' object
#'
#' @return x with the Market$Mc changed to Market$Price - Market$Markup
#' @export
#'
#' @examples #NA
marginal_cost <- function(x){
  x$Market$Mc <- x$Market$Price - x$Market$Markup
  return(x)
}

#' Method for updating Mc
#'
#' @param x a 'logit_demand_market' object
#'
#' @return x with Mc set to Mc_fixed + Mc_error
#' @export
#'
#' @examples
#' #NA
Mc_method <- function(x){
  x$Market$Mc <- x$Market$Mc_fixed + x$Market$Mc_error
  return(x)
}

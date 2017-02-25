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

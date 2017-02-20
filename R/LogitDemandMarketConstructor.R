#' Constructor of Logit Demand Objects
#'
#' @param Firms logical vector with TRUE indicating ownership by firm 1 for each product
#' @param Delta vector of the fixed part of consumer utility for each product in the market
#' @param Mc vector of the fixed part of marginal cost for each product in the market
#' @param Struct_error vector giving the structural error in the consumers utility function for each product
#' @param Deriv_price a constand (eventually a function) that will compute the derivated of consumer utility with respect to price
#'
#' @return an object (list) with entries: Market, a data.frame with Price, Share, Delta, Firms, Mc, and Struct_Err;
#'  Deriv_price; O the ownership matrix;
#'  Ds a place holder for the jacobian of the shares;
#'  and Jt the number of products
#' @export
#'
#' @examples set.seed(1234)
#'
#'## Parameters of demand function
#'alpha <- 4
#'beta <- c(2, 3.5)
#'gamma <- c(0.5, 0.3)
#'
#'## Make a list of factor level
#'char_levels <- list(char1 = c(0,1), char2 = c(0,1))
#'
#'## Make list of all potential products
#'XP <- expand.grid(char_levels)
#'J <- dim(XP)[1]
#'
#'XP <- data.frame(XP, prodname=1:J)
#'
#'a1 <- sample(c(0,1), 4, replace = TRUE)
#'a2 <- sample(c(0,1), 4, replace = TRUE)
#'
#'Xi <- rnorm(1, 0, 1)
#'Omega <- rnorm(1, 0, 1)
#'
#'my_prod_table <- data.frame(j = XP$prodname, delta = (cbind(XP$char1, XP$char2) %*% beta), mc = (cbind(XP$char1, XP$char2) %*% gamma))
#'
#'my_mkt_prods <- rbind(my_prod_table[which(a1==1),], my_prod_table[which(a2==1),])
#'my_mkt_prods <- data.frame(my_mkt_prods, firm = c(1,1,1,0,0))
#'
#'my_ldm_obj <- logit_demand_market(my_mkt_prods$firm, my_mkt_prods$delta, Mc = my_mkt_prods$mc, Struct_error = Xi, Deriv_price = alpha)
logit_demand_market <- function(Firms, Delta, Mc, Struct_error, Deriv_price){
  Jt <- length(Firms)
  Market <- data.frame('Price'=rep(0, Jt),
                       'Share'=rep(0, Jt),
                       'Firms'=Firms,
                       'Delta'=Delta,
                       'Mc'=Mc,
                       'Struct_Err'=Struct_error)
  Logit_Demand_Market <- list("Market"=Market,
                              "Deriv_price"=Deriv_price,
                              'O'=O_fun(Market$Firms, Jt),
                              'Ds' = NULL,
                              'Jt' = Jt)
  class(Logit_Demand_Market) <- 'Logit_Demand_Market'
  return(Logit_Demand_Market)
}

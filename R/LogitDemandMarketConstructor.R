#' Constructor of Logit Demand Objects
#'
#' @param Firms logical vector with TRUE indicating ownership by firm 1 for each product (required)
#' @param Delta vector of the fixed part of consumer utility for each product in the market
#' @param Mc vector of the fixed part of marginal cost for each product in the market
#' @param Struct_error vector giving the structural error in the consumers utility function for each product
#' @param Deriv_price a constant (eventually a function) that will compute the derivated of consumer utility with respect to price
#' @param Price vector of prices
#' @param Share vector of shares
#' @param Markup vector of markups
#' @param u_out_opt utility of the outside option defualt is the tradition 0 normalization (scalar)
#' @param Market_Size market size (scalar) not used anywhere, but useful for interpreting results
#'
#' @return an object (list) with entries: Market, a data.frame with Price, Share, Delta, Firms, Mc, and Struct_Err;
#'  Deriv_price; O the ownership matrix;
#'  Ds a place holder for the jacobian of the shares;
#'  and Jt the number of products. (All except Firms )
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
logit_demand_market <- function(Firms,
                                Delta=NULL,
                                Mc=NULL,
                                Price=NULL,
                                Share=NULL,
                                Struct_error=NULL,
                                Markup=NULL,
                                Deriv_price=1,
                                u_out_opt=0,
                                Market_Size=1){

  Jt <- length(Firms)
  if(is.null(Delta)){
    Delta <- rep(0, Jt)
  }
  if(is.null(Mc)){
    Mc <- rep(0, Jt)
  }
  if(is.null(Price)){
    Price <- rep(0, Jt)
  }
  if(is.null(Share)){
    Share <- rep(0, Jt)
  }
  if(is.null(Struct_error)){
    Struct_error <- rep(0, Jt)
  }
  if(is.null(Markup)){
    Markup <- rep(0, Jt)
  }

  Market <- data.frame('Price'=Price,
                       'Share'=Share,
                       'Firms'=Firms,
                       'Delta'=Delta,
                       'Mc'=Mc,
                       'Struct_Err'=Struct_error)
  Logit_Demand_Market <- list("Market"=Market,
                              "Deriv_price"=Deriv_price,
                              "u_out_opt"=u_out_opt,
                              'O'=O_fun(Market$Firms, Jt),
                              'Ds' = NULL,
                              'Jt' = Jt)
  class(Logit_Demand_Market) <- 'Logit_Demand_Market'
  return(Logit_Demand_Market)
}

#' Logit Demand Class Constructor
#'
#' This is the constructor for an R6 class, it does not have a return that will be useable. It defualts all arguements to 0 if not given.
#' Objects of the Logit Demand Class carry all needed information to find counterfactual prices, and market shares, using their methods.
#'
#' @param Firms vector giving which firm owns each product in the market
#' @param Prod_ids unique ids for the products in the market
#' @param Delta fixed part of consumer utility for each product
#' @param Price the prices of each product
#' @param Share the market share of each product
#' @param Struct_error the econometric error for each product
#' @param Mc_fixed the fixed part of marginal cost for each product
#' @param Mc_error the econometric error in marginal cost
#' @param Markup the markup for each product
#' @param Deriv_price the derivative of the consumers utility function with respect to price
#' @param U_out_opt the utility of the outside option (default = 0)
#' @param Market_Size the size of the market (default to 1)
#'
#' @return has no return
#' @export
#'
#' @examples #NA
ldmkt_construct <- function(Firms,
                            Prod_ids=NULL,
                            Delta=NULL,
                            Price=NULL,
                            Share=NULL,
                            Struct_error=NULL,
                            Mc_fixed=NULL,
                            Mc_error=NULL,
                            Markup=NULL,
                            Deriv_price=1,
                            U_out_opt=0,
                            Market_Size=1){

  Jt <- length(Firms)
  if(is.null(Prod_ids)){
    Prod_ids <- rep(0, Jt)
  }
  if(is.null(Delta)){
    Delta <- rep(0, Jt)
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
  if(is.null(Mc_error)){
    Mc_error <- rep(0, Jt)
  }
  if(is.null(Mc_fixed)){
    Mc_fixed <- rep(0, Jt)
  }
  if(is.null(Markup)){
    Markup <- rep(0, Jt)
  }

  private$Market <- data.frame('Price'=Price,
                               'Share'=Share,
                               'Firms'=Firms,
                               'Prod_ids'=Prod_ids,
                               'Delta'=Delta,
                               'Mc_error'=Mc_error,
                               'Mc_fixed'=Mc_fixed,
                               'Struct_Err'=Struct_error,
                               'Markup' = Markup)

  private$ujs <- Delta + Struct_error
  private$cjs <- exp(Mc_error+Mc_fixed)
  private$Jt <- Jt
  private$Market_size <- Market_Size
  private$Deriv_price <- Deriv_price
  private$U_out_opt <- U_out_opt
  private$O <- O_fun(Firms, Jt)
  private$firm_names <- unique(Firms)
  private$num_firms <- length(private$firm_names)
}

#' Random Coef. Logit Market Constructor
#'
#' Construct the random coefficient logit objects.
#'
#' @param Firms  vector giving which firm owns each product in the market
#' @param Muij_mat number of products x number of simulation draws matrix of random part of consumer utility
#' @param Aij_mat number of products x number of simulation draws matrix of random part of consumer's coefficient on price
#' @param Prod_ids unique ids for the products in the market
#' @param Delta fixed part of consumer utility for each product
#' @param Price the prices of each product
#' @param Share the market share of each product
#' @param Struct_error the econometric error for each product
#' @param Mc_fixed the fixed part of marginal cost for each product
#' @param Mc_error the econometric error in marginal cost
#' @param Markup the markup for each product
#' @param Alpha the fixed part of the derivative of the consumers utility function with respect to price
#' @param U_out_opt the utility of the outside option (default = 0)
#' @param Market_Size the size of the market (default to 1)
#'
#' @return has no return
#' @export
#'
#' @examples #NA
rldmkt_construct <- function(Firms,
                            Muij_mat,
                            Aij_mat,
                            Prod_ids=NULL,
                            Delta=NULL,
                            Price=NULL,
                            Share=NULL,
                            Struct_error=NULL,
                            Mc_fixed=NULL,
                            Mc_error=NULL,
                            Markup=NULL,
                            Alpha=1,
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
  private$muijs <- Muij_mat
  private$aijs <- Aij_mat
  private$ujs <- Delta + Struct_error
  private$uijs <- private$ujs + private$muijs
  private$cjs <- exp(Mc_error)*Mc_fixed
  private$Jt <- Jt
  private$Market_size <- Market_Size
  private$Alpha <- Alpha
  private$Deriv_price <- Alpha + private$aijs
  private$U_out_opt <- U_out_opt
  private$O <- O_fun(Firms, Jt)
  private$firm_names <- unique(Firms)
  private$num_firms <- length(private$firm_names)
}

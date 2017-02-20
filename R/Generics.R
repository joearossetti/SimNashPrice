#' Compute Jacobian of Market Shares
#'
#' @param x an object that has a Ds_fun method
#'
#' @return x with the Jacobian of shares attached
#' @export
#' @seealso \code{\link{Ds_fun.Logit_Demand_Market}}
#' @examples #Ds_fun(x)
Ds_fun <- function(x){
  UseMethod('Ds_fun')
}

#' Compute Market shares
#'
#' @param x an object with a share method
#'
#' @return x with the market shares updated
#' @export
#' @seealso \code{\link{share.Logit_Demand_Market}}
#' @examples #share(x)
share <- function(x){
  UseMethod('share')
}

#' Zeta Fixed Point Iterations
#'
#' @param x object with a zeta fixed point method
#'
#' @return x updated with prices using the zeta fixed point iterations
#' @export
#' @seealso \code{\link{zeta_fixed_point.Logit_Demand_Market}}
#' @examples #zeta_fixed_point(x)
zeta_fixed_point <- function(x){
  UseMethod('zeta_fixed_point')
}

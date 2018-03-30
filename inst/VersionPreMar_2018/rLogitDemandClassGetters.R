#' Get Method for Aijs
#'
#' @return Aijs 1 x draws vector of random part of coeficient on price
#' @export
#'
#' @examples #NA
rldmkt_getAijs <- function(){
  return(private$aijs)
}

#' Get Method for Uijs
#'
#' @return Uijs number of products x draws matrix of the sume of the fixed part of utility, random coefficients on the product characteristics, and the structural errors
#' @export
#'
#' @examples #NA
rldmkt_getUijs <- function(){
  return(private$uijs)
}

#' Get Method for Alpha
#'
#' @return fixed part of coefficient on price
#' @export
#'
#' @examples #NA
rldmkt_getAlpha <- function(){
  return(private$Alpha)
}

#' Get method for Coefficient on price
#'
#' @return Deriv_price, the sum of the random and fixed parts of the coefficient on price
#' @export
#'
#' @examples #NA
rldmkt_getDerivPrice <- function(){
  return(private$Deriv_price)
}

#' Get method for random part of utility
#'
#' @return Muijs, number of products x draws matrix of the random parts of the random coefficients on product characteristics
#' @export
#'
#' @examples #NA
rldmkt_getMuijs <- function(){
  return(private$muijs)
}

#' Get method for indiv. shares
#'
#' @return Sijs
#' @export
#'
#' @examples #NA
rldmkt_getSijs <- function(){
  return(private$Si)
}


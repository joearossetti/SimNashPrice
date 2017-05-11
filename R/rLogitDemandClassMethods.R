#' Random Coef. Logit Share Function
#'
#' Method for rldmkt class that simulates market shares.
#'
#' @return invisibly self
#' @export
#'
#' @examples #NA
rldmkt_share <- function(){
  Index <- private$uijs - private$Market[['Price']] %*% private$Deriv_price
  S <- exp(Index)
  Denom <- exp(private$U_out_opt) + colSums(S)
  private$Si <- S / matrix(rep(Denom, private$Jt), nrow = private$Jt, ncol = length(Denom), byrow = TRUE)
  private$Market[['Share']] <- rowMeans(private$Si)
  invisible(self)
}

#' Random Ceof. Jacobian of Market Shares
#'
#' Method for rldmkt class that simulates the jaobian of market shares.
#'
#' @return invisibly return self
#' @export
#'
#' @examples #NA
rldmkt_Ds_fun <- function(){
  Jt <- private$Jt
  Lambda_p <- -diag(rowMeans(private$Si*as.numeric(private$Deriv_price)), nrow = Jt, ncol = Jt)
  Gamma_p <- gamma_helper(Sr = private$Si, ar=private$Deriv_price, Or=private$O)
  D_p <- Lambda_p + Gamma_p
  Ds <- list(Lambda_p, Gamma_p, D_p)
  names(Ds) <- c("Lambda_p", "Gamma_p", "D_p")
  class(Ds) <- "LogitShareJacobian"
  private$Ds <- Ds
  invisible(self)
}



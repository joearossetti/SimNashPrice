#' Compute Shares assuming Logit Demand
#'
#' @param x a object of class 'Logit_Demand_Market'
#'
#' @return x updated with market shares computed using prices and parameters in x
#' @export
#'
#' @examples
#' set.seed(1234)
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
#'my_ldm_obj <- share(my_ldm_obj)
share.Logit_Demand_Market <- function(x){
  delta <- x$Market$Delta - x$Deriv_price * x$Market$Price + x$Market$Struct_Err
  S <- exp(delta)
  S <- S / (1 + sum(S))
  x$Market$Share <- S
  return(x)
}

#' Compute Jacobian of Shares assuming Logit Demand
#'
#' @param x a object of class 'Logit_Demand_Market'
#'
#' @return x updated with market shares computed using prices and parameters in x. This function depends on the shares, so update them first!
#' @export
#'
#' @examples
#'set.seed(1234)
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
#'my_ldm_obj <- share(my_ldm_obj)
#'my_ldm_obj <- Ds_fun(my_ldm_obj)
Ds_fun.Logit_Demand_Market <- function(x){
  S <- as.numeric(x$Market$Share)
  Lambda_p <- -x$Deriv_price * diag(S)
  Gamma_p <- x$Deriv_price * (S %*% t(S)) * x$O
  D_p <- Lambda_p + Gamma_p
  Ds <- list(Lambda_p, Gamma_p, D_p)
  names(Ds) <- c("Lambda_p", "Gamma_p", "D_p")
  class(Ds) <- "LogitShareJacobian"
  x$Ds <- Ds
  return(x)
}



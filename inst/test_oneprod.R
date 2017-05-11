test_market1 <- Markets$market_obj[[18]]

uijs <- test_market1$getUijs()
price <- test_market1$getMarket()[['Price']]
deriv_price <- test_market1$getDerivPrice()
u_opt_out <- 0

Index <-uijs - price %*% deriv_price
S <- exp(Index)
Denom <- exp(u_opt_out) + colSums(S)
Si <- S / Denom
share <- rowMeans(Si)

Jt <- dim(test_market1$getMarket())[1]

Lambda_p <- -diag(rowMeans(Si*as.numeric(deriv_price)), nrow = Jt, ncol = Jt)


Gamma_p <- gamma_helper(Sr = private$Si, ar=private$Deriv_price, Or=private$O)
D_p <- Lambda_p + Gamma_p
Ds <- list(Lambda_p, Gamma_p, D_p)
names(Ds) <- c("Lambda_p", "Gamma_p", "D_p")
class(Ds) <- "LogitShareJacobian"
private$Ds <- Ds
invisible(self)

test_market2 <- Markets$market_obj[[12]]

uijs <- test_market2$getUijs()
price <- test_market2$getMarket()[['Price']]
deriv_price <- test_market1$getDerivPrice()
u_opt_out <- 0

Index <-uijs - price %*% deriv_price
S <- exp(Index)
Denom <- exp(u_opt_out) + colSums(S)
Si <- S / Denom
share <- rowMeans(Si)

Lambda_p <- -diag(rowMeans(Si*as.numeric(deriv_price)))

Jt <- dim(test_market1$getMarket())[1]

Lambda_p <- -diag(rowMeans(Si*as.numeric(deriv_price)), nrow = Jt, ncol = Jt)


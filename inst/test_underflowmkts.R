test_market1 <- Markets$market_obj[[961]]

uijs <- test_market1$getUijs()
price <- test_market1$getMarket()[['Price']]
price <- rep(0, length(price))
deriv_price <- test_market1$getDerivPrice()
u_opt_out <- 0
O <- test_market1$getOwnership()
Jt <- length(price)
MC <- test_market1$getCjs()

Index <- uijs - price %*% deriv_price
S <- exp(Index)
Denom <- exp(u_opt_out) + colSums(S)
Si <- S / Denom
share <- rowMeans(Si)

Lambda_p <- -diag(rowMeans(Si*as.numeric(deriv_price)), nrow = Jt, ncol = Jt)
Gamma_p <- gamma_helper(Sr = Si, ar=deriv_price, Or=O)
D_p <- Lambda_p + Gamma_p
Ds <- list(Lambda_p, Gamma_p, D_p)
names(Ds) <- c("Lambda_p", "Gamma_p", "D_p")
class(Ds) <- "LogitShareJacobian"

Lambda_inv <- diag(1/diag(Ds[['Lambda_p']]), nrow = Jt, ncol = Jt)

price2 <- Lambda_inv %*% as.numeric(share) -
  Lambda_inv %*% Ds[['Gamma_p']] %*% (as.numeric(price) - MC) + MC

## use the new prices to calculate new shares and stuff
price <- price2

Index <- uijs - price %*% deriv_price
S <- exp(Index)
Denom <- exp(u_opt_out) + colSums(S)
Si <- S / Denom
share <- rowMeans(Si)

Lambda_p <- -diag(rowMeans(Si*as.numeric(deriv_price)), nrow = Jt, ncol = Jt)
Gamma_p <- gamma_helper(Sr = Si, ar=deriv_price, Or=O)
D_p <- Lambda_p + Gamma_p
Ds <- list(Lambda_p, Gamma_p, D_p)
names(Ds) <- c("Lambda_p", "Gamma_p", "D_p")
class(Ds) <- "LogitShareJacobian"

test_market2 <- Markets$market_obj[[1]]

uijs <- test_market2$getUijs()
price <- test_market2$getMarket()[['Price']]
price <- rep(0, length(price))
deriv_price <- test_market2$getDerivPrice()
u_opt_out <- 0
O <- test_market2$getOwnership()
Jt <- length(price)
MC <- test_market2$getCjs()

Index <- uijs - price %*% deriv_price
S <- exp(Index)
Denom <- exp(u_opt_out) + colSums(S)
Si <- S / Denom
share <- rowMeans(Si)

Lambda_p <- -diag(rowMeans(Si*as.numeric(deriv_price)), nrow = Jt, ncol = Jt)
Gamma_p <- gamma_helper(Sr = Si, ar=deriv_price, Or=O)
D_p <- Lambda_p + Gamma_p
Ds <- list(Lambda_p, Gamma_p, D_p)
names(Ds) <- c("Lambda_p", "Gamma_p", "D_p")
class(Ds) <- "LogitShareJacobian"

Lambda_inv <- diag(1/diag(Ds[['Lambda_p']]), nrow = Jt, ncol = Jt)

price2 <- Lambda_inv %*% as.numeric(share) -
  Lambda_inv %*% Ds[['Gamma_p']] %*% (as.numeric(price) - MC) + MC

## use the new prices to calculate new shares and stuff
price <- price2

Index <- uijs - price %*% deriv_price
S <- exp(Index)
Denom <- exp(u_opt_out) + colSums(S)
Si <- S / Denom
share <- rowMeans(Si)

Lambda_p <- -diag(rowMeans(Si*as.numeric(deriv_price)), nrow = Jt, ncol = Jt)
Gamma_p <- gamma_helper(Sr = Si, ar=deriv_price, Or=O)
D_p <- Lambda_p + Gamma_p
Ds <- list(Lambda_p, Gamma_p, D_p)
names(Ds) <- c("Lambda_p", "Gamma_p", "D_p")
class(Ds) <- "LogitShareJacobian"

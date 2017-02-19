share_gen <- function(P, delta, demand_shocks){
  delta <- delta - alpha * P + demand_shocks
  S <- exp(delta)
  S <- S / (1 + sum(S))
  return(S)
}

O_fun <- function(Firm, Jt){
  O_mat <- matrix(ncol = Jt, nrow = Jt, data = 0)
  for(j in 1:Jt){
    for(k in 1:Jt){
      O_mat[j,k] <- Firm[j] == Firm[k]
    }
  }
  return(O_mat)
}

Ds_fun <- function(S, alpha, Own){
  Lambda_p <- -alpha * diag(as.numeric(S))
  Gamma_p <- alpha * (S %*% t(S)) * Own
  D_p <- Lambda_p + Gamma_p
  Ds <- list(Lambda_p, Gamma_p, D_p)
  names(Ds) <- c("Lambda_p", "Gamma_p", "D_p")
  class(Ds) <- "ShareJacobian"
  return(Ds)
}

zeta_fixed_point <- function(P_init, prods, Alpha, Demand_shocks, tol){
  Jt <- length(prods$firm)
  O_mat <- O_fun(prods$firm, Jt)

  error <- 1e6
  p <- P_init
  s <- share_gen(P = p, delta = prods$delta, demand_shocks = Demand_shocks) # initial shares
  Ds <- Ds_fun(S=s, alpha = Alpha, Own = O_mat) # first derivative matrix
  Lambda_inv <- solve(Ds$Lambda_p) # first inverse
  while(error > tol){
    ## Update the price using the fixed point equation
    P1 <- Lambda_inv %*% (-s) - Lambda_inv %*% Ds$Gamma_p %*% (p - prods$mc) + prods$mc
    ## use the new prices to calculate new shares
    s <- share_gen(P = P1, delta = prods$delta, demand_shocks = Demand_shocks)
    Ds <- Ds_fun(S=s, alpha = Alpha, Own = O_mat) # new derivative matrix
    Lambda_inv <- solve(Ds$Lambda_p) # new inverse
    foc <- t(Ds$D_p) %*% (P1 - prods$mc) + s # compute the first order condition
    print(foc)
    error <- max(abs(foc)) # check for whether the first order condition is 0 for this iteration
    print(error)
    print(P1-p)
    p <- P1
  }
  return(p)
}

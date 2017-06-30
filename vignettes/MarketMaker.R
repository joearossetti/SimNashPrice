market_maker_factory <- function(){
  ## Parameters of demand function
  alpha <- 5
  beta <- c(-3, -1)
  gamma <- c(1, 0.5)/10
  B <- 500

  ## Make a list of factor level
  char_levels <- list(char1 = c(0,1), char2 = c(0,1))

  ## Make list of all potential products
  XP <- expand.grid(char_levels)
  J <- dim(XP)[1]

  XP <- data.frame(XP, prodname=1:J)

  my_prod_table <- data.frame(j = XP$prodname, delta = (cbind(XP$char1, XP$char2) %*% beta), mc = (cbind(XP$char1, XP$char2) %*% gamma))

  # pi1 <- seq(from=-1, to=1, by=0.5)[-3]
  # pi2 <- (-2:2)[-3]
  # pi3 <- -2*(-2:2)[-3]
  #
  # Pi_mat <- rbind(pi1, pi2, pi3)

  market_maker <- function(mc_shift){
    a1 <- rep(0,4)
    while(sum(a1)==0){
      a1 <- sample(c(0,1), 4, replace = TRUE)
    }

    a2 <- rep(0,4)
    while(sum(a2)==0){
      a2 <- sample(c(0,1), 4, replace = TRUE)
    }

    my_mkt_prods <- rbind(my_prod_table[which(a1==1),], my_prod_table[which(a2==1),])

    my_mkt_prods <- data.frame(my_mkt_prods, firm = c(rep(1,sum(a1==1)),  rep(0,sum(a2==1))))

    Jm <- dim(my_mkt_prods)[1]

    Xi <- rnorm(Jm, 0, 1)
    Omega <- rnorm(Jm, 0, 1)/100 + (2/100)*mc_shift

    #inc_levels <- sample(1:5, replace = TRUE, size = B, prob = probs_inc)

    #mkt_dis <- sapply(inc_levels, FUN = function(x){y <- rep(0, 5); y[x] <- 1; y[1:4]})

    #mudis <- Pi_mat %*% mkt_dis

    my_alphais <- t(rnorm(B, 0, 1)) # add unobservable normal rv to price coefficient

    beta_is <- cbind(rnorm(500, 0, 1), rnorm(500, 0, 1))

    #beta_is <- mudis[2:3,]

    my_mkt_muis <- as.matrix(XP[,1:2]) %*% t(beta_is)

    my_mkt_muis <- my_mkt_muis[my_mkt_prods$j,]

    my_rldmkt_obj <- rLogit_Demand_Market$new(Firms = my_mkt_prods$firm,
                                              Muij_mat = my_mkt_muis,
                                              Aij_mat = my_alphais,
                                              Prod_ids = my_mkt_prods$j,
                                              Delta = my_mkt_prods$delta,
                                              Mc_fixed = my_mkt_prods$mc,
                                              Mc_error = Omega,
                                              Struct_error =  Xi,
                                              U_out_opt = 0,
                                              Alpha = alpha)
    return(my_rldmkt_obj)
  }
  return(market_maker)
}

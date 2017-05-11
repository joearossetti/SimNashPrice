library(tidyverse)
set.seed(1234)

market_maker_factory2 <- function(){
  ## Parameters of demand function
  alpha <- 4
  beta <- c(-5, -4)
  gamma <- c(1, 0.5)

  ## Make a list of factor level
  char_levels <- list(char1 = c(0,1), char2 = c(0,1))

  ## Make list of all potential products
  XP <- expand.grid(char_levels)
  J <- dim(XP)[1]

  XP <- data.frame(XP, prodname=1:J)

  my_prod_table <- data.frame(j = XP$prodname, delta = (cbind(XP$char1, XP$char2) %*% beta), mc = (cbind(XP$char1, XP$char2) %*% gamma))

  beta_is <- cbind(rnorm(500, 0, 1), rnorm(500, 0, 1))

  my_muis <- as.matrix(XP[,1:2]) %*% t(beta_is)

  my_alphais <- t(rnorm(500, 0, 1))

  market_maker <- function(){
    a1 <- sample(c(0,1), 4, replace = TRUE)
    a2 <- sample(c(0,1), 4, replace = TRUE)

    my_mkt_prods <- rbind(my_prod_table[which(a1==1),], my_prod_table[which(a2==1),])

    my_mkt_prods <- data.frame(my_mkt_prods, firm = c(rep(1,sum(a1==1)),  rep(0,sum(a2==1)))) ## needs to detect this automatically

    Jm <- dim(my_mkt_prods)[1]

    Xi <- rnorm(Jm, 0, 1)
    Omega <- rnorm(Jm, 0, 1)

    my_mkt_muis <- my_muis[my_mkt_prods$j,]

    # my_rldmkt_obj <- rLogit_Demand_Market$new(Firms = my_mkt_prods$firm,
    #                                           Muij_mat = my_mkt_muis,
    #                                           Aij_mat = my_alphais,
    #                                           Prod_ids = my_mkt_prods$j,
    #                                           Delta = my_mkt_prods$delta,
    #                                           Mc_fixed = my_mkt_prods$mc,
    #                                           Mc_error = Xi,
    #                                           Struct_error = Omega,
    #                                           U_out_opt = 0,
    #                                           Alpha = alpha)
    return(my_mkt_prods)
    #return(my_rldmkt_obj)
  }
  return(market_maker)
}


my_market_maker2 <- market_maker_factory()

T_ <- 1000
Markets <- tibble(period = 1:T_, market_obj =map(period, safely(.f=~my_market_maker())))

Markets <- Markets %>% mutate(
  error = map(market_obj, ~is.null(.x$error))
)

Markets %>% dplyr::filter(error==FALSE)

Markets2 <- tibble(period = 1:T_, market_obj =map(period, safely(.f=~my_market_maker2())))


Markets <- Markets %>% mutate(
  market_obj = map(market_obj, safely(.f=~.x$zeta_fixed_point(tol=1e-6)))
)



Markets %>% mutate(
  market_dfs = map(market_obj, .f=~.x$getMarket()%>%select(Prod_ids, Firms, Price, Share))
) %>%
  select(period, market_dfs) %>%
  unnest()

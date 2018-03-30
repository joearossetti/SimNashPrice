lmkt_construct <- function(
  Potential_products,
  Firms,
  Deriv_price = 1,
  Market_size = 1,
  U_opt_out = 0
){
  ## assertthat style error checking

  ## fill in private fields
  private$Potential_products <- Potential_products
  private$firm_names <- as.factor(Firms)
  private$market_size <- Market_size
  private$u_opt_out <- exp(U_opt_out)
  private$num_firms <- length(private$firm_names)
  private$deriv_price <- Deriv_price

  ## move to own set methods
  # private$Active_products <- as.logical(Active_products)
  # private$own_vec <- as.factor(Active_owners)
  # private$costs <- exp(private$Potential_products[['Costs']][private$Active_products] + Cost_shocks)
  # private$deltas <- private$Potential_products[['Tastes']][private$Active_products] + Taste_shocks
  # private$prices <- as.numeric(Prices)
  # private$shares <- as.numeric(Shares)
  invisible(self)
}

rlmkt_construct <- function(
  Potential_products,
  Firms,
  Muij_mat,
  Aij_mat,
  Alpha,
  Market_size = 1,
  U_opt_out = 0
){
  ## assertthat style error checking

  ## fill in private fields
  private$Potential_products <- Potential_products
  private$firm_names <- as.factor(Firms)
  private$market_size <- Market_size
  private$u_opt_out <- exp(U_opt_out)
  private$num_firms <- length(private$firm_names)

  private$Muij_all <- Muij_mat

  private$alpha <- Alpha
  private$aijs <- Aij_mat
  private$deriv_price <- private$alpha + private$aijs
  private$diag_deriv_price <- diag(as.numeric(private$deriv_price))


  invisible(self)
}

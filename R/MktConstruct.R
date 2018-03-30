mkt_construct <- function(
  Potential_products,
  Firms,
  Market_size = 1,
  U_opt_out = 0
){
  ## assertthat style error checking

  ## fill in private fields
  private$Potential_products <- Potential_products
  private$firm_names <- as.character(Firms)
  private$market_size <- Market_size
  private$u_opt_out <- exp(U_opt_out)
  private$num_firms <- length(private$firm_names)

  ## move to own set methods
  # private$Active_products <- as.logical(Active_products)
  # private$own_vec <- as.factor(Active_owners)
  # private$costs <- exp(private$Potential_products[['Costs']][private$Active_products] + Cost_shocks)
  # private$deltas <- private$Potential_products[['Tastes']][private$Active_products] + Taste_shocks
  # private$prices <- as.numeric(Prices)
  # private$shares <- as.numeric(Shares)
  invisible(self)
}

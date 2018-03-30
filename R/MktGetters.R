getPotential_products <- function(){
  private$Potential_products
}

getMarket <- function(){
  list("Prod_ids" = private$active_products,
             "Firms" = private$own_vec,
             "Prices" = private$prices,
             "Shares" = private$shares,
             "Costs" = private$Potential_products[['Costs']][private$active_products],
             "Tastes" = private$Potential_products[['Tastes']][private$active_products],
             "Cost_shocks" = private$cost_shocks,
             "Taste_shocks" = private$taste_shocks)
}

getProfits <- function(){
  profit_vec <- numeric(private$num_firms)
  for(i in 1:private$num_firms){
    is_firm <- private$Which_is_firm_mat[i]
    profit_vec[i] <- sum((private$prices[is_firm] - private$costs[is_firm]) * private$shares[is_firm])
  }
  return(profit_vec)
}

getMarkups <- function(){
  private$prices - private$costs
}

getPrices <- function(){
  private$prices
}

getCosts <- function(){
  private$costs
}

getShares <- function(){
  private$shares
}

getMeanUs <- function(){
  private$deltas
}

getWhichFirm <- function(a_firm_id){
  ## check valid
  private$Which_is_firm_mat[which(private$firm_names==a_firm_id)]
}

getDs <- function(){
  list('Lambda' = private$Lambda, 'Gamma' = private$Gamma, 'Ds' = private$Ds, 'O_mat' = private$O_mat)
}

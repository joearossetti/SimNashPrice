setPrice <- function(Prices){
  ## need to check if valid...

  private$prices <- as.numeric(Prices)
  invisible(self)
}

setShares <- function(Shares){
  ## need to check if valid...

  private$shares <- as.numeric(Shares)
  invisible(self)
}

setShocks <- function(Cost_shocks=0, Taste_shocks=0){
  ## need to check if valid...
  private$cost_shocks <- as.numeric(Cost_shocks)
  private$taste_shocks <- as.numeric(Taste_shocks)
  private$costs <- exp(private$Potential_products[['Costs']][private$active_products] + private$cost_shocks)
  private$deltas <- private$Potential_products[['Tastes']][private$active_products] + private$taste_shocks
  invisible(self)
}

updateActivity <- function(){

  number_prods_per_firm <- apply(X = private$A_mat, MARGIN = 2, FUN = sum)

  private$Jt <- sum(number_prods_per_firm)

  temp <- c(0,cumsum(number_prods_per_firm))

  private$own_vec <- character(length = private$Jt)
  for(i in (1:private$num_firms + 1)){
    private$own_vec[(temp[i-1]+1):temp[i]] <- rep(private$firm_names[i-1], number_prods_per_firm[i-1])
  }

  private$Which_is_firm_mat <- matrix(FALSE, nrow = private$Jt, ncol = private$num_firms)
  for(i in 1:private$num_firms){
    private$Which_is_firm_mat[,i] <- private$own_vec == private$firm_names[i]
  }

  private$active_products <- integer(private$Jt)
  for(i in 1:private$num_firms){
    private$active_products[private$Which_is_firm_mat[,i]] <- private$Potential_products[['Prod_ids']][private$A_mat[,i]]
  }

  private$are_prods_active <- TRUE
}

setActiveProds <- function(Activity_matrix){
  ## need to check if valid...

  private$A_mat <- Activity_matrix
  self$updateActivity()
  self$Ownership()
  invisible(self)
}

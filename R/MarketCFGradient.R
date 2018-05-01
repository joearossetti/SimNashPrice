mktCF_gradient <- function(base_set, prod_index, firm_to_change, set_grad){
  if(all(base_set==FALSE)){
    ## firm isn't offering any products tp = 0
    base_tp <- 0
  }else{
    base_tp <- self$exec_cf(prod_state = base_set, firm_to_change = firm_to_change, MEM = TRUE)
  }
  temp_set <- base_set
  if(base_set[prod_index]==TRUE){
    ## if product is in base_set then drop it
    temp_set[prod_index] <- FALSE
    new_tp <- self$exec_cf(prod_state = temp_set, firm_to_change = firm_to_change, MEM = TRUE)
    return(new_tp-base_tp)
  }else{
    ## if product is NOT in base_set then add it
    temp_set[prod_index] <- TRUE
    new_tp <- self$exec_cf(prod_state = temp_set, firm_to_change = firm_to_change, MEM = TRUE)
    if(set_grad==TRUE){
      ## set grad returns the gain from adding the product to get the base set
      return(base_tp-new_tp)
    }else{
      ## if set grad is false what is the loss from dropping the product
      return(new_tp-base_tp)
    }
  }
}

mktCF_gradients <- function(base_set, firm_to_change, set_grad, prod_indexes=NULL){
  if(is.null(prod_indexes)){
    ## if not specified compute gradient for all product indexes
    prod_indexes <- 1:nrow(private$current_A_mat)
  }
  purrr::map_dbl(prod_indexes, .f=~self$gradient(base_set=base_set, prod_index=.x, firm_to_change=firm_to_change, set_grad=set_grad))
}

mktCF_curvature <- function(base_set, firm_to_change, set_grad){
  numerator <- self$gradients(base_set = base_set, firm_to_change = firm_to_change, set_grad=set_grad)
  denominator <- self$gradients(base_set = rep(FALSE, nrow(private$current_A_mat)), firm_to_change = firm_to_change, set_grad=set_grad)
  return(numerator/denominator)
}

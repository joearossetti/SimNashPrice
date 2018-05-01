mktCF_exec_cf <- function(prod_state, firm_to_change, MEM){
  if(MEM==TRUE){
    private$exec_cf_count <- private$exec_cf_count + 1
  }

  if(all(prod_state==FALSE)){
    return(0)
  }

  firm_change_location <- which(private$firm_names==firm_to_change)

  private$current_A_mat[,firm_change_location] <- prod_state

  private$mkt_obj$setActiveProds(Activity_matrix = private$current_A_mat)

  num_prods <- sum(private$current_A_mat)

  private$mkt_obj$setPrices(Prices = rep(0, num_prods))
  private$mkt_obj$setShares(Shares = rep(0, num_prods))
  private$mkt_obj$setShocks(Cost_shocks = rep(0, num_prods),
                          Taste_shocks = rep(0, num_prods))

  #print(private$mkt_obj$getMarket())

  if(private$type_mkt=='rlmkt'){
    private$mkt_obj$updateUijs()
  }

  private$mkt_obj$Ownership()
  Jt <- private$mkt_obj$getJt()

  if(MEM==TRUE){
    temp_hash <- digest::digest(private$current_A_mat)

    if(hash::has.key(temp_hash, private$tp_cache)){
      private$tp_cache[[temp_hash]][['visits']] <- private$tp_cache[[temp_hash]][['visits']] + 1

      exp_prof_val <- private$tp_cache[[temp_hash]][['exp_prof_val']]
      tp <- exp_prof_val[firm_change_location] - private$fixed_cost_param*num_prods

      self$hash_manager() # before returning run the hash manager
      return(tp)
    }else{
      exp_prof_val <- private$mkt_obj$exp_profs(mc_error_mat = private$Error_mats[['mc_error_mat']][,1:Jt],
                                                      struct_error_mat = private$Error_mats[['struct_error_mat']][,1:Jt],
                                                      draws = private$num_draws,
                                                      tol = private$ZFP_opts$tol,
                                                      Max_iter = private$ZFP_opts$Max_iter,
                                                rel_tol = private$ZFP_opts$rel_tol)

      tp <- exp_prof_val[firm_change_location] - private$fixed_cost_param*num_prods
      private$tp_cache[[temp_hash]] <- list('exp_prof_val' = tp,
                                            'count' = private$exec_cf_count,
                                            'visits' = 0L)

      self$hash_manager() # before returning run the hash manager
      return(tp)
    }
  }else{
    exp_prof_val <- private$mkt_obj$exp_profs(mc_error_mat = private$Error_mats[['mc_error_mat']][,1:Jt],
                                              struct_error_mat = private$Error_mats[['struct_error_mat']][,1:Jt],
                                              draws = private$num_draws,
                                              tol = private$ZFP_opts$tol,
                                              Max_iter = private$ZFP_opts$Max_iter,
                                              rel_tol = private$ZFP_opts$rel_tol)
    tp <- exp_prof_val[firm_change_location] - private$fixed_cost_param*num_prods
    return(tp)
  }
}

#' @importFrom foreach %dopar%
mktCF_batch_exec_cf <- function(prod_states, firm_to_change, CLUSTER){
  set_up_A_mat <- function(prod_state, firm_to_change){
    A_mat <- private$current_A_mat
    A_mat[,which(private$firm_names==firm_to_change)] <- prod_state
    return(A_mat)
  }

  list_of_A_mats <- purrr::map(prod_states, .f =~set_up_A_mat(prod_state = .x, firm_to_change = firm_to_change))
  digested <- purrr::map_chr(list_of_A_mats, .f=digest::digest)
  in_cache_check <- purrr::map_lgl(digested, .f=~has.key(.x, private$tp_cache))

  ## check if prod states are in the cache first
  list_of_new_states <- prod_states[!in_cache_check]
  list_of_new_digested <- digested[!in_cache_check]
  list_old_digested <- digested[in_cache_check]

  if(length(list_old_digested)>0){
    purrr::walk(list_old_digested, .f=function(X){private$tp_cache[[X]][['visits']] <- private$tp_cache[[X]][['visits']] + 1})
  }

  num_new_states <- length(list_of_new_states)

  ## loop over prod_states not in the cache and store the results
  if(num_new_states>0){
    my_cluster <- parallel::makeCluster(CLUSTER, type = 'FORK')
    doParallel::registerDoParallel(my_cluster)
    list_results <- foreach::foreach(i=list_of_new_states, .export = c("self"), .multicombine = TRUE) %dopar% self$exec_cf(prod_state=i, firm_to_change=firm_to_change, MEM=FALSE)
    parallel::stopCluster(my_cluster)

    store_fun <- function(exp_prof_val, temp_hash){
      private$exec_cf_count <- private$exec_cf_count + 1

      private$tp_cache[[temp_hash]] <- list('exp_prof_val' = exp_prof_val,
                                            'count' = private$exec_cf_count,
                                            'visits' = 0L)
    }

    purrr::walk2(.x=list_results, .y=list_of_new_digested, .f=~store_fun(exp_prof_val = .x, temp_hash = .y))
  }
  self$hash_manager()
}

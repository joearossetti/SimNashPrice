mktCF_exec_cf <- function(prod_state, firm_to_change, MEM){
  private$current_A_mat[,which(private$firm_names==firm_to_change)] <- prod_state

  private$mkt_obj$setActiveProds(Activity_matrix = private$current_A_mat)

  num_prods <- sum(private$current_A_mat)

  private$mkt_obj$setPrices(Prices = rep(0, num_prods))
  private$mkt_obj$setShares(Shares = rep(0, num_prods))
  private$mkt_obj$setShocks(Cost_shocks = rep(0, num_prods),
                          Taste_shocks = rep(0, num_prods))

  if(private$type_mkt=='rlmkt'){
    private$mkt_obj$updateUijs()
  }

  private$mkt_obj$Ownership()

  if(MEM==TRUE){
    temp_hash <- digest::digest(private$current_A_mat)

    if(has.key(temp_hash, private$tp_cache)){
      return(private$tp_cache[[temp_hash]])
    }else{
      exp_prof_val <- private$mkt_obj$exp_profs(mc_error_mat = private$Error_mats[['mc_error_mat']],
                                                      struct_error_mat = private$Error_mats[['struct_error_mat']],
                                                      draws = private$num_draws,
                                                      tol = private$ZFP_opts$tol,
                                                      Max_iter = private$ZFP_opts$Max_iter,
                                                rel_tol = private$ZFP_opts$rel_tol)

      private$tp_cache[[temp_hash]] <- exp_prof_val

      return(exp_prof_val)
    }
  }
}

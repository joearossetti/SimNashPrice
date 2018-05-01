mktCF_construct <- function(Firm_id,
                            fixed_cost_per_prod,
                            Firm_Names,
                            A_mat,
                            ZFP_opts,
                            Error_mats,
                            market_type, hash_size=1000L, ...){
  if(market_type=='rlmkt'){
    private$mkt_obj <- RLMarket$new(...)
  }else{
    private$mkt_obj <- LMarket$new(...)
  }
  private$current_A_mat <- A_mat
  private$firm_perspective <- Firm_id
  private$fixed_cost_param <- fixed_cost_per_prod
  private$firm_names <- Firm_Names
  private$Error_mats <- Error_mats
  private$ZFP_opts <- ZFP_opts
  private$type_mkt <- market_type
  private$num_draws <- dim(private$Error_mats$mc_error_mat)[1]
  private$tp_cache <- hash::hash()
  private$hash_limit <- hash_size
  private$num_pot_prods <- dim(private$current_A_mat)[1]
}


mktCF_getMkt <- function(){
  return(private$mkt_obj)
}

mktCF_getTp_cache <- function(){
  return(private$tp_cache)
}

mktCF_hash_manager <- function(){
  if(private$exec_cf_count>=floor(private$hash_limit/2)){
    ## have run enough to potentially use up more than 1/10th of the hash limit so check if we are close to the limit

    overflow <- length(private$tp_cache) - (private$hash_limit-1)

    if(overflow>=0){
      ## we are within 1 of the hash limit so delete any values computed the first half of the run visited only once
      if(overflow>floor(private$hash_limit/10)){
        keys_to_delete <- names(purrr::discard(as.list(private$tp_cache), .p=~.x[['count']]<overflow))
        hash::del(x = keys_to_delete, hash = private$tp_cache)
      }else{
        keys_to_delete <- names(purrr::discard(as.list(private$tp_cache), .p=~.x[['count']]<floor(private$hash_limit/10)))
        hash::del(x = keys_to_delete, hash = private$tp_cache)
      }

      invisible(self)
    }else{
      ## we were not close to the hash_limit so do nothing
      invisible(self)
    }
  }else{
    ## haven't even run for 1/2th the hash limit: do nothing
    invisible(self)
  }
}



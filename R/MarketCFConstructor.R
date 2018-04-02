mktCF_construct <- function(Firm_id,
                            fixed_cost_per_prod,
                            Firm_Names,
                            A_mat,
                            ZFP_opts,
                            Error_mats,
                            market_type, ...){
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
}


mktCF_getMkt <- function(){
  return(private$mkt_obj)
}

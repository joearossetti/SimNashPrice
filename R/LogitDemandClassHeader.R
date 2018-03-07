#' @include OwnershipMatrix.R LogitDemandClassConstructor.R LogitDemandClassMethods.R LogitDemandClassZetaFP.R LogitDemandClassExpProf.R
#' @export
Logit_Demand_Market <- R6::R6Class(
  "ldmkt",
  public = list(
    initialize = ldmkt_construct,
    share = ldmkt_share,
    getMarket = ldmkt_getMarket,
    getOwnership = ldmkt_getOwnership,
    getDs = ldmkt_getDs,
    Ds_fun = ldmkt_Ds_fun,
    zeta_fixed_point = ldmkt_zeta_fixed_point,
    exp_profits = ldmkt_exp_profits,
    exp_profits_mat = ldmkt_exp_profits_mat,
    firm_profits_fun = ldmkt_firm_profits,
    markups = ldmkt_markups,
    markupsb = ldmkt_markupsb,
    marginal_cost = ldmkt_marginal_cost,
    getUjs = ldmkt_getUjs,
    getCjs = ldmkt_getCjs,
    CVEV = ldmkt_CVEV
  ),
  private = list(
    Market = NULL,
    Jt = NULL,
    Market_size = NULL,
    Deriv_price = NULL,
    U_out_opt = NULL,
    firm_profits = NULL,
    O = NULL,
    Ds = NULL,
    ujs = NULL,
    cjs = NULL,
    num_firms = NULL,
    firm_names= NULL
  )
)

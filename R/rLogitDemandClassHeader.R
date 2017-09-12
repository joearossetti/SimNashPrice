#' @include RcppExports.R OwnershipMatrix.R rLogitDemandClassConstructor.R rLogitDemandClassGetters.R rLogitDemandClassMethods.R
#' @export
rLogit_Demand_Market <- R6::R6Class(
  "rldmkt",
  public = list(
    initialize = rldmkt_construct,
    share = rldmkt_share,
    getMarket = ldmkt_getMarket,
    getOwnership = ldmkt_getOwnership,
    getDs = ldmkt_getDs,
    Ds_fun = rldmkt_Ds_fun,
    zeta_fixed_point = ldmkt_zeta_fixed_point,
    exp_profits = ldmkt_exp_profits,
    inc_value = rldmkt_inc_value,
    firm_profits_fun = ldmkt_firm_profits,
    markups = ldmkt_markups,
    marginal_cost = ldmkt_marginal_cost,
    getUjs = ldmkt_getUjs,
    getCjs = ldmkt_getCjs,
    getAijs = rldmkt_getAijs,
    getUijs = rldmkt_getUijs,
    getAlpha = rldmkt_getAlpha,
    getDerivPrice = rldmkt_getDerivPrice,
    getMuijs = rldmkt_getMuijs
  ),
  private = list(
    Market = NULL,
    Si = NULL,
    Jt = NULL,
    Market_size = NULL,
    Alpha = NULL,
    Deriv_price = NULL,
    U_out_opt = NULL,
    firm_profits = NULL,
    O = NULL,
    Ds = NULL,
    ujs = NULL,
    muijs = NULL,
    uijs = NULL,
    aijs = NULL,
    cjs = NULL,
    num_firms = NULL,
    firm_names= NULL
  )
)

LMarket <- R6::R6Class(
  "lmkt",
  inherit = Market,
  public = list(
    initialize = lmkt_construct,
    share_fun = lmkt_share_fun,
    Ds_fun = lmkt_Ds_fun,
    getIncVal = lmkt_getIncVal,
    exp_profs = lmkt_exp_profs
  ),
  private = list(
    deriv_price = NULL
  )
)

RLMarket <- R6::R6Class(
  "rlmkt",
  inherit = Market,
  public = list(
    initialize = rlmkt_construct,
    updateUijs = rlmkt_updateUijs,
    share_fun = rlmkt_share_fun,
    Ds_fun = rlmkt_Ds_fun,
    getIncVal = rlmkt_getIncVal,
    exp_profs = rlmkt_exp_profs
  ),
  private = list(
    alpha = NULL,
    deriv_price = NULL,
    diag_deriv_price = NULL,
    Index = NULL,
    Muij_all = NULL,
    muijs = NULL,
    uijs = NULL,
    aijs = NULL,
    Si = NULL
  )
)

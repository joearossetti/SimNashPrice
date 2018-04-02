MarkeCF <- R6::R6Class(
  "mktCF",
  public = list(
    initialize = mktCF_construct,
    gradient = NULL,
    difference = NULL,
    list_difference = NULL,
    curvature = NULL,
    hmc_sampler = NULL,
    exec_cf = mktCF_exec_cf,
    getMkt = mktCF_getMkt
  ),
  private = list(
    firm_names = NULL,
    current_A_mat = NULL,
    firm_perspective = NULL,
    fixed_cost_param = NULL,
    tp_cache = NULL,
    mkt_obj = NULL,
    ZFP_opts = NULL,
    Error_mats = NULL,
    num_draws = NULL,
    type_mkt = NULL
  )
)

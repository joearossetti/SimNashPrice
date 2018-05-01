MarketCF <- R6::R6Class(
  "mktCF",
  public = list(
    initialize = mktCF_construct,
    gradient = mktCF_gradient,
    gradients = mktCF_gradients,
    #difference = NULL,
    batch_exec_cf = mktCF_batch_exec_cf,
    curvature = mktCF_curvature,
    hmc_sampler = mktCF_hmc_sampler,
    exec_cf = mktCF_exec_cf,
    getMkt = mktCF_getMkt,
    getTp_cache = mktCF_getTp_cache,
    hash_manager = mktCF_hash_manager
  ),
  private = list(
    firm_names = NULL,
    num_pot_prods = NULL,
    current_A_mat = NULL,
    firm_perspective = NULL,
    fixed_cost_param = NULL,
    exec_cf_count = 0L,
    hash_limit = NULL,
    tp_cache = NULL,
    mkt_obj = NULL,
    ZFP_opts = NULL,
    Error_mats = NULL,
    num_draws = NULL,
    type_mkt = NULL
  )
)

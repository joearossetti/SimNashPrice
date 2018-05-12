## script to generate simulated data
library(SimNashPrice)

set.seed(1234)
alpha <- 4
beta <- c(2.0, 1.0, 1.5, 0.5, 1.75)
gamma <- c(0.2, 0.1, 0.05, 0.15, 0.25)

## Make a list of factor level
char_levels <- list(char1 = c(0,1), char2 = c(0,1), char3 = c(0,1) , char4 = c(0,1), char5 = c(0,1))

## Make list of all potential products
XP <- expand.grid(char_levels)
J <- dim(XP)[1]

XP <- data.frame(XP, prodname=1:J)

my_Tastes <- cbind(XP$char1, XP$char2, XP$char3, XP$char4, XP$char5) %*% beta
my_Costs <- cbind(XP$char1, XP$char2, XP$char3, XP$char4, XP$char5) %*% gamma

my_Pot_prod_df <- make_potential_prods_df(Prod_ids = XP$prodname, Tastes = my_Tastes, Costs = my_Costs)

beta_is <- cbind(rnorm(50, 0, 0.25), rnorm(50, 0, 0.25), rnorm(50, 0, 0.25), rnorm(50, 0, 0.25), rnorm(50, 0, 0.25))

my_muis <- as.matrix(XP[,1:5]) %*% t(beta_is)

my_alphais <- t(rnorm(50, 0, 0.25))

my_act_mat_sampler <- function(FIRM, PRODUCTS){
  a_temp <- vector('logical', dim(PRODUCTS)[1])
  a_temp[sample(1:dim(PRODUCTS)[1], size=5)] <- TRUE
  return(a_temp)
}

my_init_cost_shocks_fun <- function(N_PRODUCTS){
  rnorm(N_PRODUCTS, mean = 0, sd = 0.05)
}

my_init_taste_shocks_fun <- function(N_PRODUCTS){
  rnorm(N_PRODUCTS, mean = 0, sd = 0.25)
}

protect <- function(foo, foo_args){
  safe_foo <- purrr::safely(foo)
  
  temp <- safe_foo(foo_args)
  
  if(is.null(temp$result)){
    return(temp$error)
  }else{
    return(temp$result)
  }
}

simulate_N_mkts <- function(num_mkts, firms, pot_prod_df, act_mat_sampler,
                           alpha, alphais, muis,
                           init_cost_shocks_fun,
                           init_taste_shocks_fun,
                           zfp_opts = list("tol" = .Machine$double.neg.eps,
                                           "max_iter" = 2000,
                                           "rel_tol" = .Machine$double.neg.eps,
                                           "quietly" = TRUE)){
  N_pos_prods <- dim(pot_prod_df)[1]
  N_firms <- length(firms)
  
  rlmkt <- RLMarket$new(Potential_products = pot_prod_df,
                                Firms = firms,
                                Muij_mat = muis,
                                Alpha = alpha,
                                Aij_mat = alphais)
  
  make_1_mkt <- function(my_test_rlmkt){
    activity_matrix <- matrix(FALSE, nrow = N_pos_prods, ncol = N_firms)
    
    for(j in 1:N_firms){
      activity_matrix[,j] <- act_mat_sampler(FIRM=firms[j], PRODUCTS=pot_prod_df)
    }
    
    num_prods <- sum(colSums(activity_matrix))
    
    my_test_rlmkt$setActiveProds(Activity_matrix = activity_matrix)
    my_test_rlmkt$updateActivity()
    
    my_test_rlmkt$setPrices(Prices = rep(0, num_prods))
    my_test_rlmkt$setShares(Shares = rep(0, num_prods))
    my_test_rlmkt$setShocks(Cost_shocks = init_cost_shocks_fun(N_PRODUCTS = num_prods),
                            Taste_shocks = init_taste_shocks_fun(N_PRODUCTS = num_prods))
    my_test_rlmkt$updateUijs()
    
    my_test_rlmkt$ZFP(tol = zfp_opts[['tol']],
                      max_iter = zfp_opts[['max_iter']],
                      rel_tol = zfp_opts[['rel_tol']],
                      quietly = zfp_opts[['quietly']])
    
    return(my_test_rlmkt)
  }
  
  list_of_mkts <- purrr::rerun(.n=num_mkts, rlmkt$clone())
  
  list_of_mkts <- purrr::map(.x=list_of_mkts, .f=~protect(foo = make_1_mkt, foo_args = .x))
                              
  return(list_of_mkts)
}

test_rlmkts <- simulate_N_mkts(num_mkts = 100, firms = c(1,2),
               pot_prod_df = my_Pot_prod_df,
               act_mat_sampler = my_act_mat_sampler,
               alpha = alpha, alphais = my_alphais, muis = my_muis,
               init_cost_shocks_fun = my_init_cost_shocks_fun,
               init_taste_shocks_fun = my_init_taste_shocks_fun)


df_test_mkts <- purrr::map(test_rlmkts, .f=~dplyr::as_tibble(.x$getMarket()))

test_df <- dplyr::bind_rows(df_test_mkts, .id = 'Mkt_id')

test_df <- dplyr::left_join(test_df, XP, by=c('Prod_ids'='prodname'))

blp1_fun <- function(firm_vec, char_vec){
  purrr::map_dbl(firm_vec, .f=~sum(char_vec[which(firm_vec==.x)]))
}

blp2_fun <- function(firm_vec, char_vec){
  purrr::map_dbl(firm_vec, .f=~sum(char_vec[which(firm_vec!=.x)]))
}

library(tidyverse)

test_df <- test_df %>%
  group_by(Mkt_id) %>%
  mutate(
    BLP1_id_char1 = blp1_fun(firm_vec = Firms, char_vec = char1),
    BLP1_id_char2 = blp1_fun(firm_vec = Firms, char_vec = char2),
    BLP1_id_char3 = blp1_fun(firm_vec = Firms, char_vec = char3),
    BLP1_id_char4 = blp1_fun(firm_vec = Firms, char_vec = char4),
    BLP1_id_char5 = blp1_fun(firm_vec = Firms, char_vec = char5),
    BLP2_id_char1 = blp2_fun(firm_vec = Firms, char_vec = char1),
    BLP2_id_char2 = blp2_fun(firm_vec = Firms, char_vec = char2),
    BLP2_id_char3 = blp2_fun(firm_vec = Firms, char_vec = char3),
    BLP2_id_char4 = blp2_fun(firm_vec = Firms, char_vec = char4),
    BLP2_id_char5 = blp2_fun(firm_vec = Firms, char_vec = char5)
  )

readr::write_csv(test_df, 'inst/test_data.csv')

## make the BLP model matrices
make_formula <- function(var_names){
  as.formula(paste("~", paste(var_names, collapse =" + "), sep = '')) 
  
}

Z_frmla <- make_formula(stringr::str_subset(names(test_df), pattern = "BLP"))

Z_mat <- model.matrix(Z_frmla, data = test_df) %>% as.matrix()

which_are_chars <- stringr::str_detect(names(test_df), pattern = "char") & !stringr::str_detect(names(test_df), pattern = "BLP")

X1_frmla <- make_formula(c('Prices', names(test_df)[which_are_chars]))

X1_mat <- model.matrix(X1_frmla, data = test_df) %>% as.matrix()

X2_frmla <- make_formula(c('Prices', names(test_df)[which_are_chars]))

X2_mat <- model.matrix(X2_frmla, data = test_df) %>% as.matrix()

y_frmla <- make_formula(c('Shares'))

y_mat <- model.matrix(y_frmla, data = test_df) %>% as.matrix()

id_frmla <- make_formula(c('Mkt_id', 'Firms', 'Prod_ids'))

id_mat <- model.matrix(id_frmla, data = test_df) %>% as.matrix()

readr::write_csv(as_tibble(Z_mat), 'inst/blp_test_data/Z_mat.csv')
readr::write_csv(as_tibble(X1_mat), 'inst/blp_test_data/X1_mat.csv')
readr::write_csv(as_tibble(X2_mat), 'inst/blp_test_data/X2_mat.csv')
readr::write_csv(as_tibble(y_mat), 'inst/blp_test_data/y_mat.csv')
readr::write_csv(as_tibble(id_mat), 'inst/blp_test_data/id_mat.csv')

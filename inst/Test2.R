set.seed(1234)

## Parameters of demand function
alpha <- 4
beta <- c(-5, -4)
gamma <- c(1, 0.5)

## Make a list of factor level
char_levels <- list(char1 = c(0,1), char2 = c(0,1))

## Make list of all potential products
XP <- expand.grid(char_levels)
J <- dim(XP)[1]

XP <- data.frame(XP, prodname=1:J)

a1 <- sample(c(0,1), 4, replace = TRUE)
a2 <- sample(c(0,1), 4, replace = TRUE)

Xi <- rnorm(1, 0, 1)
Omega <- rnorm(1, 0, 1)

my_prod_table <- data.frame(j = XP$prodname, delta = (cbind(XP$char1, XP$char2) %*% beta), mc = (cbind(XP$char1, XP$char2) %*% gamma))

my_mkt_prods <- rbind(my_prod_table[which(a1==1),], my_prod_table[which(a2==1),])
my_mkt_prods <- data.frame(my_mkt_prods, firm = c(1,1,1,0,0))

my_ldm_obj <- logit_demand_market(my_mkt_prods$firm, my_mkt_prods$delta, Mc = my_mkt_prods$mc, Struct_error = Xi, u_out_opt = 1, Deriv_price = alpha)

my_ldm_obj <- share(my_ldm_obj)
my_ldm_obj <- zeta_fixed_point(my_ldm_obj, tol = 1e-6)

my_ldm_obj <- markups(my_ldm_obj)
my_ldm_obj <- marginal_cost(my_ldm_obj)

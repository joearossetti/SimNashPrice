set.seed(1234)

## Parameters of demand function
alpha <- 4
beta <- c(2, 3.5)
gamma <- c(0.5, 0.3)
#sigma_eps <- 0.2

## Index Parameters
K <- length(beta) # number of chars

## Make a list of factor level
char_levels <- list(char1 = c(0,1), char2 = c(0,1))

## Make list of all potential products
XP <- expand.grid(char_levels)
J <- dim(XP)[1]

XP <- data.frame(XP, prodname=1:J)

size <- 10000

a1 <- sample(c(0,1), 4, replace = TRUE)
a2 <- sample(c(0,1), 4, replace = TRUE)


J1 <- sum(a1)
J2 <- sum(a2)
Jt <- J1 + J2

Xi <- rnorm(1, 0, 1)
Omega <- rnorm(1, 0, 1)

my_prod_table <- data.frame(j = XP$prodname, delta = (cbind(XP$char1, XP$char2) %*% beta), mc = (cbind(XP$char1, XP$char2) %*% gamma))

my_mkt_prods <- rbind(my_prod_table[which(a1==1),], my_prod_table[which(a2==1),])
my_mkt_prods <- data.frame(my_mkt_prods, firm = c(1,1,1,0,0))

P <- rep(1, Jt)


O_fun(Firm = my_mkt_prods$firm, Jt = Jt)

my_s <- share_gen(P, my_mkt_prods$delta, demand_shocks = Xi)
my_D <- Ds_fun(S = share_gen(P, my_mkt_prods$delta, demand_shocks = Xi), alpha = alpha, Own = O_fun(Firm = my_mkt_prods$firm, Jt = Jt))
my_Lambda_inv <- solve(my_D$Lambda_p)

P1 <- my_Lambda_inv %*% (-my_s) - my_Lambda_inv %*% my_D$Gamma_p %*% (P - my_mkt_prods$mc) + my_mkt_prods$mc

my_s <- share_gen(P1, my_mkt_prods$delta, demand_shocks = Xi)
my_D <- Ds_fun(S = my_s, alpha = alpha, Own = O_fun(Firm = my_mkt_prods$firm, Jt = Jt))
my_Lambda_inv <- solve(my_D$Lambda_p)

P1 <- my_Lambda_inv %*% (-my_s) - my_Lambda_inv %*% my_D$Gamma_p %*% (P - my_mkt_prods$mc) + my_mkt_prods$mc


P_star <- zeta_fixed_point(P_init = (my_mkt_prods$mc+10), prods = my_mkt_prods, Alpha = alpha, Demand_shocks = Xi, tol = 1e-6)

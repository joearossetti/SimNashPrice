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

a_mat <- cbind(a1, a2)

J1 <- sum(a1)
J2 <- sum(a2)
Jt <- J1 + J2

Xi <- rnorm(1, 0, 1)
Omega <- rnorm(1, 0, 1)


my_prod_table <- data.frame(j = XP$prodname, delta = (cbind(XP$char1, XP$char2) %*% beta), mc = (cbind(XP$char1, XP$char2) %*% gamma))

my_mkt_prods <- rbind(my_prod_table[which(a_mat[,1]==1),], my_prod_table[which(a_mat[,2]==1),])

P <- rep(1, Jt)

Delta <- my_mkt_prods$delta
MC <- my_mkt_prods$mc

S <- share_gen(P, delta = Delta, demand_shocks = rep(Xi, Jt))

a1_w <- which(a1==1)
a2_w <- which(a2==1)

DpS <- D_fun(S = S, num_prods = Jt, alpha = alpha)

S[a1_w] + DpS[a1_w, a1_w] %*% (P[a1_w] - MC[a1_w])
S[a2_w] + DpS[a2_w, a2_w] %*% (P[a2_w] - MC[a2_w])

foc(P = P, A1_w = a1_w, A2_w = a2_w, prods = my_mkt_prods, alpha = alpha, xi = Xi, Jt=Jt)
soc(P = P, A1_w = a1_w, A2_w = a2_w, prods = my_mkt_prods, alpha = alpha, xi = Xi, Jt=Jt)

jacobian_foc(P = P, A1_w = a1_w, A2_w = a2_w, prods = my_mkt_prods, alpha = alpha, xi = Xi, Jt=Jt)
jacobian_soc(P = P, A1_w = a1_w, A2_w = a2_w, prods = my_mkt_prods, alpha = alpha, xi = Xi, Jt=Jt)


# require(nloptr)
# my_options <- list(
#   print_level=0,
#   algorithm='NLOPT_GN_ISRES',
#   xtol_rel=1e-8,
#   xtol_abs=1e-8,
#   maxeval=2400000,
#   maxtime=60,
#   ftol_rel=1e-32,
#   ftol_abs=1e-32)
#
# out <- nloptr(x0 = my_mkt_prods$mc,
#        eval_f = total_profit,
#        eval_g_eq = foc,
#        eval_g_ineq = soc,
#        lb = rep(0, 5),
#        ub = rep(1000, 5),
#        A1_w = a1_w, A2_w = a2_w,
#        prods = my_mkt_prods, alpha = alpha, xi = Xi, Jt=Jt, total = FALSE,
#        opts = my_options)

root_foc <- rootSolve::multiroot(foc, start = my_mkt_prods$mc, jacfunc = jacobian_foc,
                     A1_w = a1_w, A2_w = a2_w,
                     prods = my_mkt_prods, alpha = alpha, xi = Xi, Jt=Jt, total = FALSE)

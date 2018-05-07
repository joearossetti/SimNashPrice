library(SimNashPrice)
set.seed(1234)

my_rldmkt_obj <- my_test_market_obj$clone()

my_rldmkt_obj$zeta_fixed_point(tol = 1e-20, max_iter = 2000)
my_rldmkt_obj$markups()

mean_market <- my_rldmkt_obj$getMarket()
#microbenchmark::microbenchmark({

JJ <- length(mean_market$Delta)
Si_mat <- my_rldmkt_obj$getSijs()
Ai_mat <- my_rldmkt_obj$getDerivPrice()
O_mat <- my_rldmkt_obj$getOwnership()

R <- ncol(Si_mat)
R_ <- 1/R

Gamma_si <- array(0, dim=c(JJ,JJ,R))
Lambda_si <- array(0, dim=c(JJ,JJ,R))
Dp_si <- array(0, dim=c(JJ,JJ,R))
for(i in 1:R){
  Dp_si[,,i] <- matrix(Si_mat[,i], JJ, JJ) * t(matrix(Si_mat[,i], JJ, JJ))
  Gamma_si[,,i] <- Dp_si[,,i]* Ai_mat[,i]
  Lambda_si[,,i] <- diag(-Si_mat[,i]* Ai_mat[,i], nrow = JJ, ncol = JJ)
  diag(Dp_si[,,i]) <- -Si_mat[,i] + diag(Dp_si[,,i])
  Dp_si[,,i] <- Dp_si[,,i] * Ai_mat[,i]
}

Lambda_s <- matrix(0, JJ, JJ)
Gamma_s <- matrix(0, JJ, JJ)
Dp_s <- matrix(0, JJ, JJ)
for(i in 1:R){
  Lambda_s <- Lambda_s+ Lambda_si[,,i]
  Gamma_s <- Gamma_s+ Gamma_si[,,i]
  Dp_s <- Dp_s + Dp_si[,,i]
}
Dp_s <- Dp_s * R_
Lambda_s <- Lambda_s * R_
Gamma_s <- Gamma_s * R_

Dv_si <- array(0, dim=c(JJ, JJ, R))
for(i in 1:R){
  Dv_si[,,i] <- -(Si_mat[,i] %*% t(Si_mat[,i]))
  diag(Dv_si[,,i]) <- Si_mat[,i] * (1- Si_mat[,i])
}

Dv_s <- matrix(0, JJ, JJ)
for(i in 1:R){
  Dv_s <- Dv_s + Dv_si[,,i]
}
Dv_s <- Dv_s * R_

## second derivative with respect to v
DvDv_si <- vector('list', R)
for(i in 1:R){
  DvDv_si[[i]] <- array(0, dim=c(JJ, JJ, JJ))
  S_s <- matrix(Si_mat[,i], JJ, JJ)
  S_s_t <- t(S_s)
  diag(S_s_t) <- 1 - Si_mat[,i]

  Alpha_mat <- matrix(-1, JJ, JJ)
  diag(Alpha_mat) <- -diag(Alpha_mat) # on diagonal terms positive

  D_mat <- Dv_si[,,i]
  for(j in 1:JJ){
    D_repd <- matrix(D_mat[j,], JJ, JJ)
    D_repd_t <- t(D_repd)
    diag(D_repd_t)  <- -diag(D_repd_t)
    DvDv_si[[i]][,,j] <- Alpha_mat * (D_repd * S_s_t + D_repd_t * S_s)
  }
}

DvDv_s <- Reduce('+', DvDv_si) * R_

## second derivative with respect to p
DpDp_si <- vector('list', R)
for(i in 1:R){
  DpDp_si[[i]] <- array(0, dim=c(JJ, JJ, JJ))
  S_s <- matrix(Si_mat[,i], JJ, JJ)
  S_s_t <- t(S_s)
  diag(S_s_t) <- (1 - Si_mat[,i])

  Alpha_mat <- matrix(Ai_mat[,i], JJ, JJ)
  diag(Alpha_mat) = -diag(Alpha_mat) # for price the diagonal is negative

  D_mat <- Dp_si[,,i]
  for(j in 1:JJ){
    D_repd <- matrix(D_mat[j,], JJ, JJ)
    D_repd_t <- t(D_repd)
    diag(D_repd_t)  <- -diag(D_repd_t)
    DpDp_si[[i]][,,j] <- Alpha_mat * (D_repd * S_s_t + D_repd_t * S_s)
  }
}

DpDp_s <- Reduce('+', DpDp_si) * R_

## second derivative of Dp with respect to v
## slices will be indexed by the k in dvkDps notation
DvDp_si <- vector('list', R)
for(i in 1:R){
  DvDp_si[[i]] <- array(0, dim=c(JJ, JJ, JJ))
  S_s <- matrix(Si_mat[,i], JJ, JJ)
  S_s_t <- t(S_s)
  diag(S_s_t) <- 1 - Si_mat[,i]

  Alpha_mat <- matrix(Ai_mat[,i], JJ, JJ)
  diag(Alpha_mat) = -diag(Alpha_mat) # for price the diagonal is negative

  D_mat <- Dv_si[,,i]
  for(j in 1:JJ){
    D_repd <- matrix(D_mat[j,], JJ, JJ)
    D_repd_t <- t(D_repd)
    diag(D_repd_t)  <- -diag(D_repd_t)
    DvDp_si[[i]][,,j] <- Alpha_mat * (D_repd * S_s_t + D_repd_t * S_s)
  }
}

DvDp_s <- Reduce('+', DvDp_si) * R_

## computing derivative of reaction function
b <- my_rldmkt_obj$getMarket()[['Markup']]

temp <- matrix(0, JJ, JJ)
for(j in 1:JJ){
  temp <- temp + b[j] * (DpDp_s[,,j] * O_mat)
}

Dp_y <- 2 * (Dp_s * O_mat) + temp

Dv_p <- matrix(0, JJ, JJ)
for(l in 1:JJ){
  dvl_y <- Dv_s[l,] + (DvDp_s[,,l] * O_mat) %*% b
  Dv_p[,l] <- -solve(Dp_y) %*% dvl_y
}

## computing gradient of profits
firm_id_vec <- my_rldmkt_obj$getMarket()[['Firms']]
unique_firms <- unique(firm_id_vec)
FF <- length(unique_firms)
firms_total_grad_profs <- matrix(0, FF, JJ)
for(f in 1:FF){
  Firm <- unique_firms[f]
  which_is_firm <- which(firm_id_vec==Firm)
  which_is_not_firm <- which(firm_id_vec!=Firm)
  #print(which_is_firm)
  #print(which_is_not_firm)

  if(length(which_is_firm)==1){
    grad_prof_f <- Dp_s[which_is_not_firm, which_is_firm] * b[which_is_firm]
  }else{
    grad_prof_f <- Dp_s[which_is_not_firm, which_is_firm] %*% b[which_is_firm]
  }

  for(l in 1:JJ){
    ## computing derivative of profit
    dv_prof <- Dv_s[which_is_firm,l] %*% b[which_is_firm]

    ## total gradient of eq. profits wrt v
    firms_total_grad_profs[f,l] <- sum(grad_prof_f * Dv_p[which_is_not_firm,l]) + dv_prof
  }
}

## computing second derivatives of reaction function

## DpDv_s
## second derivative of Dv with respect to p
## slices will be indexed by the k in dpkDv_s notation
DpDv_si <- vector('list', R)
for(i in 1:R){
  DpDv_si[[i]] <- array(0, dim=c(JJ, JJ, JJ))
  S_s <- matrix(Si_mat[,i], JJ, JJ)
  S_s_t <- t(S_s)
  diag(S_s_t) <- 1 - Si_mat[,i]

  Alpha_mat <- matrix(-1, JJ, JJ)
  diag(Alpha_mat) <- -diag(Alpha_mat) # on diagonal terms positive

  D_mat <- Dp_si[,,i]
  for(j in 1:JJ){
    D_repd <- matrix(D_mat[j,], JJ, JJ)
    D_repd_t <- t(D_repd)
    diag(D_repd_t)  <- -diag(D_repd_t)
    DpDv_si[[i]][,,j] <- Alpha_mat * (D_repd * S_s_t + D_repd_t * S_s)
  }
}

DpDv_s <- Reduce('+', DpDv_si) * R_

## DvDvDp_s
DvDvDp_s <- vector('list', JJ)
for(k in 1:JJ){
  DvDvDp_si <- vector('list', R)
  for(i in 1:R){
    DvDvDp_si[[i]] <- array(0, dim=c(JJ, JJ, JJ))

    S_s <- matrix(Si_mat[,i], JJ, JJ)
    S_s_t <- t(S_s)
    diag(S_s_t) <- 1 - Si_mat[,i]

    Alpha_mat <- matrix(Ai_mat[,i], JJ, JJ)
    diag(Alpha_mat) = -diag(Alpha_mat) # for price the diagonal is negative

    D_mat <- Dv_si[,,i]
    D2_mat <- DvDv_si[[i]]
    for(j in 1:JJ){
      D_repd <- matrix(D_mat[j,], JJ, JJ)
      D_repd_t <- t(D_repd)
      diag(D_repd_t)  <- -diag(D_repd_t)

      D2_repd <- matrix(D2_mat[,j,k], JJ, JJ)
      D2_repd_t <- t(D2_repd)
      diag(D2_repd_t)  <- -diag(D2_repd_t)

      DvDvDp_si[[i]][,,j] <- Alpha_mat * (D2_repd * S_s_t + 2 * D_repd * D_repd_t + D2_repd_t * S_s)
    }
  }
  DvDvDp_s[[k]] <- Reduce('+', DvDvDp_si) * R_
}

## DpDpDp_s
DpDpDp_s <- vector('list', JJ)
for(k in 1:JJ){
  DpDpDp_si <- vector('list', R)
  for(i in 1:R){
    DpDpDp_si[[i]] <- array(0, dim=c(JJ, JJ, JJ))

    S_s <- matrix(Si_mat[,i], JJ, JJ)
    S_s_t <- t(S_s)
    diag(S_s_t) <- 1 - Si_mat[,i]

    Alpha_mat <- matrix(Ai_mat[,i], JJ, JJ)
    diag(Alpha_mat) = -diag(Alpha_mat) # for price the diagonal is negative

    D_mat <- Dp_si[,,i]
    D2_mat <- DpDp_si[[i]]
    for(j in 1:JJ){
      D_repd <- matrix(D_mat[j,], JJ, JJ)
      D_repd_t <- t(D_repd)
      diag(D_repd_t)  <- -diag(D_repd_t)

      D2_repd <- matrix(D2_mat[,j,k], JJ, JJ)
      D2_repd_t <- t(D2_repd)
      diag(D2_repd_t)  <- -diag(D2_repd_t)

      DpDpDp_si[[i]][,,j] <- Alpha_mat * (D2_repd * S_s_t + 2 * D_repd * D_repd_t + D2_repd_t * S_s)
    }
  }
  DpDpDp_s[[k]] <- Reduce('+', DpDpDp_si) * R_
}

## DpDvDp_s
DpDvDp_s <- vector('list', JJ)
for(k in 1:JJ){
  DpDvDp_si <- vector('list', R)
  for(i in 1:R){
    DpDvDp_si[[i]] <- array(0, dim=c(JJ, JJ, JJ))

    S_s <- matrix(Si_mat[,i], JJ, JJ)
    S_s_t <- t(S_s)
    diag(S_s_t) <- 1 - Si_mat[,i]

    Alpha_mat <- matrix(Ai_mat[,i], JJ, JJ)
    diag(Alpha_mat) = -diag(Alpha_mat) # for price the diagonal is negative

    D_mat <- Dp_si[,,i]
    D2_mat <- DvDp_si[[i]]
    for(j in 1:JJ){
      D_repd <- matrix(D_mat[j,], JJ, JJ)
      D_repd_t <- t(D_repd)
      diag(D_repd_t)  <- -diag(D_repd_t)

      D2_repd <- matrix(D2_mat[,j,k], JJ, JJ)
      D2_repd_t <- t(D2_repd)
      diag(D2_repd_t)  <- -diag(D2_repd_t)

      DpDvDp_si[[i]][,,j] <- Alpha_mat * (D2_repd * S_s_t + 2 * D_repd * D_repd_t + D2_repd_t * S_s)
    }
  }
  DpDvDp_s[[k]] <- Reduce('+', DpDvDp_si) * R_
}

O_cube <- array(0, dim=c(JJ, JJ, JJ))
for(j in 1:JJ){
  O_cube[,,j] <- O_mat
}



## DpDp_y
# temp3 <- array(0, dim=c(JJ, JJ, JJ))
# for(j in 1:JJ){
#   temp3 <- temp3 + b[j] * DpDpDp_s[[j]]
# }

DpDp_y <-(3 * DpDp_s + vector_times_field(vec = b, field = DpDpDp_s, com_dim = JJ)) * O_cube

## DpDp_y symmetric?
# temp8 <- array(0, dim=c(JJ, JJ, JJ))
# for(j in 1:JJ){
#   for(k in 1:JJ){
#     temp8[,,j] <- temp8[,,j] + b[k] * DpDpDp_s[[k]][,,j]
#   }
# }
#
# DpDp_y2 <- (3 * DpDp_s + temp3) * O_cube
#
# DpDp_y3 <- (3 * DpDp_s + vector_times_field(vec = b, field = DpDpDp_s, com_dim = JJ)) * O_cube

## DpDv_y
# temp4 <- array(0, dim=c(JJ, JJ, JJ))
# for(j in 1:JJ){
#   temp4 <- temp4 + b[j] * DpDvDp_s[[j]]
# }

DpDv_y <- (DpDv_s + DvDp_s + vector_times_field(vec = b, field = DpDvDp_s, com_dim = JJ)) * O_cube

## DvDv_y
# temp5 <- array(0, dim=c(JJ, JJ, JJ))
# for(j in 1:JJ){
#   temp5 <- temp5 + b[j] * DvDvDp_s[[j]]
# }

DvDv_y <- (DvDv_s + vector_times_field(vec = b, field = DvDvDp_s, com_dim = JJ)) * O_cube

# DvDv_p_diag <- matrix(0, JJ, JJ)
# for(k in 1:JJ){
#   temp7 <- matrix(0, JJ, JJ)
#   for(j in 1:JJ){
#     temp7 <- temp7 + Dv_p[k,j] * DpDp_y[,,j]Dp
#   }
#   DvDv_p_diag[,k] <- solve(Dp_y) %*% (temp7 %*% Dv_p[,k] + DpDv_y[,,k] %*% Dv_p[,k] + DvDv_y[,k,k])
# }

DvDv_p <- array(0, dim=c(JJ, JJ, JJ))
for(k in 1:JJ){
  for(l in 1:JJ){
    # temp7 <- matrix(0, JJ, JJ)
    # for(j in 1:JJ){
    #   temp7 <- temp7 + Dv_p[k,j] * DpDp_y[,,j]
    # }

    DvDv_p[,k,l] <- -solve(Dp_y) %*% (vector_times_cube(vec = Dv_p[k,], cube = DpDp_y, com_dim = JJ) %*% Dv_p[,l] + DpDv_y[,,l] %*% Dv_p[,k] + DvDv_y[,k,l])
  }

  #DvDv_p[,,k] <- solve(Dp_y) %*% (vector_times_cube(vec = Dv_p[k,], cube = DpDp_y, com_dim = JJ) %*% Dv_p[,l] + DpDv_y[,,l] %*% Dv_p[,k] + DvDv_y[,k,l])
}

DvDv_p2 <-  mat_times_cube(mat = -solve(Dp_y), cube = (mat_times_cube(mat=Dv_p, cube = mat_times_cube(mat=Dv_p, cube = DpDp_y, com_dim = JJ), com_dim = JJ) + mat_times_cube(mat = Dv_p, cube=DpDv_y, com_dim = JJ)+DvDv_y), com_dim = JJ)

## computing diagonal of hessian of profits
hessian_prof <- array(0, dim=c(FF, JJ, JJ))
for(f in 1:FF){
  Firm <- unique_firms[f]
  which_is_firm <- which(firm_id_vec==Firm)
  which_is_not_firm <- which(firm_id_vec!=Firm)

  if(length(which_is_firm)==1){
    grad_prof_f <- Dp_s[which_is_not_firm, which_is_firm] * b[which_is_firm]
  }else{
    grad_prof_f <- Dp_s[which_is_not_firm, which_is_firm] %*% b[which_is_firm]
  }


  for(k in 1:JJ){
    for(l in 1:JJ){
      ## derivative wrt vk of gradient of profit
      if(length(which_is_firm)==1){
        dvk_grad_prof <- Dp_s[which_is_not_firm, which_is_firm] * Dv_p[which_is_firm,k] + DvDp_s[which_is_not_firm,which_is_firm,k] * b[which_is_firm]
      }else{
        dvk_grad_prof <- Dp_s[which_is_not_firm, which_is_firm] %*% Dv_p[which_is_firm,k] + DvDp_s[which_is_not_firm,which_is_firm,k] %*% b[which_is_firm]
      }


      ## second derivative of profit with respect to vk
      if(length(which_is_firm)==1){
        dd_prof <- sum(Dv_p[which_is_firm,k] * Dv_s[which_is_firm, l]) + b[which_is_firm] * DvDv_s[which_is_firm,k,l]
      }else{
        dd_prof <- Dv_p[which_is_firm,k] %*% Dv_s[which_is_firm, l] + b[which_is_firm] %*% DvDv_s[which_is_firm,k,l]
      }

      hessian_prof[f,k,l] <- sum(dvk_grad_prof * Dv_p[which_is_not_firm,k]) + sum(grad_prof_f * DvDv_p2[which_is_not_firm,k,l]) + dd_prof
    }
  }
}


# var_struct_errors <- 1
# mean_struct_errors <- 0
# mean_prof_vect <- vector('numeric', length = length(unique(mean_market$Firms)))
# exp_prof_vect <- vector('numeric', length = length(unique(mean_market$Firms)))
# for(f in 1:length(unique(mean_market$Firms))){
#   Firm <- unique(mean_market$Firms)[f]
#   which_is_firm <- which(mean_market$Firms==Firm)
#
#   mean_prof_vect[f] <- sum(mean_market$Markup[which_is_firm] * mean_market$Share[which_is_firm])
#   exp_prof_vect[f] <- mean_prof_vect[f] + 0.5 * ( sum(diag(hessian_prof[f,,])*var_struct_errors) - sum(hessian_prof[f,,] * (mean_struct_errors)^2))
# }



#microbenchmark::microbenchmark(sweep(test_mat, MARGIN=2, STATS=c(2,3), FUN = '*'), times = 1000)
#microbenchmark::microbenchmark(test_mat %*% diag(c(2,3), 2, 2), times = 1000)

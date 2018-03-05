#' Compute Derivatives of shares
#'
#' @return List of Derivatives of shares
#' @export
#'
#' @examples #NA
rldmkt_computeJacobians <- function(){
  private$DDD <- envelope_helper(Sr = private$Si, ar=private$Deriv_price)

  #return(private$DDD)
  #invisible(self)
}

#' Derivative of Reaction Function
#'
#' @return Derivative of Reaction Function
#' @export
#'
#' @examples #NA
rldmkt_Dv_reaction_fun <- function(costs){

  JJ <- private$Jt

  O_cube <- array(0, dim=c(JJ, JJ, JJ))
  for(j in 1:JJ){
    O_cube[,,j] <- private$O
  }
  ## computing derivative of reaction function

  b <- as.numeric(private$Market[['Markup']])
  # temp <- matrix(0, JJ, JJ)
  # for(j in 1:JJ){
  #   temp <- temp + b[j] * (private$DDD$DpDp_s[,,j] * private$O)
  # }
  #
  # private$Dp_y <- 2 * (private$DDD$Dp_s * private$O) + temp


  private$Dp_y <- 2 * (private$DDD$Dp_s * private$O) + vector_times_cube(vec = b, cube = (private$DDD$DpDp_s*O_cube), com_dim = JJ)
  Dp_y_inv <- solve(private$Dp_y)
  # Dv_p <- matrix(0, JJ, JJ)
  # for(l in 1:JJ){
  #   dvl_y <- private$DDD$Dv_s[l,] + (private$DDD$DvDp_s[,,l] * private$O) %*% b
  #   Dv_p[,l] <- - Dp_y_inv %*% dvl_y
  # }
  private$Dv_p <- - Dp_y_inv %*% (private$DDD$Dv_s + vector_times_cube(vec = b, cube = private$DDD$DvDp_s, com_dim = JJ) * private$O)

  if(costs==TRUE){
    private$Dw_p <- - Dp_y_inv %*% (diag(as.numeric(private$cjs), JJ, JJ) %*% (private$DDD$Dp_s * private$O))
  }


  #return(Dv_p)
  #invisible(self)
}


#' Gradient of Eq. Profits
#'
#' @return Gradients of Eq. Profit
#' @export
#'
#' @examples #NA
rldmkt_grad_eq_prof <- function(name_of_firm){
  #print('Profit Gradient')

  firm_number_id <- which(private$firm_names==name_of_firm)
  which_is_firm <- which(private$Market[['Firms']]==name_of_firm)
  which_is_not_firm <- which(private$Market[['Firms']]!=name_of_firm)

  b <- as.numeric(private$Market[['Markup']])

  if(length(which_is_firm)==1){
    grad_prof_f <- private$DDD$Dp_s[which_is_not_firm, which_is_firm] * b[which_is_firm]
  }else{
    grad_prof_f <- private$DDD$Dp_s[which_is_not_firm, which_is_firm] %*% b[which_is_firm]
  }

  for(l in 1:private$Jt){
    ## computing derivative of profit
    dv_prof <- private$DDD$Dv_s[which_is_firm,l] %*% b[which_is_firm]

    ## total gradient of eq. profits wrt v
    #print(firm_number_id)
    private$firms_total_grad[firm_number_id,l] <- sum(grad_prof_f * private$DDD$Dv_p[which_is_not_firm,l]) + dv_prof
  }

  return(private$firms_total_grad[firm_number_id,])
  #invisible(self)
}

#' 2nd Derivative of Reaction Function
#'
#' @return 2nd Derivative of Reaction Function
#' @export
#'
#' @examples #NA
rldmkt_DvDv_reaction_fun <- function(costs){
  JJ <- private$Jt

  O_cube <- array(0, dim=c(JJ, JJ, JJ))
  for(j in 1:JJ){
    O_cube[,,j] <- private$O
  }

  b <- as.numeric(private$Market[['Markup']])

  DpDp_y <- private$DDD$DpDp_s + private$DDD$DpDp_s * O_cube + vector_times_field(vec = b, field = private$DDD$DpDpDp_s, com_dim = JJ) * O_cube

  DpDv_y <- private$DDD$DpDv_s + private$DDD$DvDp_s * O_cube + vector_times_field(vec = b, field = private$DDD$DpDvDp_s, com_dim = JJ) * O_cube

  DvDp_y <- private$DDD$DvDp_s + private$DDD$DvDp_s * O_cube + vector_times_field(vec = b, field = private$DDD$DvDpDp_s, com_dim = JJ) * O_cube

  DvDv_y <- (private$DDD$DvDv_s + vector_times_field(vec = b, field = private$DDD$DvDvDp_s, com_dim = JJ)) * O_cube

  Dp_y_inv <- solve(private$Dp_y)

  # DvDv_p <- array(0, dim=c(JJ, JJ, JJ))
  # for(k in 1:JJ){
  #   for(l in 1:JJ){
  #     DvDv_p[,k,l] <- -Dp_y_inv %*% (vector_times_cube(vec = private$Dv_p[k,], cube = DpDp_y, com_dim = JJ) %*% private$Dv_p[,l] + DpDv_y[,,l] %*% private$Dv_p[,k] + DvDv_y[,k,l])
  #   }
  # }

  DvDv_p <- mat_times_cube(mat=-Dp_y_inv, cube = (mat_times_cube(mat = private$Dv_p, cube = mat_times_cube(mat = private$Dv_p, cube = DpDp_y, com_dim = JJ), com_dim = JJ) +
                                                    mat_times_cube(mat=private$Dv_p, cube = DvDp_y, com_dim = JJ) +
                                                    mat_times_cube(mat=private$Dv_p, cube = DpDv_y, com_dim = JJ) +
                                                    DvDv_y), com_dim = JJ)

  private$DvDv_p <- DvDv_p

  if(costs==TRUE){
  DwDw_c <- array(0, dim=c(JJ, JJ, JJ))
  for(j in 1:JJ){
        DwDw_c[j,j,j] <- as.numeric(private$cjs)[j]
  }

  Dw_c <- diag(as.numeric(private$cjs), JJ, JJ)

  DwDp_y <- -mat_times_cube(mat = Dw_c, cube = (private$DDD$DpDp_s*O_cube), com_dim = JJ)
  DpDw_y <- DwDp_y

  DwDw_y <- -mat_times_cube(mat = private$O*private$DDD$Dp_s, cube = DwDw_c, com_dim = JJ)

  private$DwDw_p <- mat_times_cube(mat = -Dp_y_inv, cube = (mat_times_cube(mat = private$Dw_p, cube = mat_times_cube(mat=private$Dw_p, cube = DpDp_y, com_dim = JJ), com_dim = JJ) +
                                            mat_times_cube(mat = private$Dw_p, cube = DwDp_y, com_dim = JJ) +
                                            mat_times_cube(mat = private$Dw_p, cube =  DpDw_y, com_dim = JJ) +
                                            DwDw_y), com_dim = JJ)
  }

  #return(DvDv_p)
  #invisible(self)
}

#' Hessian of Eq. Profits
#'
#' @return Hessian of Eq. Profits
#' @export
#'
#' @examples #NA
rldmkt_hessian_eq_prof <- function(name_of_firm, costs){
  JJ <- private$Jt
  firm_number_id <- which(private$firm_names==name_of_firm)
  which_is_firm <- which(private$Market[['Firms']]==name_of_firm)
  which_is_not_firm <- which(private$Market[['Firms']]!=name_of_firm)

  hessian_prof_v <- matrix(0, JJ, JJ)
  b <- as.numeric(private$Market[['Markup']])

  if(length(which_is_firm)==1){
    grad_prof_f <- private$DDD$Dp_s[which_is_not_firm, which_is_firm] * b[which_is_firm]
  }else{
    grad_prof_f <- private$DDD$Dp_s[which_is_not_firm, which_is_firm] %*% b[which_is_firm]
  }


  for(k in 1:JJ){
    for(l in 1:JJ){
      ## derivative wrt vk of gradient of profit
      if(length(which_is_firm)==1){
        dvk_grad_prof <- private$DDD$Dp_s[which_is_not_firm, which_is_firm] * private$Dv_p[which_is_firm,k] +
          private$DDD$DvDp_s[which_is_not_firm,which_is_firm,k] * b[which_is_firm]
      }else{
        dvk_grad_prof <- private$DDD$Dp_s[which_is_not_firm, which_is_firm] %*% private$Dv_p[which_is_firm,k] +
          private$DDD$DvDp_s[which_is_not_firm,which_is_firm,k] %*% b[which_is_firm]
      }


      ## second derivative of profit with respect to vk
      if(length(which_is_firm)==1){
        dd_prof <- sum(private$Dv_p[which_is_firm,k] * private$DDD$Dv_s[which_is_firm, l]) +
          b[which_is_firm] * private$DDD$DvDv_s[which_is_firm,k,l]
      }else{
        dd_prof <- private$Dv_p[which_is_firm,k] %*% private$DDD$Dv_s[which_is_firm, l] +
          b[which_is_firm] %*% private$DDD$DvDv_s[which_is_firm,k,l]
      }

      hessian_prof_v[k,l] <- sum(dvk_grad_prof * private$Dv_p[which_is_not_firm,k]) +
        sum(grad_prof_f * private$DvDv_p[which_is_not_firm,k,l]) + dd_prof
    }
  }

  if(costs==TRUE){
  Dw_c <- diag(as.numeric(private$cjs), JJ, JJ)
  DwDw_c <- array(0, dim=c(JJ, JJ, JJ))
  for(j in 1:JJ){
    DwDw_c[j,j,j] <- as.numeric(private$cjs)[j]
  }

  Dw_s <-  private$DDD$Dp_s %*% private$Dw_p

  hessian_prof_w <- matrix(0, JJ, JJ)
  for(k in 1:JJ){
    #print(dim(private$DDD$DpDp_s[which_is_not_firm,which_is_firm,which_is_not_firm]))
    #print(private$Dv_p[which_is_not_firm,k][1])
    DwDp_sf <- vector_times_cube2(vec = private$Dw_p[which_is_not_firm,k],
                                  cube = private$DDD$DpDp_s[which_is_not_firm,which_is_firm,which_is_not_firm],
                                  com_dim = length(which_is_not_firm), dim = c(length(which_is_not_firm), length(which_is_firm)))
    for(l in 1:JJ){
      ## derivative wrt wk of gradient of profit
      if(length(which_is_firm)==1){
        dwk_grad_prof <- (private$Dw_p[which_is_firm,k]-Dw_c[which_is_firm,k]) * private$DDD$Dp_s[which_is_firm,which_is_not_firm] +
          DwDp_sf * b[which_is_firm]

      }else{
        dwk_grad_prof <- -private$DDD$Dp_s[which_is_not_firm,which_is_firm] %*% Dw_c[which_is_firm,k]+
          DwDp_sf %*% b[which_is_firm]
      }


      ## second derivative of profit with respect to wk
      if(length(which_is_firm)==1){
        dd_prof <- -sum(DwDw_c[which_is_firm,k,l] * private$Market[['Share']][which_is_firm])  - Dw_c[which_is_firm,l] * Dw_s[which_is_firm,k]
      }else{
        dd_prof <- -sum(DwDw_c[which_is_firm,k,l] * private$Market[['Share']][which_is_firm]) - sum(Dw_c[which_is_firm,l] * Dw_s[which_is_firm,k])
      }

      hessian_prof_w[k,l] <- sum(dwk_grad_prof * private$Dw_p[which_is_not_firm,k]) +
        sum(grad_prof_f * private$DwDw_p[which_is_not_firm,k,l]) + dd_prof
    }
  }
  }


  if(costs==TRUE){
    full_hessian_prof <- matrix(0, 2*JJ, 2*JJ)

    full_hessian_prof[1:JJ,1:JJ] <- hessian_prof_v
    full_hessian_prof[(JJ+1):(2*JJ),(JJ+1):(2*JJ)] <- hessian_prof_w

    return(full_hessian_prof)
  }else{
    return(hessian_prof_v)
  }
  #invisible(self)
}



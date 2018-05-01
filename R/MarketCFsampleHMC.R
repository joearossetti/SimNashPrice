mktCF_hmc_sampler <- function(L, Time, Sigma, firm_to_change, temp=1){
  #my_progress_bar <- progress::progress_bar$new(total = (L-1), show_after = 0, clear = FALSE)
  d <- private$num_pot_prods
  log_lik <- vector('numeric', L)
  energies <- vector('numeric', L)
  time_iter <- vector('numeric', L)
  #ll <- f.logp(x_to_s(X_init))
  firm_change_location <- which(private$firm_names==firm_to_change)
  init_state <- private$current_A_mat[,firm_change_location]
  ll <- temp*self$exec_cf(prod_state = init_state, firm_to_change=firm_to_change, MEM=TRUE)
  log_lik[1] <- ll

  wall_hits <- 0
  wall_crosses <- 0

  epsilon <- 1e-16

  ## these variables were defined in the source code, but don't appear again
  #touched <- matrix(nrow = d, ncol = ceiling(Time/pi)*(L-1))
  #mts <- matrix(nrow = d, ncol = ceiling(Time/pi)*(L-1))
  #xx <- vector('numeric', d)

  X_init <- ifelse(init_state==TRUE, abs(rnorm(1, 0, 1)), -abs(rnorm(1, 0, 1)))

  X_last <- X_init
  Xs <- matrix(nrow = d, ncol = L)
  Ss <- matrix(nrow = d, ncol = L)
  Ss[,1] <- init_state
  Xs[,1] <- X_init

  V_mat <- matrix(0,nrow = L-1,ncol=d)
  class(V_mat)<-"numeric"
  mvnfast::rmvn(n = L-1, mu = rep(0,d), sigma = Sigma, A = V_mat)
  #my_progress_bar$tick(1)

  for(i in 2:L){

    #i = 2
    #print(i)
    start_time <- proc.time()
    stop_condition <- FALSE
    j <- 0
    #V <- rnorm(d, 0, Sigma) ## initional velocity
    #V <- MASS::mvrnorm(1, rep(0,d), Sigma = Sigma)
    V <- as.numeric(V_mat[i-1,])
    #print(V)
    X <- X_last

    tt <- 0 ## how much has the particle moved already
    S <- ifelse(X>=0, TRUE, FALSE) ## current state

    while(TRUE){
      a <- V
      b <- X
      phi <- atan2(b,a) ## phi %in% c(-pi,pi)

      ## find first time constraint becomes zero
      wt1 = -phi  ## time coords hit the walls
      wt1[phi>0] <- pi - phi[phi>0]

      ## some kind of protection against numerical errors
      if(j>0){
        tt1 <- wt1[j]
        if(abs(tt1)<epsilon || abs(tt1-2*pi) < epsilon){
          wt1[j] <- Inf
        }
      }

      mt <- min(wt1)
      j <- which.min(wt1)

      ## at this point you could pre-fetch knowing at least the first product
      ## should sort wt1 and pre-fetch them in order

      tt <- tt+mt

      if(tt >= Time){
        mt <- mt - (tt - Time)
        stop_condition <- TRUE
      }else{
        wall_hits <- wall_hits + 1
      }

      ## move particle a time mt

      X <- a*sin(mt) + b*cos(mt)
      V <- a*cos(mt) - b*sin(mt)

      #print(stop_condition)

      if(stop_condition){
        break
      }

      X[j] <- 0

      #v2_new <- V[j]^2 + sign(V[j])*2*f.log_p_change(State = S, J=j)
      v2_new <- V[j]^2 + sign(V[j])*2*temp*self$gradient(base_set = S, prod_index = j, firm_to_change = firm_to_change, set_grad=TRUE)
      if(v2_new > 0){
        V[j] <- sqrt(v2_new)*sign(V[j])
        S[j] <- ifelse(S[j]==FALSE, TRUE, FALSE)
        wall_crosses <- wall_crosses + 1
      }else{
        V[j] <- -V[j]
      }
    }

    Xs[,i] <- X
    Ss[,i] <- S
    #ll <- f.logp(S)
    ll <- temp*self$exec_cf(prod_state = S, firm_to_change=firm_to_change, MEM=TRUE)
    log_lik[i] <- ll
    energies[i] <- 0.5*(t(X)%*%X+t(V)%*%V)-log_lik[i]
    last_X <- X
    end_time <- proc.time()
    time_iter[i] <- end_time[3]-start_time[3]
    #my_progress_bar$tick()
  }
  #Ss <- apply(Xs, 2, x_to_s)
  #state_df <- as_tibble(t(Ss))
  #names(state_df) <- state_var_names
  return(list("log_lik" = log_lik, "energies"= energies, "time_iter" = time_iter, "Ss" = Ss, "Xs" = Xs))
}

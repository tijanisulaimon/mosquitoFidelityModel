
# Model with fidelity. 
calculate_R0 <- function(f,
                         p_A,
                         Nph,
                         N_c,
                         alpha = 1 / 3,
                         beta = 1,
                         gamma = 1 / 5,
                         mu = 1 / 365,
                         mu_m = 1 / 30) {
     
  N_de <- 1000 
    N <- N_c + N_de
    N_m <- N*Nph
    rho_A <- (p_A * N_c) / (p_A * N_c + (1 - p_A) * N_de)
    
    f1 <- (f + (1 - f) * rho_A)
    f2 <- ((1 - f) * rho_A)
    
    R0_mos_imp_A_by_host_A <-
      (f1 * alpha * beta * rho_A * alpha * N_m + rho_A * alpha * beta * (1 -alpha) * N_m) / ((gamma + mu) * N)
    
    R0_mos_imp_D_by_host_A <-
      (f2 * alpha * beta * (1 - rho_A) * alpha * N_m) / ((gamma + mu) * N)
    
    R0_host_A_by_mos_imp_A <- (f1 * alpha * beta * N_c) / (mu_m * N)
    
    R0_host_A_by_mos_imp_D <- (f2 * alpha * beta * N_c) / (mu_m * N)
    
    R_0 <- sqrt(
      R0_mos_imp_A_by_host_A * R0_host_A_by_mos_imp_A + R0_mos_imp_D_by_host_A *  R0_host_A_by_mos_imp_D
    )

    R_0 <- ifelse(is.infinite(R_0) | is.na(R_0), 0, R_0)
    return(R_0)
  }

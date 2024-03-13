
calculate_R0 <- function(f
                         ,p_A
                         ,N_m
                         ,N_c
                         ,alpha = 1/3
                         ,beta = 1
                         ,gamma = 1/5
                         ,mu = 1/365
                         ,mu_m = 1/30) {
  
  N_de <- 1000 - N_c
  rho_A <- (p_A*N_c)/(p_A*N_c + (1-p_A)*N_de )
  
  f1 <- ( f + (1 - f) * rho_A )
  f2 <- ( (1 - f) * rho_A )
  
  term1 <- alpha * beta / mu_m
  term2 <- (alpha * beta * N_m) / ( (gamma + mu) * N_c )
  
  term3 <- rho_A * f1 * ( alpha * f1 - alpha + 1 )
  term4 <- alpha * f2 * f2 * (1 - rho_A)
  
  R_0 <- sqrt( term1 * term2 * (term3 + term4) )
  R_0 <- ifelse(is.infinite(R_0) | is.na(R_0), 0, R_0)
  return(R_0)
}

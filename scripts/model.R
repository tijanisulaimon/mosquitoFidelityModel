
library(deSolve)            

###############################################################################################
#*************** Mosquito-borne virus transmission model with fidelity behaviour **************
#*##############################################################################################

mod_with_fidelity <- function(tt,yy,parms) with(c(parms,as.list(yy)), {
  
  #*****************state variables**********************
  N_c <- S_c + I_c + R_c       # total competent hosts
  N_de <- S_de + I_de + R_de  # total dead-end hosts
  Nh <- N_c +  N_de        # total hosts
  
  Nv <- Sv_c + Iv_c + Sv_de + Iv_de + Nulv    # total vectors
  
  # proportion of blood meals on competent hosts:
  if(hostDefBeh==F){
    rho_c <- (prefComp*N_c/(prefComp*N_c + (1-prefComp)*N_de))
    rho_de <- 1 - rho_c
  }else{
    numBiteV <- Nv*alpha/N_c
    rho_c <- (numBiteV*N_c)/(numBiteV*N_c + ( ((1-prefComp)/prefComp)^(1/m_de) * numBiteV^(m_c/m_de)*N_de ) )
    rho_de <- 1 - rho_c
  }
  #********************************************************
  #*********************odes*****************************
  
  # Nuliparious mosquitoes
  dNulv <- f_m*Nv - alpha*rho_c*Nulv - alpha*rho_de*Nulv  - mu_v*Nulv
    
  # mosquitoes imprinted on competent hosts
  dSv_c <- (alpha*rho_c*Nulv - alpha*rho_c*Nulv*p_hv*I_c/N_c) -  (alpha*p_hv*(rho_c + f*rho_de)*I_c/N_c) *Sv_c - mu_v*Sv_c   # susceptible
  dIv_c <- alpha*rho_c*Nulv*p_hv*I_c/N_c +  (alpha*p_hv*(rho_c + f*rho_de)*I_c/N_c) *Sv_c - mu_v*Iv_c                  # infectious
  
  # mosquitoes imprinted on dead-end hosts
  dSv_de <-  alpha*rho_de*Nulv - (alpha*p_hv*(1-f)*rho_c*I_c/N_c) *Sv_de - mu_v*Sv_de     # susceptible
  dIv_de <- (alpha*p_hv*(1-f)*rho_c*I_c/N_c) *Sv_de - mu_v*Iv_de                       # infectious
  
  # competent hosts
  dS_c <- f_h*N_c - alpha*p_vh*(rho_c + f*rho_de)*Iv_c*S_c/N_c - alpha*p_vh*(1-f)*rho_c*Iv_de*S_c/N_c - mu_h*S_c      # susceptible              
  dI_c <- alpha*p_vh*(rho_c + f*rho_de)*Iv_c*S_c/N_c + alpha*p_vh*(1-f)*rho_c*Iv_de*S_c/N_c - phi_h*I_c - mu_h*I_c    # infected                                
  dR_c <- phi_h*I_c - mu_h*R_c                              # recovered
  
  # dead-end hosts
  dS_de <- f_h*N_de - alpha*p_vh*(rho_de + f*rho_c)*Iv_de*S_de/N_de - alpha*p_vh*(1-f)*rho_de*Iv_c*S_de/N_de - mu_h*S_de     # susceptible              
  dI_de <- alpha*p_vh*(rho_de + f*rho_c)*Iv_de*S_de/N_de + alpha*p_vh*(1-f)*rho_de*Iv_c*S_de/N_de - phi_h*I_de - mu_h*I_de  # infected                                
  dR_de <- phi_h*I_de - mu_h*R_de                              # recovered
  
  return(list( c(dNulv, dSv_c,dIv_c, dSv_de,dIv_de, dS_c,dI_c,dR_c, dS_de,dI_de,dR_de) ))
})

#*******************Function to run model**********************************
sim_fidelity_model <- function(init=initial, tseq = times, modFunction = mod_with_fidelity
                , parms = params()) {
  sim_f_Dat <- as.data.frame(lsoda(init, tseq, modFunction, parms=parms))
  return(sim_f_Dat)
}

simV_fidelity <- Vectorize(sim_fidelity_model)

#######################################
#*************** R_0 **************
#*####################################
###Simple R0 is for the model without fidelity 
calculate_R0_simple <- function(N_m, p_A, N_c, alpha = 1/3, beta = 1, gamma = 1/5, mu = 1/365, mu_m = 1/30) {
  N_de <- 1000 - N_c
  N <- N_c + N_de
  rho_A <- (p_A*N_c)/(p_A*N_c + (1-p_A)*N_de )
  
  R_0 <- sqrt( (rho_A * alpha * beta * N_m / ((gamma + mu) * N)) * (rho_A * alpha * beta *N_c) / (mu_m*N) )
  return(R_0)
}

# Model with fidelity. Check to see this is the same as above when f = 0 (Checked)
calculate_R0 <- function(f, p_A, N_m, N_c, alpha = 1/3, beta = 1, gamma = 1/5, mu = 1/365, mu_m = 1/30) {
  
  N_de <- 1000 - N_c
  N <- N_c + N_de
  rho_A <- (p_A*N_c)/(p_A*N_c + (1-p_A)*N_de )
  
  f1 <- ( f + (1 - f) * rho_A )
  f2 <- ( (1 - f) * rho_A )
  
  R0_mos_imp_A_by_host_A <- 
    (f1*alpha*beta*rho_A*alpha*N_m + rho_A*alpha*beta*(1-alpha)*N_m)/( (gamma + mu) * N_c )
  
  R0_mos_imp_D_by_host_A <- (f2*alpha*beta*(1-rho_A)*alpha*N_m)/( (gamma + mu) * N_c )
  
  R0_host_A_by_mos_imp_A <- (f1*alpha*beta)/mu_m
  
  R0_host_A_by_mos_imp_D <- (f2*alpha*beta)/mu_m
  
  R_0 <- sqrt(
    R0_mos_imp_A_by_host_A*R0_host_A_by_mos_imp_A + R0_mos_imp_D_by_host_A*R0_host_A_by_mos_imp_D
  )

  R_0 <- ifelse(is.infinite(R_0) | is.na(R_0), 0, R_0)
  return(R_0)
}

### Feeding pattern experiment, based on initial pref, host density and fidelity value
feeding_preference <- function(p_A, H_A, H_D, N_m, f) {
  
  rho_A <- (p_A * H_A) / (p_A * H_A + (1 - p_A) * H_D)
  rho_D <- 1 - rho_A
  
  initial_pigs <- round(rho_A * N_m)
  initial_cows <- N_m - initial_pigs
  
  AA <- (f + (1 - f) * rho_A)
  AD <- ((1 - f) * (1 - rho_A))
  DD <- (f + (1 - f) * (1 - rho_A))
  DA <- ((1 - f) * rho_A)
  
  pigs_to_pigs <- round(AA * initial_pigs)
  pigs_to_cows <- round(AD * initial_pigs)
  
  cows_to_cows <- round(DD * initial_cows)
  cows_to_pigs <- round(DA * initial_cows)
  
  feeding_res <- tibble(
    feeding_pattern = c("A", "AA", "AD", "D", "DA", "DD"),
    percent_fed = c(rho_A, AA, AD, rho_D, DA, DD),
    num_fed = c(initial_pigs, pigs_to_pigs, pigs_to_cows, initial_cows, cows_to_pigs, cows_to_cows)#,
  #  first_meal = c(initial_pigs, initial_pigs, initial_cows, initial_cows)
  ) #%>% 
    # mutate(
    #   Behavior = if_else(feeding_pattern %in% c("PP", "CC"), "Loyal", "Non-loyal")
    # )
  
  return(feeding_res)
}

# parameter estimate function, given experimental data
# probability.of.data <- function(f, 
#                                 n,
#                                 r,
#                                 N,
#                                 R,
#                                 rho_A,
#                                 rho_D){
#   probBiteCowImprintedPig <- (1-f)*rho_D
#   probBiteCowImprintedCow <- rho_D + f*rho_A
#   y <- dbinom(r,n,probBiteCowImprintedPig) * dbinom(R,N,probBiteCowImprintedCow)
#   # y <- (comb(n,r) * probBiteCowImprintedPig^r * (1-probBiteCowImprintedPig)^(n-r)) * 
#   #  (comb(N,R) * probBiteCowImprintedCow^R * (1-probBiteCowImprintedCow)^(N-R))
#   return(y)
# }

## Use likelihood function
neg_log_likelihood <- function(f, n_imprinted_pig, r_pig_pig, n_imprinted_cow, r_cow_cow, rho_A) {
  rho_D <- 1 - rho_A
  probBitePigImprintedPig <- f + (1 - f) * rho_A
  probBiteCowImprintedCow <- rho_D + f * rho_A
  likelihood_pig_pig <- dbinom(r_pig_pig, n_imprinted_pig, probBitePigImprintedPig)
  likelihood_cow_cow <- dbinom(r_cow_cow, n_imprinted_cow, probBiteCowImprintedCow)
  return(-(log(likelihood_pig_pig * likelihood_cow_cow)))
}

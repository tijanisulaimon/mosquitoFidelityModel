# packages required
library(ggplot2)
library(reshape)
library(grid)
library(gridExtra)
library(lhs)
library(parallel)
library(here)
library(tidyverse)

# read in scripts
source(here(".//scripts//model.R"))
source(here(".//scripts//parameters.R"))

# feeding pattern experiment (number of CP and CP always the same, why?, lazy to do the math)
# Too many scenarios?
feeding_exp_df <- expand_grid(
  p_A = c(0.01, 0.05, 0.1, 0.2, 0.5),
  H_A = 500,  
  H_D = 500,  
  N_m = 5000,   
  f = c(0, 0.2, 0.5, 0.8, 1)    
)

# Apply feeding_preference function to each row
feeding_exp_df <- feeding_exp_df %>%
  rowwise() %>%
  mutate(
    feeding_res = list(feeding_preference(p_A, H_A, H_D, N_m, f))
  ) %>%
  unnest(cols = c(feeding_res))

## Plotting the bar plot faceted by p_A and f
## Deal with labeller() later
ggplot(feeding_exp_df %>% 
         mutate(labelled_f = paste0("Fidelity = ", f), 
                labelled_p_A = paste0("Initial preference\nfor pigs = ", p_A),
                bite_order = if_else(feeding_pattern %in% c("D", "A"), "First bite", "subsequent bites" ) ),
       aes(x = feeding_pattern, y = percent_fed*100, label = num_fed, fill = bite_order ) ) +
  geom_bar(stat = "identity", width = 0.9, show.legend = T) +
  geom_text(parse = TRUE, vjust = -0.5, size = 3, position = position_dodge(0.9)) +
  labs(x = "Feeding pattern", y = "% fed") +
  facet_grid(labelled_f ~ labelled_p_A) +
  scale_fill_brewer(name = "", palette = "Dark2") +
  scale_y_continuous(n.breaks = 9)+
  geom_vline(xintercept = 3.5) +
  theme_bw()+
  expand_limits(x = 0, y = 1.1*100)+
  theme(panel.grid = element_blank(),
        legend.position = "top",
        axis.text.x  = element_text(colour = "black", face = "bold", size = 11),
        axis.text.y  = element_text(colour = "black", size = 11),
        axis.title = element_text(colour = "black", size = 16),
        strip.background = element_rect(colour = "black", fill = "white"),
        strip.text = element_text(colour = "black", face = "bold", size = 14),
        legend.key.height = unit(0, "cm"),
        legend.margin=margin(0,0,0,0),
        legend.text = element_text(colour = "black", face = "bold", size = 16)) #+
#  guides(fill = guide_colorbar( barheight = 30))

# looks bettern when you zoom and save with width and height = 12
# ggsave("./figures/choiceExperimentOriginal.pdf", width = 12, height = 12, units = "in")

# plot version for main text
ggplot(
  feeding_exp_df %>% filter(feeding_pattern %in% c("AA", "AD", "DA", "DD") ) %>%
    mutate(first_bite = ifelse(feeding_pattern %in% c("AA", "AD"), "A", "D" ),
           feeding_pattern = factor(feeding_pattern, levels = c("DA","AD","DD","AA")),
           labelled_f = paste0("Fidelity = ", f), 
           labelled_p_A = paste0("Initial preference\nfor amplifying host\nspecies (pigs) = ", p_A)) ,
       aes(x = factor(first_bite, levels = c("D", "A"), labels = c("Cows (D)", "Pigs (A)")), 
           y = num_fed/N_m, fill = feeding_pattern)) +
  geom_bar(stat = "identity", position = "stack", colour = "black" ) +
  facet_grid(labelled_f ~ labelled_p_A) +
  labs(x = "Imprinted host species", y = "Proportion of mosquitoes imprinted onto each species") +
  scale_fill_manual(name = "Subsequent feeds after\nimprinting (first bite)",
                    breaks = c("DD","DA","AD","AA"),
                    values = c("blue","pink","skyblue","red"),
                    labels = c("Cow (imprinted onto cow)", 
                               "Pig (imprinted onto cow)",
                               "Cow (imprinted onto pig)",
                               "Pig (imprinted onto pig)") ) +
  # scale_colour_manual(name = "Imprinted Host",
  #                   breaks = c("C","P"),
  #                   values = c("blue","red")) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.text.x  = element_text(colour = "black", face = "bold", size = 13),
        axis.text.y  = element_text(size = 12),
        axis.title = element_text(size = 16),
        legend.title = element_text(size = 12),
        panel.spacing = unit(0.5, "lines"),
        legend.position = "top",
        strip.background = element_rect(colour = "black", fill = "white"),
        strip.text = element_text(colour = "black", size = 14),
        legend.spacing.y = unit(0.5, 'cm'),
        legend.text = element_text(colour = "black", face = "bold", size = 12)) #+
 # guides(fill = guide_legend(byrow = TRUE))
# ggsave("./figures/choiceExperiment.pdf", width = 12, height = 12, units = "in")


# set scenario
Nhost <- 1000
M0 <- 5000     # Number of vectors
times <- seq(0,365,1)   # Time steps for reporting
startingS_c <- 500       # Number of susceptible competent hosts
deadEnds <- Nhost - startingS_c
prefComp <- 0.5


##################################
#********R_0 exploration*********
R0_exp_df <- expand.grid(
  f = seq(0, 1, 0.1),
  N_c = seq(0, 1000, 100),
  N_m = c(1, 5, 10, 20, 50)*Nhost,
  p_A = c(0.01, 0.05, 0.1, 0.2, 0.5) 
) 

# Appy R0 function on parameter combinations
R0_exp_df <- R0_exp_df %>%
  rowwise() %>%
  mutate(
    R0 = list(calculate_R0(f = f, p_A = p_A, N_m = N_m, N_c = N_c))
  ) %>%
  unnest(cols = c(R0))

# # Line plot for R0
# R0_exp_df %>%
#   ggplot(aes(x = f, y = R0)) +
#   geom_line(aes(group = factor(N_c), color = factor(N_c/Nhost)), linewidth = 0.8) +
#   geom_hline(aes(yintercept = 1), linewidth = 0.8) +
#   facet_grid(N_m/Nhost ~ p_A, scales = "free_y") +
#   scale_x_continuous(breaks = seq(0, 1, 0.2)) +
#   labs(x = "Fidelity", y = expression(R[0]), color = expression(N[A]/N)) +
#   theme(legend.position = "bottom",
#         panel.border = element_rect(fill = NA),
#         panel.grid = element_blank(),
#         axis.text = element_text(colour = "black", face = "bold"),
#         strip.background = element_rect(colour = "black", fill = "white"),
#         strip.text = element_text(colour = "black", face = "bold"),
#         legend.box.background = element_rect(colour = "black", fill = "white"),
#         legend.text = element_text(colour = "black", face = "bold")) +
#   guides(colour = guide_legend(nrow = 1))

# cols <- c("#0020E9", "#0076FF", "#00B8C2", "#04E466", "#49FB25", 
#           "#E7FD09", "#FEEA02", "#FFC200", "#FFA500", "#FF8C00", 
#           "#FF0000", "#DC143C", "#8B0000")
# 
# # As heat plot
# R0_exp_df %>% mutate(R0_mod = if_else(R0 < 1, NA, R0) ) %>% 
#   ggplot(aes(x = f, y = N_c/Nhost)) +
#   geom_raster(aes(fill = R0_mod) ) +  
#   facet_grid(N_m/Nhost ~ p_A, scales = "free_y") +
#   scale_x_continuous(breaks = seq(0, 1, 0.2)) +
#   scale_y_continuous(breaks = seq(0, 1, 0.2)) +
#   scale_fill_gradientn(colours = cols, breaks = seq(0, max(R0_exp_df$R0), 5), na.value = "darkgray")+
#  #  scale_fill_viridis_c(option = "H", breaks = seq(0, max(R0_exp_df$R0), 5), na.value = "darkgray") + 
#   labs(x = "Fidelity", fill = expression(R[0]), y = expression(N[A]/N)) +
#   theme_classic()+
#   theme(panel.grid = element_blank(),
#         axis.text = element_text(colour = "black", face = "bold"),
#         strip.background = element_rect(colour = "black", fill = "white"),
#         strip.text = element_text(colour = "black", face = "bold"),
#         legend.text = element_text(colour = "black", face = "bold")) +
#   guides(fill = guide_colorbar( barheight = 30))


# R0 value plot: final modification - AW
library(leaflet)
# Custom palette for >1 R values
pal = colorNumeric(rev("OrRd"), 1:6)

R0_exp_df %>% mutate(
  prefComp.pretty = paste0("Initial preference\nfor amplifying\nhost = ", p_A),
  mosPerHost.pretty = 
    factor(paste0("Mosquitoes\nper host = ", N_m/Nhost), levels = paste0("Mosquitoes\nper host = ", unique(N_m/Nhost)) ) 
  ) %>% 
  mutate(R0 = cut(R0, breaks= c(0, 1, 2, 4, 6, 8, 10, max(R0)%>%ceiling), include.lowest = TRUE, right = F)) %>%   # bin the r0 values
  ggplot(aes(x = f, y = N_c/Nhost)) +
  geom_raster(aes(fill = R0) ) +  
  facet_grid(mosPerHost.pretty ~ prefComp.pretty, scales = "free_y") +
  scale_x_continuous(breaks = seq(0, 1, 0.2)) +
  scale_y_continuous(breaks = seq(0, 1, 0.2)) +
  scale_fill_manual(values = c("skyblue", pal(1:6))) +
  labs(x = "Fidelity", fill = expression(R[0]), 
       y = expression( paste("Proportion of the host population that are amplifying hosts (", N[A]/N,")" ) ) ) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.text = element_text(colour = "black", size = 10),
        axis.title = element_text(colour = "black", size = 16),
        strip.background = element_rect(colour = "black", fill = "white"),
        strip.text = element_text(colour = "black", face = "bold", size = 14),
        legend.text = element_text(colour = "black", face = "bold", size = 12)) 
# ggsave("./figures/R0.pdf", width = 12, height = 12, units = "in")

# initial conditions for simulation (scenario not included in paper)
initial <- c(Nulv = M0
             ,Sv_c = 0 #prefComp*M0      # susceptible mosquitoes
             ,Iv_c = 1               # infectious mosquitoes
             ,Sv_de = 0 # (1-prefComp)*M0      # susceptible mosquitoes
             ,Iv_de = 0               # infectious mosquitoes
             ,S_c = startingS_c      # susceptible competent hosts
             ,I_c = 0               # infected competent hosts
             ,R_c = 0               # recovered competent hosts
             ,S_de = deadEnds      # susceptible dead-end hosts
             ,I_de = 0               # infected dead-end hosts
             ,R_de = 0               # recovered dead-end hosts
)

#********test run the model*********
sim_f_Dat <- as.data.frame(lsoda(y = initial  # initial state variables
                              , times = times
                              , func = mod_with_fidelity   # see model.R
                              , parms = params(prefComp = prefComp, f = 0) ))  # preference and numbers of dead-end hosts per patch

# post process to calculate total (susc. and inf.) mosquito populations
sim_f_Dat <- sim_f_Dat %>% 
  mutate(
    Sv = Nulv + Sv_c + Sv_de,
    Iv = Iv_c + Iv_de
  )

# plot not included in the paper: delete?
sim_f_Dat %>% select(-c(Sv, Iv)) %>% 
  pivot_longer(cols = !time, names_to = "class", values_to = "pop") %>% 
  mutate(
    type =  case_when(
      class %in% c("S_c", "I_c", "R_c") ~ "Competent_hosts",
      class %in% c("S_de", "I_de", "R_de") ~ "DeadEnd_hosts",
      class %in% c("Sv_c", "Iv_c") ~ "Imprinted_CH",
      class %in% c("Sv_de", "Iv_de") ~ "Imprinted_DH",
      class %in% "Nulv" ~ "Nulv",
      .default = NA),
    State = case_when(
      class %in% c("S_c", "S_de", "Sv_c", "Sv_de", "Nulv") ~ "Susceptible",
      class %in% c("I_c", "I_de", "Iv_c", "Iv_de") ~ "Infected",
      class %in% c("R_c", "R_de") ~ "Recovered",
      .default = NA
    )
  ) %>% filter(type != "Nulv") %>% 
  ggplot()+
  geom_line(aes(x = time,y = pop, colour = State, group = State),linewidth=1) +
  facet_wrap(~ type, scales = "free_y")+
  theme_bw()


#*************************
#*
#*
#

#***************Sensitivity analysis**********************
#*
#* Maybe use R_0 to identify interesting dynamics. E.g., ignore obvious ones, where no epidemic spread
Nhost <- 1000

sensParamsDf <- expand.grid(
  startingNulv = c(5, 10, 20, 50)*Nhost,
  startingS_c =  c(100, 200, 500),
  prefComp = c(0.05, 0.1, 0.2, 0.5),
  fidelity = seq(0, 1, 0.1),
  stringsAsFactors = F
) %>% 
  mutate(
    deadEnds = Nhost - startingS_c,
    mosquitoes.per.host = startingNulv/(startingS_c + deadEnds)
  )


# ********run model for each parameter set******
sensTrajList <- mclapply(1:nrow(sensParamsDf), function(x){
  library(here)
  source(here(".//scripts//model.R"))
  source(here(".//scripts//parameters.R"))

  times <- seq(0,365,1)   # Time steps for reporting

  initial <- c(Nulv = sensParamsDf$startingNulv[x]
               ,Sv_c = 0 #prefComp*M0      # susceptible mosquitoes
               ,Iv_c = 1               # infectious mosquitoes
               ,Sv_de = 0 # (1-prefComp)*M0      # susceptible mosquitoes
               ,Iv_de = 0               # infectious mosquitoes
               ,S_c = sensParamsDf$startingS_c[x]      # susceptible competent hosts
               ,I_c = 0               # infected competent hosts
               ,R_c = 0               # recovered competent hosts
               ,S_de = sensParamsDf$deadEnds[x]      # susceptible dead-end hosts
               ,I_de = 0               # infected dead-end hosts
               ,R_de = 0               # recovered dead-end hosts
  )
  
  #********test run the model*********
  #* No host defensive behaviour influsing vector host choice
  sim.res <- as.data.frame(lsoda(y = initial  # initial state variables
                                   , times = times
                                   , func = mod_with_fidelity   # see model.R
                                   , parms = params(prefComp = sensParamsDf$prefComp[x], 
                                                    f = sensParamsDf$fidelity[x]) ))  

  return( bind_cols(sensParamsDf[x, ], sim.res) )
}, mc.cores = 2 ) 

#******************************************************
sensTrajDF <- do.call(rbind, sensTrajList)

# New catch-all version
sensTrajDF %>% 
  pivot_longer(
    cols = c("I_c", "I_de"),
    names_to = "Host_sp",
    values_to = "I_host"
  ) %>% 
  filter(mosquitoes.per.host == 5, 
         startingS_c %in% c(100, 500),
         fidelity %in% c(0, 0.2, 0.5, 0.8, 1),
         prefComp %in% c(0.05, 0.2)) %>% 
  mutate(startingS_c.pretty = paste(startingS_c, " amplifying\n", deadEnds, " dead-end", sep=""),
         prefComp.pretty = paste("\nInitial amplifying\npreference = ", prefComp, sep=""),
         fidelity.pretty = paste("f = ", fidelity, sep=""),
         fidelity.pretty = ifelse(fidelity.pretty == "f = 0", "f = 0\nNo fidelity", fidelity.pretty),
         fidelity.pretty = ifelse(fidelity.pretty == "f = 0.5", "f = 0.5\nModerate fidelity", fidelity.pretty),
         fidelity.pretty = ifelse(fidelity.pretty == "f = 1", "f = 1\nPerfect fidelity", fidelity.pretty)) %>%
  ggplot()+
  geom_line(aes( x = time, y = I_host, colour = Host_sp), linewidth=1)+
  scale_x_continuous(n.breaks = 3, limits=c(0,NA), expand=c(0,0))+
  scale_y_continuous(limits=c(0, NA), expand=c(0.05, 0.05), position = "left") +
  coord_cartesian(clip = "off") +
  facet_grid(paste0(startingS_c.pretty, "\n", prefComp.pretty) ~ fidelity.pretty) +
  #facet_grid(paste0(startingS_c.pretty, "\n", prefComp.pretty) ~ fidelity.pretty, switch = "y") +
  scale_color_brewer("",
                     palette = "Set1",
                     breaks = c("I_c","I_de"),
                     labels = c("Amplifying host", "Dead-end host")) +
  theme_bw(base_size = 9) +
  ylab("Prevalence") +
  xlab("Time") +
#  ggtitle("Epidemic trajectories") +
  theme(strip.background = element_rect(fill="grey98"),
        strip.placement = "outside",
       # title = element_text(size = 12),
        strip.text = element_text(size = 17),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 16),
        legend.text = element_text(face="bold", size = 16),
        legend.position = "bottom",
        legend.direction = "horizontal")
# ggsave(paste0(here(),"/figures/traj5MosPerHost_revised.pdf"), width = 12, height = 12, units = "in")

#********Specie specific estimates of rho_A and f, with R_0 explorations*********
# Get CI around estimates for initial preference for pigs, rho_A

rhoA_CxT = binom.test(x = 0, n = 10, conf.level = 0.95)
rhoA_CxG = binom.test(x = 14, n = 63, conf.level = 0.95)
rhoA_CxV = binom.test(x = 2, n = 13, conf.level = 0.95)

init_pref_est_df <- data.frame(
  species = c("CxT", "CxG", "CxV"),
  p_A = c(0.05, rhoA_CxG$estimate[[1]], rhoA_CxV$estimate[[1]]),
  p_A_lci = c(rhoA_CxT$conf.int[[1]], rhoA_CxG$conf.int[[1]], rhoA_CxV$conf.int[[1]]),
  p_A_uci = c(rhoA_CxT$conf.int[[2]], rhoA_CxG$conf.int[[2]], rhoA_CxV$conf.int[[2]])
)

# f <- seq(0, 1, 0.01)
# 
# #
# f_est <- NULL
f <- seq(0, 1, 0.01)
likelihoodCxT <- likelihood(f=f, n = 21, r = 9, N = 8, R = 7, rho_A = 0.05, rho_D = 1 - 0.05)
likelihoodCxG <- likelihood(f=f, n = 14, r = 5, N = 5, R = 5, rho_A = 14/63, rho_D = 1 - 14/63)
likelihoodCxV <- likelihood(f=f, n = 12, r = 3, N = 13, R = 13, rho_A = 2/13, rho_D = 1 - 2/13)
# 
f_est <- NULL
f_est$CxG <- f[which.max(likelihoodCxG)]
f_est$CxT <- f[which.max(likelihoodCxT)]
f_est$CxV <- f[which.max(likelihoodCxV)]
f_est

# Use optim to estimate paramters and 
fit_CxT <- optim(par = 0.0001, fn = neg_log_likelihood, lower = 0, upper = 1,
                 n = 21, r = 9, N = 8, R = 7, rho_A = 0.05, rho_D = 1 - 0.05, method = "Brent", hessian =T)

fit_CxG <- optim(par = 0.0001, fn = neg_log_likelihood, lower = 0, upper = 1,
                 n = 14, r = 5, N = 5, R = 5, rho_A = 14/63, rho_D = 1 - 14/63, method = "Brent", hessian =T)

fit_CxV <- optim(par = 0.0001, fn = neg_log_likelihood, lower = 0, upper = 1,
                 n = 12, r = 3, N = 13, R = 13, rho_A = 2/13, rho_D = 1 - 2/13, method = "Brent", hessian =T)

summary_from_optimize <- function(fit){
  hessian <- fit$hessian
  hessian.inv <- solve(hessian)
  parameter.se <- sqrt(diag(hessian.inv))
  parameter.se
  return(c(
    nll = fit$value,
    ll = exp(-fit$value),
    est = fit$par,
    lci = fit$par - 1.96 * parameter.se,
    uci = fit$par + 1.96 * parameter.se
  ))
}

f_estimate_df <- t(sapply(list(CxT = fit_CxT, CxG = fit_CxG, CxV = fit_CxV), summary_from_optimize)) %>% 
  as.data.frame() %>% rownames_to_column("species") 


segment_df <- f_estimate_df %>% 
  mutate(species = factor(species, levels = c("CxG", "CxT", "CxV"),
                           labels = c("Culex gelidus", "Culex tritaeniorhynchus", "Culex vishnui")))

data.frame(f, CxG = likelihoodCxG, CxT = likelihoodCxT, CxV = likelihoodCxV) %>% 
  reshape2::melt(id = f, variable.name = "species") %>% 
  mutate(species = factor(species, levels = c("CxG", "CxT", "CxV"),
                           labels = c("Culex gelidus", "Culex tritaeniorhynchus", "Culex vishnui"))) %>% 
  ggplot(aes(x = f, y = value, colour = species) )+
  geom_density(stat = "identity", linewidth = 1.2) +
  geom_segment(data = segment_df, aes(x = est, y = 0, xend = est, yend =  ll), linetype="dashed", linewidth = 1) +
  scale_x_continuous(breaks = seq(0, 1, 0.1)) +
  scale_colour_brewer(palette = "Set1", direction = -1) +
  labs(x = "Fidelity", y = "Probability of biting an host given fidelity", colour = "Mosquito species") +
  theme_classic()+
  theme(legend.position = "inside", 
        legend.position.inside = c(0.2, 0.6), #"bottom",
        legend.background = element_blank(),
        legend.key.size = unit(1, "cm"),
        axis.text = element_text(colour = "black", size = 16), 
        axis.title = element_text(colour = "black", size = 18),
        legend.title = element_text(colour = "black", size = 16),
        legend.box.background = element_blank(),
        legend.text = element_text(colour = "black", face = "bold", size = 16))

# ggsave("./figures/fidelity_estimate.pdf", width = 10, height = 10, units = "in")

##################################
#********R_0 exploration*********

Nhost <- 1000 

# # select random sets of parameter values within parameter value ranges
rLHS <- randomLHS(1000, nrow(f_estimate_df) )
r_fidelity_Vals <- lapply(1:nrow(f_estimate_df), function(x){
  temp <- f_estimate_df[x, ]
  randomSample <- runif(rLHS[ ,x], min=temp$lci, max=temp$uci)
})

r_InitPref_Vals <- lapply(1:nrow(init_pref_est_df), function(x){
  temp <- init_pref_est_df[x, ]
  randomSample <- runif(rLHS[ ,x], min=temp$p_A_lci, max=temp$p_A_uci)
})

r_fidelity_Vals <- do.call(cbind.data.frame, r_fidelity_Vals)

names(r_fidelity_Vals) <- f_estimate_df$species

R0_sp_fidelity_df <- reshape2::melt(r_fidelity_Vals, variable.name = "species", value.name = "f") %>% 
  inner_join(init_pref_est_df, by = "species") %>% 
  expand_grid(N_c = seq(10, 1000, 1)) %>% 
  rowwise() %>%
  mutate(R0 = list( calculate_R0(f = f, p_A = p_A, N_m = 5*Nhost, N_c = N_c) ) ) %>%
  unnest(cols = c(R0) )

R0_sp_fidelity_df$pretty_species <-
  factor(R0_sp_fidelity_df$species, levels = c("CxG", "CxT", "CxV"),
         labels = c(paste0("Culex gelidus", " (f = ", f_est$CxG, ")" ),
                    paste0("Culex tritaeniorhynchus", " (f = ", f_est$CxT, ")" ),
                    paste0("Culex vishnui", " (f = ", f_est$CxV, ")" )
                    ))

R0_sp_fidelity_df %>% 
  group_by(species, pretty_species, N_c) %>% 
  # dplyr::summarise(mean_R0 = mean(R0), 
  #                  uci_R0 = Rmisc::CI(R0)[1], 
  #                  lci_R0 = Rmisc::CI(R0)[3], .groups = "drop") %>% 
  dplyr::summarise(mean_R0 = mean(R0), 
                   uci_R0 = quantile(R0)[[2]], 
                   lci_R0 = quantile(R0)[[4]], .groups = "drop") %>% 
  ggplot(aes(x = N_c/Nhost, y = mean_R0, fill = pretty_species, color = pretty_species)) +
  geom_ribbon(aes(ymin = lci_R0, ymax = uci_R0), alpha = 0.2, colour = NA) +
  geom_line(linewidth = 1.2) +
  geom_hline(aes(yintercept = 1), linewidth = 0.8, linetype = "dashed", colour = "gray60") +
  scale_x_continuous(breaks = seq(0, 1, 0.2)) +
  scale_y_continuous(n.breaks = 9, limits = c(0, max(R0_sp_fidelity_df$R0))) +
  scale_colour_brewer(name = "Mosquito species", palette = "Set1", direction = -1) +
  scale_fill_brewer(name = "Mosquito species", palette = "Set1", direction = -1) +
  labs(x = expression( paste("Proportion of amplifying hosts, pigs (", N[A]/N, ")" ) ), 
       y = expression(paste("Basic reproduction number (", R[0], ")" ) )) +
  theme_bw()+
  theme(legend.position = "inside", #"bottom",
        legend.position.inside = c(0.3, 0.7),
       # panel.grid = element_blank(),
        legend.background = element_blank(),
        legend.key.size = unit(1.2, "cm"),
        legend.key.spacing.y = unit(0.5, "cm"),
        axis.text = element_text(colour = "black", size = 14),
        axis.title = element_text(colour = "black", size = 18),
        legend.title = element_text(colour = "black", size = 18),
        legend.text = element_text(colour = "black", face = "bold", size = 16)) 

# ggsave("./figures/R0_sp.pdf", width = 10, height = 10, units = "in")




# R0_sp_df <- bind_rows(
#   data.frame(
#     species = "CxT",
#     mos_sp = paste0("Culex tritaeniorhynchus", " (f = ", f_est$CxT, ")" ), 
#               f = f_est$CxT, # estimated fidelity for Cx tritaeniorhynchus
#               p_A = 0.05,  # estimated preference for a pis
#               N_m = 5*Nhost, # starting number of mosquitoes
#               N_c = seq(10, 1000, 1)  # starting number of pigs
#   ),
#   data.frame(
#     species = "CxG",
#     mos_sp = paste0("Culex gelidus", " (f = ", f_est$CxG, ")" ), 
#               f = f_est$CxG, # estimated fidelity for Cx. gelidus
#               p_A = 14/63,  # estimated preference for a pis
#               N_m = 5*Nhost, # starting number of mosquitoes
#               N_c = seq(10, 1000, 1)#,  # starting number of pigs
#   ),
#   data.frame(
#     species = "CxV",
#     mos_sp = paste0("Culex vishnui", " (f = ", f_est$CxV, ")" ), 
#               f = f_est$CxV, # estimated fidelity for Cx Vishnui
#               p_A = 2/13,  # estimated preference for a pis
#               N_m = 5*Nhost, # starting number of mosquitoes
#               N_c = seq(10, 1000, 1) #,  # starting number of pigs
#   )
# ) 
# 
# # Appy R0 function on parameter combinations
# 
# R0_sp_df <- R0_sp_df %>% 
#   rowwise() %>%
#   mutate(R0 = list( calculate_R0(f = f, p_A = p_A, N_m = N_m, N_c = N_c) ) ) %>%
#   unnest(cols = c(R0) )
# 
# R0_sp_df %>%
#   ggplot(aes(x = N_c/Nhost, y = R0)) +
#   geom_line(aes(group = mos_sp, color = mos_sp), linewidth = 1.2) +
#   #  geom_hline(aes(yintercept = 1), linewidth = 0.8, linetype = "dashed") +
#   #  facet_wrap(~ N_m/Nhost,scales = "free_y", ncol = 1) +
#   scale_x_continuous(breaks = seq(0, 1, 0.2)) +
#   scale_y_continuous(n.breaks = 11) +
#   scale_colour_brewer(palette = "Set1", direction = -1) +
#   labs(x = expression( paste("Proportion of amplifying hosts, pigs (", N[A]/N, ")" ) ), 
#        y = expression(paste("Basic reproduction number (", R[0], ")" ) ), 
#        color = "Mosquito species" ) +
#   theme_classic()+
#   theme(legend.position = c(0.3, 0.75), #"bottom",
#         panel.grid = element_blank(),
#         legend.background = element_blank(),
#         legend.key.size = unit(1, "cm"),
#         axis.text = element_text(colour = "black", size = 14),
#         axis.title = element_text(colour = "black", size = 18),
#         legend.title = element_text(colour = "black", size = 16),
#         #  legend.box.background = element_rect(colour = "black", fill = NA),
#         legend.text = element_text(colour = "black", face = "bold", size = 15)) #+
# # guides(colour = guide_legend(nrow = 1))
# # ggsave("./figures/R0_sp.pdf", width = 12, height = 12, units = "in")




# packages required
library(ggplot2)
library(reshape)
library(grid)
library(gridExtra)
library(lhs)
library(parallel)
library(here)
library(tidyverse)

#setwd("~/Documents/External projects/JEV_models")
# read in scripts
source(here(".//ODEModel_with_fidelity//scripts//model.R"))
source(here(".//ODEModel_with_fidelity//scripts//parameters.R"))

# feeding pattern experiment (number of CP and CP always the same, why?, lazy to do the math)
# Too many scenarios?
feeding_exp_df <- expand_grid(
  p_A = c(0, 0.05, 0.1, 0.3, 0.5), # c(0, 0.1, 0.2, 0.5),
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
                labelled_p_A = paste0("Init. pref. for pigs = ", p_A),
                bite_order = if_else(feeding_pattern %in% c("C", "P"), "First bite", "subsequent bites" ) ),
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
        axis.text.x  = element_text(colour = "black"),
        axis.title = element_text(colour = "black", size = 15),
        strip.background = element_rect(colour = "black", fill = "white"),
        strip.text = element_text(colour = "black", face = "bold", size = 10),
        legend.key.height = unit(0, "cm"),
        legend.margin=margin(0,0,0,0),
        legend.text = element_text(colour = "black", face = "bold", size = 15)) #+
#  guides(fill = guide_colorbar( barheight = 30))

# looks bettern when you zoom and save with width and height = 12
# ggsave("./ODEModel_with_fidelity/figures/choiceExperimentOriginal.pdf", width = 12, height = 12, units = "in")

# plot version for main text
ggplot(
  feeding_exp_df %>% filter(feeding_pattern %in% c("PP", "PC", "CP", "CC") ) %>% 
    mutate(first_bite = ifelse(feeding_pattern %in% c("PP", "PC"), "P", "C" ),
           feeding_pattern = factor(feeding_pattern, levels = c("CP","PC","CC","PP")),
           labelled_f = paste0("Fidelity = ", f), labelled_p_A = paste0("Initial preference\nfor pigs = ", p_A)) , 
       aes(x = first_bite, y = num_fed/N_m, fill = feeding_pattern)) +
  geom_bar(stat = "identity", position = "stack", colour = "black" ) +
  facet_grid(labelled_f ~ labelled_p_A) +
  labs(x = "Imprinted host species", y = "Proportion of mosquitoes\nimprinted on that species") +
  scale_fill_manual(name = "Subsequent feeds after imprinting",
                    breaks = c("CC","CP","PC","PP"),
                    values = c("blue","pink","skyblue","red"),
                    labels = c("Cow (imprinted onto cow)", "Pig (imprinted onto cow)","Cow (imprinted onto pig)","Pig (imprinted onto pig)") ) +
  # scale_colour_manual(name = "Imprinted Host",
  #                   breaks = c("C","P"),
  #                   values = c("blue","red")) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.text.x  = element_text(colour = "black", face = "bold", size = 10),
        strip.background = element_rect(colour = "black", fill = "white"),
        strip.text = element_text(colour = "black", face = "bold"),
        legend.spacing.y = unit(0.5, 'cm'),
        legend.text = element_text(colour = "black", face = "bold")) +
  guides(fill = guide_legend(byrow = TRUE))
# ggsave("./ODEModel_with_fidelity/figures/choiceExperiment.pdf", width = 12, height = 12, units = "in")

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
  p_A = c(0.01, 0.05, 0.1, 0.2) 
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
  prefComp.pretty = paste0("Initial preference for\namplifying host = ", p_A),
  mosPerHost.pretty = 
    factor(paste0("Mosquitoes\nper host = ", N_m/Nhost), levels = paste0("Mosquitoes\nper host = ", unique(N_m/Nhost)) ) 
  ) %>% 
  mutate(R0 = cut(R0, breaks= c(0, 1, 2, 4, 6, 8, 10, max(R0)%>%ceiling), include.lowest = TRUE)) %>%   # bin the r0 values
  ggplot(aes(x = f, y = N_c/Nhost)) +
  geom_raster(aes(fill = R0) ) +  
  facet_grid(mosPerHost.pretty ~ prefComp.pretty, scales = "free_y") +
  scale_x_continuous(breaks = seq(0, 1, 0.2)) +
  scale_y_continuous(breaks = seq(0, 1, 0.2)) +
  scale_fill_manual(values = c("skyblue", pal(1:6))) +
  labs(x = "Fidelity", fill = expression(R[0]), y = expression(N[A]/N)) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.text = element_text(colour = "black", face = "bold"),
        strip.background = element_rect(colour = "black", fill = "white"),
        strip.text = element_text(colour = "black", face = "bold", size = 10),
        legend.text = element_text(colour = "black", face = "bold")) 
# ggsave("./ODEModel_with_fidelity/figures/R0.pdf", width = 12, height = 12, units = "in")

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
  startingNulv = c(1, 5, 10, 20, 50)*Nhost,
  startingS_c =  c(50, 100, 200, 500),
  prefComp = c(0.02, 0.05, 0.1, 0.2),
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
  source(here(".//ODEModel_with_fidelity//scripts//model.R"))
  source(here(".//ODEModel_with_fidelity//scripts//parameters.R"))

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
}, mc.cores = 5 ) 

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
         prefComp.pretty = paste("\nInitial amplifying\npreference: ", prefComp, sep=""),
         fidelity.pretty = paste("f = ", fidelity, sep=""),
         fidelity.pretty = ifelse(fidelity.pretty == "f = 0", "f = 0\nNo fidelity", fidelity.pretty),
         fidelity.pretty = ifelse(fidelity.pretty == "f = 0.5", "f = 0.5\nMorderate fidelity", fidelity.pretty),
         fidelity.pretty = ifelse(fidelity.pretty == "f = 1", "f = 1\nPerfect fidelity", fidelity.pretty)) %>%
  ggplot()+
  geom_line(aes( x = time, y = I_host, colour = Host_sp), linewidth=1)+
  scale_x_continuous(n.breaks = 3, limits=c(0,NA), expand=c(0,0))+
  scale_y_continuous(limits=c(0,NA), expand=c(0.05,0.05), position = "left") +
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
  ggtitle("Epidemic trajectories") +
  theme(strip.background = element_rect(fill="grey98"),
        strip.placement = "outside",
        strip.text = element_text(face="bold", size = 9),
        axis.text = element_text(face="bold"),
        legend.text = element_text(face="bold", size = 12),
        legend.position = "bottom",
        legend.direction = "horizontal")
# ggsave(paste0(here(),"/ODEModel_with_fidelity/figures/traj5MosPerHost_revised.pdf"))



#********Specie specific estimates of rho_A and f, with R_0 explorations*********
# Get CI around estimates for initial preference for pigs, rho_A
binom.test(x = 0, n = 10, conf.level = 0.95)
binom.test(x = 14, n = 63, conf.level = 0.95)
binom.test(x = 2, n = 13, conf.level = 0.95)

f <- seq(0, 1, 0.01)

# 
f_est <- NULL
likelihoodCxT <- probability.of.data(f, n = 21, r = 9, N = 8, R = 7, rho_A = 0.05, rho_D = 1 - 0.05)
likelihoodCxG <- probability.of.data(f, n = 14, r = 5, N = 5, R = 5, rho_A = 14/63, rho_D = 1 - 14/63)
likelihoodCxV <- probability.of.data(f, n = 12, r = 3, N = 13, R = 13, rho_A = 2/13, rho_D = 1 - 2/13)

f_est$CxG <- f[which.max(likelihoodCxG)]
f_est$CxT <- f[which.max(likelihoodCxT)]
f_est$CxV <- f[which.max(likelihoodCxV)]
f_est

# plot(f,likelihoodCxT, type = 'l'
#      , xlab = "Fidelity of Cx. tritaeniorhynchus (f)" , ylab ='Probability of Biting an Host Given Fidelity')
# abline(v=f_est$CxT,lty=2,col='gray40')
# 
# plot(f,likelihoodCxG, type = 'l',
#      xlab = "Fidelity of Cx. gelidus (f)" , ylab ='Probability of Biting an Host Given Fidelity' )
# abline(v=f_est$CxG,lty=2,col='gray40')
# 
# plot(f,likelihoodCxV, type = 'l', 
#      xlab = "Fidelity of Cx. vishui (f)", ylab ='Probability of Biting an Host Given Fidelity')
# abline(v=f_est$CxV,lty=2,col='gray40')

segment_df <- data.frame(f_est) %>% 
  melt() %>% 
  cbind(maxLik = c(max(likelihoodCxG), max(likelihoodCxT), max(likelihoodCxV)) ) %>% 
  mutate(variable = factor(variable, levels = c("CxG", "CxT", "CxV"),
                           labels = c("Culex gelidus", "Culex tritaeniorhynchus", "Culex vishnui")))

data.frame(f, CxG = likelihoodCxG, CxT = likelihoodCxT, CxV = likelihoodCxV) %>% 
  reshape2::melt(id = f) %>% 
  mutate(variable = factor(variable, levels = c("CxG", "CxT", "CxV"),
                           labels = c("Culex gelidus", "Culex tritaeniorhynchus", "Culex vishnui"))) %>% 
  # group_by(variable) %>% 
  # mutate(grp_max = f[which.max(value)], yend = max(value)) %>% 
  ggplot(aes(x = f, y = value, colour = variable) )+
  geom_density(stat = "identity", linewidth = 1.2) +
  geom_segment(data = segment_df, aes(x = value, y = 0, xend = value, yend =  maxLik), linetype="dashed", linewidth = 1) +
  # geom_vline(aes(xintercept = grp_max), linetype="dashed")+
  #facet_wrap(~ factor(variable, labels = c("Culex tritaeniorhynchus", "Culex gelidus", "Culex vishnui"))) +
  scale_x_continuous(breaks = seq(0, 1, 0.1)) +
  scale_colour_brewer(palette = "Set1", direction = -1) +
  labs(x = "Fidelity", y = "Probability of biting an host given fidelity", colour = "Mosquito species") +
  theme_classic()+
  theme(legend.position = c(0.2, 0.6), #"bottom",
        legend.background = element_blank(),
        legend.key.size = unit(1, "cm"),
        axis.text = element_text(colour = "black", size = 12), 
        axis.title = element_text(colour = "black", size = 18),
        legend.title = element_text(colour = "black", size = 15),
        legend.box.background = element_blank(),
        legend.text = element_text(colour = "black", face = "bold", size = 15))

# ggsave("./ODEModel_with_fidelity/figures/fidelity_estimate.pdf", width = 12, height = 12, units = "in")


##################################
#********R_0 exploration*********
#*# remember values that are fixed, and the ones we want to explore
#*# function(f, p_A, N_m, N_c, alpha = 1/3, beta = 1, gamma = 1/5, mu = 1/365, mu_m = 1/30)
Nhost <- 1000 

R0_sp_df <- bind_rows(
  expand_grid(mos_sp = paste0("Culex tritaeniorhynchus", " (f = ", f_est$CxT, ")" ), 
              f = f_est$CxT, # estimated fidelity for Cx tritaeniorhynchus
              p_A = 0.05,  # estimated preference for a pis
              N_m = 5*Nhost, # starting number of mosquitoes
              N_c = seq(10, 1000, 1)  # starting number of pigs
              # ,alpha = 1/5, # estimated biting rate
              # mu_m = -log(0.72) # estimated mortality rate
  ),
  expand_grid(mos_sp = paste0("Culex gelidus", " (f = ", f_est$CxG, ")" ), 
              f = f_est$CxG, # estimated fidelity for Cx. gelidus
              p_A = 14/63,  # estimated preference for a pis
              N_m = 5*Nhost, # starting number of mosquitoes
              N_c = seq(10, 1000, 1)#,  # starting number of pigs
              # alpha = 1/8, # estimated biting rate
              # mu_m = -log(0.8) # estimated mortality rate
  ),
  expand_grid(mos_sp = paste0("Culex vishnui", " (f = ", f_est$CxV, ")" ), 
              f = f_est$CxV, # estimated fidelity for Cx Vishnui
              p_A = 2/13,  # estimated preference for a pis
              N_m = 5*Nhost, # starting number of mosquitoes
              N_c = seq(10, 1000, 1)#,  # starting number of pigs
              # alpha = 1/5, # estimated biting rate
              # mu_m = -log(0.90) # estimated mortality rate
  )
) 


# Appy R0 function on parameter combinations

R0_sp_df <- R0_sp_df %>%
  rowwise() %>%
  mutate(R0 = list( calculate_R0(f = f, p_A = p_A, N_m = N_m, N_c = N_c) ) ) %>%
  unnest(cols = c(R0) )

R0_sp_df %>%
  ggplot(aes(x = N_c/Nhost, y = R0)) +
  geom_line(aes(group = mos_sp, color = mos_sp), linewidth = 1.2) +
  #  geom_hline(aes(yintercept = 1), linewidth = 0.8, linetype = "dashed") +
  #  facet_wrap(~ N_m/Nhost,scales = "free_y", ncol = 1) +
  scale_x_continuous(breaks = seq(0, 1, 0.2)) +
  scale_y_continuous(n.breaks = 11) +
  scale_colour_brewer(palette = "Set1", direction = -1) +
  labs(x = expression( paste("Proportion of amplifying hosts, pigs (", N[A]/N, ")" ) ), 
       y = expression(paste("Basic reproduction number (", R[0], ")" ) ), 
       color = "Mosquito species" ) +
  theme_classic()+
  theme(legend.position = c(0.3, 0.75), #"bottom",
        panel.grid = element_blank(),
        legend.background = element_blank(),
        legend.key.size = unit(1, "cm"),
        axis.text = element_text(colour = "black", size = 12),
        axis.title = element_text(colour = "black", size = 18),
        legend.title = element_text(colour = "black", size = 15),
        #  legend.box.background = element_rect(colour = "black", fill = NA),
        legend.text = element_text(colour = "black", face = "bold", size = 15)) #+
# guides(colour = guide_legend(nrow = 1))
# ggsave("./ODEModel_with_fidelity/figures/R0_sp.pdf", width = 12, height = 12, units = "in")



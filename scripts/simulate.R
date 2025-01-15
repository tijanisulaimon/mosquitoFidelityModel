
# packages required
library(reshape)
library(grid)
library(gridExtra)
library(lhs)
library(parallel)
library(here)
library(tidyverse)
library(kableExtra)
library(leaflet)

# read in scripts
source(here(".//scripts//model.R"))
source(here(".//scripts//parameters.R"))

#***************feeding pattern experiment**********************
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

## Plotting the bar plot faceted by p_A and f - Figure A in S1 Text.
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
        legend.text = element_text(colour = "black", face = "bold", size = 16)) 
# ggsave("./figures/choiceExperimentOriginal.pdf", width = 12, height = 12, units = "in")

# plot version for main text - Figure 2
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
        legend.text = element_text(colour = "black", face = "bold", size = 12)) 
# ggsave("./figures/choiceExperiment.pdf", width = 12, height = 12, units = "in")


# set scenarios
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

# AW: R0 value plot: custom palette for >1 R values
pal = colorNumeric(rev("OrRd"), 1:6)

# Figure 3
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


#***************disease dynamics**********************
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

#********run model for each parameter set******
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

# # Figure 4
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
  scale_color_brewer("",
                     palette = "Set1",
                     breaks = c("I_c","I_de"),
                     labels = c("Amplifying host", "Dead-end host")) +
  theme_bw(base_size = 9) +
  ylab("Prevalence") +
  xlab("Time") +
  theme(strip.background = element_rect(fill="grey98"),
        strip.placement = "outside",
        strip.text = element_text(size = 17),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 16),
        legend.text = element_text(face="bold", size = 16),
        legend.position = "bottom",
        legend.direction = "horizontal")
# ggsave(paste0(here(),"/figures/traj5MosPerHost_revised.pdf"), width = 12, height = 12, units = "in")


# Create the data
cumInc_tab <- sensTrajDF %>%
  filter(mosquitoes.per.host == 5,
         startingS_c %in% c(100, 500),
         fidelity %in% c(0, 0.2, 0.5, 0.8, 1),
         prefComp %in% c(0.05, 0.2)) %>%
  mutate(
    cummInc_c = round(startingS_c - S_c, 2),
    cummInc_de = round(deadEnds - S_de, 2),
    PropCompHost = startingS_c/(startingS_c + deadEnds)
  ) %>% 
  filter(time == max(times)) %>% 
  select(prefComp, fidelity, PropCompHost, cummInc_c, cummInc_de) %>% 
  arrange(prefComp, fidelity, PropCompHost) %>% 
  rename(
    `Preference for competent hosts` = prefComp,
    `Fidelity` = fidelity,
    `Proportion of competent hosts` = PropCompHost,
    `Competent hosts` = cummInc_c,
    `Dead-end hosts` = cummInc_de
  ) 


# Export the tidy table to LaTeX - Table B in S1 Text
kable(cumInc_tab, format = "latex", booktabs = TRUE, escape = FALSE,
             table.envir = "table", align='l', label = "tab:cumInc",
             caption = "Cumulative incidence of infected hosts"
      ) %>% 
  kable_styling(full_width = T) %>%
  column_spec(c(1, 3), width = "9em") %>% 
  column_spec(c(4, 5), width = "7em") %>% 
  column_spec(2, width = "5em") %>% 
  add_header_above(header = c("Scenario" = 3, "Cumulative incidence" = 2)) %>% 
  collapse_rows(columns = 1:3, latex_hline = "major", row_group_label_position = "stack")



#********Specie specific estimates of rho_A and f, with R_0 explorations*********
# Get CI around estimates for initial preference for pigs, rho_A
# Perform binomial tests
rhoA_CxT <- binom.test(x = 0, n = 10, p = 0.5, conf.level = 0.95)
rhoA_CxG <- binom.test(x = 14, n = 63, p = 0.5, conf.level = 0.95)
rhoA_CxV <- binom.test(x = 2, n = 13, p = 0.5, conf.level = 0.95)

# Tidy up the results
init_pref_res <- bind_rows(
  broom::tidy(rhoA_CxT) %>% mutate(Species = "Cx. tritaeniorhynchus"),
  broom::tidy(rhoA_CxG) %>% mutate(Species = "Cx. gelidus"),
  broom::tidy(rhoA_CxV) %>% mutate(Species = "Cx. vishnui")
)

# pigs were not fed on in the experiment, so set rho_A to a small value within range
init_pref_res$estimate[init_pref_res$Species == "Cx. tritaeniorhynchus"] <- 0.05

# Select and organize columns
init_pref_tab <- init_pref_res %>%
  select(Species, estimate, conf.low, conf.high, p.value) %>%
  mutate(p_estimate_95CI = paste0(round(estimate, 2), " (", 
                                round(conf.low, 2), ", ", round(conf.high, 2), ")")) %>%
  select(Species, p_estimate_95CI, p.value) %>%
  rename("p_value" = p.value)

init_pref_tab


# Use optim to estimate paramters and 
fit_CxT <- optim(par = 0.0001, fn = neg_log_likelihood, lower = 0, upper = 1,
                 n_imprinted_pig = 21, r_pig_pig = 12, n_imprinted_cow = 8, r_cow_cow = 7, rho_A = 0.05, method = "Brent", hessian =T)

fit_CxG <- optim(par = 0.0001, fn = neg_log_likelihood, lower = 0, upper = 1,
                 n_imprinted_pig = 14, r_pig_pig = 9, n_imprinted_cow = 5, r_cow_cow = 5, rho_A = 14/63, method = "Brent", hessian =T)

fit_CxV <- optim(par = 0.0001, fn = neg_log_likelihood, lower = 0, upper = 1,
                 n_imprinted_pig = 12, r_pig_pig = 9, n_imprinted_cow = 13, r_cow_cow = 13, rho_A = 2/13, method = "Brent", hessian =T)

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

# Get fidelity estimates for each species
fidelity_estimates <- t(sapply(list(CxT = fit_CxT, CxG = fit_CxG, CxV = fit_CxV), summary_from_optimize)) %>%
  as.data.frame() %>% rownames_to_column("Species")

# Rename columns for clarity
colnames(fidelity_estimates) <- c("Species", "negative_loglik", "loglik", 
                                  "Fidelity_estimate", 
                                  "Fidelity_lower", 
                                  "Fidelity_upper")

# Ensure consistency of species names between both datasets
fidelity_estimates$Species <- recode(fidelity_estimates$Species,
                                "CxT" = "Cx. tritaeniorhynchus",
                                "CxG" = "Cx. gelidus",
                                "CxV" = "Cx. vishnui")
# Table A
xtable::print.xtable(xtable::xtable(init_pref_tab), include.rownames = F, booktabs = T)

# Merge initial preference and fidelity estimates
init_pref_tab <- init_pref_tab %>%
  left_join(fidelity_estimates, by = "Species") %>% 
  mutate(f_stimate_95CI = paste0(round(Fidelity_estimate, 2), " (", 
                round(Fidelity_lower, 2), ", ", round(Fidelity_upper, 2), ")")) %>% 
  select(c(Species, p_estimate_95CI, f_stimate_95CI))

init_pref_tab

# Plot the log-likelihoods - Figure B in S1 Text
expand_grid(f = seq(0, 1, length.out = 1000), Species = fidelity_estimates$Species) %>% 
  left_join(init_pref_res %>% select(Species, estimate), by = "Species") %>%
  left_join(fidelity_estimates %>% select(Species, loglik), by = "Species") %>% 
  mutate(
    log_likelihood = case_when(
      Species == "Cx. tritaeniorhynchus" ~ neg_log_likelihood(f = f, n_imprinted_pig = 21, r_pig_pig = 12, n_imprinted_cow = 8, r_cow_cow = 7, rho_A = estimate),
      Species == "Cx. gelidus" ~ neg_log_likelihood(f = f, n_imprinted_pig = 14, r_pig_pig = 9, n_imprinted_cow = 5, r_cow_cow = 5, rho_A = estimate),
      Species == "Cx. vishnui" ~ neg_log_likelihood(f = f, n_imprinted_pig = 12, r_pig_pig = 9, n_imprinted_cow = 13, r_cow_cow = 13, rho_A = estimate),
      TRUE ~ NA_real_ 
    ),
    log_likelihood = exp(-log_likelihood)
  ) %>% 
  ggplot(aes(x = f, y = log_likelihood, colour = Species) )+
  geom_density(stat = "identity", linewidth = 1.2) +
  geom_segment(data = fidelity_estimates, aes(x = Fidelity_estimate, y = 0, xend = Fidelity_estimate, yend =  loglik), 
               linetype="dashed", linewidth = 1) +
  scale_x_continuous(breaks = seq(0, 1, 0.1)) +
  scale_colour_brewer(palette = "Set1", direction = -1) +
  labs(x = "Fidelity", y = "Likelihood", colour = "Mosquito species") +
  theme_classic()+
  theme(legend.position = "inside", 
        legend.position.inside = c(0.2, 0.6), #"bottom",
        legend.background = element_blank(),
        legend.key.size = unit(1, "cm"),
        axis.text = element_text(colour = "black", size = 16), 
        axis.title = element_text(colour = "black", size = 18),
        legend.title = element_text(colour = "black", size = 16),
        legend.box.background = element_blank(),
        legend.text = element_text(colour = "black", face = "italic", size = 16))

# ggsave("./figures/fidelity_estimate.pdf", width = 10, height = 10, units = "in")

##################################
#********R_0 exploration with species-specific fidelity values*********
Nhost <- 1000 

# # select random sets of plausible fidelity values within the ranges estimated from log-likelihood estimation
rLHS <- randomLHS(1000, nrow(fidelity_estimates) )
r_fidelity_Vals <- lapply(1:nrow(fidelity_estimates), function(x){
  temp <- fidelity_estimates[x, ]
  randomSample <- runif(rLHS[ ,x], min=temp$Fidelity_lower, max=temp$Fidelity_upper)
})


r_fidelity_Vals <- bind_cols(r_fidelity_Vals)

names(r_fidelity_Vals) <- fidelity_estimates$Species

R0_sp_fidelity_df <- reshape2::melt(r_fidelity_Vals, variable.name = "Species", value.name = "f") %>% 
  left_join(init_pref_res, by = "Species") %>% 
  left_join(fidelity_estimates, by = "Species") %>% 
  expand_grid(N_c = seq(10, 1000, 1)) %>% 
  mutate(R0 = calculate_R0(f = f, p_A = estimate, N_m = 5*Nhost, N_c = N_c),
         Sepcies_est = paste0(Species, " (f = ", round(Fidelity_estimate, 2), ")") )

# Figure 5
R0_sp_fidelity_df %>% 
  group_by(Species, N_c, Sepcies_est) %>% 
  dplyr::summarise(mean_R0 = mean(R0), 
                   uci_R0 = quantile(R0)[[2]], 
                   lci_R0 = quantile(R0)[[4]], .groups = "drop") %>% 
  ggplot(aes(x = N_c/Nhost, y = mean_R0, fill = Sepcies_est, color = Sepcies_est)) +
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
        legend.text = element_text(colour = "black", face = "italic", size = 16)) 

# ggsave("./figures/R0_sp.pdf", width = 10, height = 10, units = "in")



library(parallel)
library(lhs)

##

#******************************************************
# f <- c(0,1) 
# p_A <- c(0,1)
# N_m <- c(100, 10000)
# N_c <- c(100, 1000)
# alpha <- c(1/6,1/2)
# beta <- c(0,1)
# gamma <- c(2,7)
# mu <- c(1/1095,1/90)
# mu_m <- c(1/35,1/15)

f <- c(0, 1) 
p_A <- c(0, 0.5)
N_m <- c(100, 50000) #Jen, adjusting the max to 5 mos per host to match Fig. 3. shld discuss
N_c <- c(100, 1000)
alpha <- c(1/6, 1/2)
beta <- c(0, 1)
gamma <- c(2, 7)
mu <- c(1/1095, 1/90)
mu_m <- c(1/35, 1/15)

min <- c(f[1],p_A[1],N_m[1],N_c[1],alpha[1],beta[1],gamma[1],mu[1],mu_m[1])
max <- c(f[2],p_A[2],N_m[2],N_c[2],alpha[2],beta[2],gamma[2],mu[2],mu_m[2])

params <- c("f"
            ,"p_A" 
            ,"N_m"
            ,"N_c"
            ,"alpha"
            ,"beta"
            ,"gamma"
            ,"mu"
            ,"mu_m")

params <- cbind.data.frame(params,min,max)

# # select random sets of parameter values within parameter value ranges
r <- randomLHS(1000,length(params[,1]))
parmVals <- lapply(1:length(params[,1]),function(x){
  temp <- params[x,]
  randomSample <- runif(r[,x],min=temp$min,max=temp$max)
})
parmVals <- do.call(cbind.data.frame,parmVals)
names(parmVals) <- params$params

randSnd <- parmVals
randSnd$run <- 1:1000

no_cores <- detectCores() - 1
cl <- makeCluster(no_cores)

sensSims <- parRapply(cl,randSnd,function(x){
 # source(".\\scripts\\r0.R")
  source(".//scripts/r0.R")
  sim.res <- calculate_R0(f=x[1]
                          ,p_A=x[2]
                          ,N_m=x[3]
                          ,N_c=x[4]
                          ,alpha =x[5]
                          ,beta = x[6]
                          ,gamma =x[7]
                          ,mu = x[8]
                          ,mu_m = x[9])
  run <- x[10]
  return(cbind.data.frame(sim.res,run))
  
})

stopCluster(cl)

sensSims <- do.call(rbind,sensSims)
head(sensSims)

dat <- cbind.data.frame(randSnd, sensSims) 
dat$mos_per_host <- dat$N_m/dat$N_c # calculate mos per host


dat <- reshape::melt(dat, measure.vars=c("f"
                               ,"p_A" 
                               ,"N_m"
                               ,"N_c"
                               ,"alpha"
                               ,"beta"
                               ,"gamma"
                               ,"mu"
                               ,"mu_m"
                               ,"mos_per_host"
))

# head(dat)

library(ggplot2)

# ggplot(dat, aes(x=value,y=sim.res)) +
#   geom_point(size=0.01,col="black",alpha=0.2) +
#   facet_wrap(~variable,scales="free_x") +
#   xlab("Parameter value") +
#   ylab("R0") +
#   theme_set(theme_bw())  +
#   theme(panel.border = element_blank()
#         ,axis.line = element_line(color = 'grey')
#         ,text=element_text(size=9)
#         ,panel.spacing = unit(1, "lines")
#         #,plot.margin=unit(c(0.2,0.1,0.1,0.1), "cm")
#         ,axis.text=element_text(size=6)
#         ,legend.key.size = unit(0.6,"line")
#         ,legend.background = element_blank()
#         ,legend.text=element_text(size=9)
#         ,legend.position =c(0.9,0.5)
#         ,legend.title = element_text(size=9)
#         ,strip.background = element_rect(fill="white",color="white")
#   ) 


ggplot(dat %>% filter(!(variable %in% c("N_m", "N_c"))), aes(x=value, y=sim.res)) +
  geom_point(size=0.1, col="black",alpha=0.9) +
  facet_wrap(~ factor(variable, levels = c("f", "p_A", "mos_per_host", "alpha", "beta", "gamma", "mu", "mu_m"),
                     labels = c("f", expression(p[A]), expression(N[m]/N[A]), 
                                "alpha", "beta", "gamma", "mu", expression(mu[m])) ), 
             scales="free_x", labeller = label_parsed, nrow = 4) +
  xlab("Parameter value") +
  ylab( expression(paste("Basic reproduction number (", R[0], ")" ) ) ) +
  theme_set(theme_bw())  +
  theme_classic() + 
  theme(panel.border = element_blank()
        ,axis.line = element_line(color = 'grey')
        ,axis.title=element_text(size=15)
        , strip.text = element_text(size = 20)
        ,panel.spacing = unit(1, "lines")
        ,plot.margin=unit(c(0.1,0.5,0.1,0.1), "cm")
        ,axis.text=element_text(size = 14)
        ,strip.background = element_rect(fill="white",color="white")
  ) 
# ggsave("./figures/global_sensitivity_analysis.pdf", width = 12, height = 12, units = "in")




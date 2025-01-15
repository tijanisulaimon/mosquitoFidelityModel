
library(parallel)
library(lhs)
library(ggplot2)
set.seed(15012025)
#***********************Global sensitivity analysis*******************************

f <- c(0, 1) 
p_A <- c(0, 0.1)
Nmph <- c(1,100) # 
N_c <- c(100, 1000)
alpha <- c(1/6, 1/2)
beta <- c(0, 1)
gamma <- c(2, 7)
mu <- c(1/1095, 1/90)
mu_m <- c(1/35, 1/15)

min <- c(f[1],p_A[1],Nmph[1],N_c[1],alpha[1],beta[1],gamma[1],mu[1],mu_m[1])
max <- c(f[2],p_A[2],Nmph[2],N_c[2],alpha[2],beta[2],gamma[2],mu[2],mu_m[2])

params <- c("f"
            ,"p_A" 
            ,"Nmph"
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
  source(".//scripts/r0_JSL.R")
  sim.res <- calculate_R0(f=x[1]
                          ,p_A=x[2]
                          ,Nph=x[3]
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

#************could show effect of fidelity by color scale on rest of plots e.g.**********
dat <- cbind.data.frame(randSnd, sensSims) 
dat$comp2dead <- dat$N_c/1000
dat <- dat[,-4] # remove N_c column

dat <- reshape::melt(dat, measure.vars=c(
                               "p_A" 
                               ,"Nmph" # number of mosquitoes per host
                               ,"alpha"
                               ,"beta"
                               ,"gamma"
                               ,"mu"
                               ,"mu_m"
                               ,"comp2dead"
))

# head(dat)

# Figure C in S1 Text
ggplot(dat, aes(x=value,y=sim.res,colour=f)) +
  geom_point(size=0.8,alpha=0.5) +
  facet_wrap( ~ factor(variable, levels = c("p_A","Nmph","alpha","beta","gamma","mu","mu_m", "comp2dead"), 
                       labels = c(expression(paste("Initial amplifying host preference (",p[A],")" )  ), 
                                  expression(paste("Number of mosquitoes per host (",N[m]/H[A],")" ) ),
                                  expression(paste("Mosquito biting rate (", alpha, ")")),
                                  expression(paste("Transmission probability (", beta, ")")),
                                  expression(paste("Host recovery rate (", gamma, ")")),
                                  expression(paste("Host mortality rate (", mu, ")")),
                                  expression(paste("Mosquito mortality rate (", mu[m], ")")), 
                                  expression(paste("Number of dead-end host (", H[A]/H[D], ")"))
                                  ) ), 
              scales = "free_x", labeller = label_parsed) +
  scale_color_gradient(name = "Fidelity (f)", low="#0000FF", high="#FF0000", breaks = seq(0, 1, 0.2), 
                       limit = c(0, 1)) +
  xlab("Parameter value") +
  ylab(expression(R[0])) +
  theme_bw()  +
  theme(panel.border = element_blank()
        ,axis.line = element_line(color = 'grey')
        ,text=element_text(size=16)
        ,panel.spacing = unit(1, "lines")
        #,plot.margin=unit(c(0.2,0.1,0.1,0.1), "cm")
        ,axis.text=element_text(size=12)
        ,legend.background = element_blank()
        ,legend.text=element_text(size=12)
        ,legend.title = element_text(size=14)
        ,strip.background = element_rect(fill="white",color="white"),
        legend.position = "inside", 
        legend.position.inside = c(0.83, 0.15),
        legend.key.width = unit(1.5, "cm"),
        legend.key.height = unit(1, "cm"),
        legend.title.position = "top",
        legend.direction = "horizontal"
  ) 
# ggsave("./figures/global_sensitivity_analysis.pdf", width = 12, height = 12, units = "in")
 

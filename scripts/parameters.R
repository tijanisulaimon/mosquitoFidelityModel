
#****************************PARAMETERS***************************
params <- function(          # Parameters used in the model
  f_m = fecundM                # mosquito reproduction rate
  ,f_h = fecundH               # host reproduction rate
  ,mu_v = muMoz              # mosquito mortality rate        
  ,mu_h = muHost             # host mortality rate      
  ,alpha = biteRate          # mosquito bite rate
  ,p_hv = phostVec           # probability of host to vector transmission
  ,p_vh = pVecHost           # probability of vector to host transmission
  ,phi_h = recRate           # host recovery rate
  ,prefComp = 1              # mosquito preference for competence hosts   
  ,N_de = deadEnds             # numbers of dead-end hosts 
  ,hostDefBeh = F              # whether hosts display defensive behaviour which affects vector host choice
  ,m_c = NA                    # parameter for kelly function
  ,m_de = NA                   # parameter for kelly function
  ,f = 0                       # parameter for mosquito fidelity
 # ,M = mMoz
){
  return(as.list(environment()))
}

#******************************************************************

biteRate <- 1/3
muMoz <- 1/30              
muHost <- 1/365  
fecundH <- muHost
fecundM <- muMoz
phostVec <- 1              
pVecHost <- 1                  
recRate <- 1/5
mMoz = 1000
  
  

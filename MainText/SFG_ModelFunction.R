#R code for the base model function to reproduce findings reported in
#Tredennick and Hanan. 2014, American Naturalist

#CONTACT: atredenn@gmail.com

#This file is sourced from other scripts used to run simulations and produce figures
#Reference the main text for additional description of the model
#And see Staver & Levin (2012, American Naturalist) for the original description

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
####                     PARAMETER DEFINITIONS                           ####
#                                                                           #        
# a = forest tree birth rate                                                #
# b = savanna sapling birth rate                                            #
# mu = savanna sapling death rate                                           #
# v = adult savanna tree mortality rate                                     #
# death.intr = forest tree intrinsic (non-fire) mortality rate              #
# death.fire = forest tree "fire" mortality rate                            #
# rec.intr = savanna sapling-to-adult intrinsic (non-fire) recruitment rate #
# rec.fire = savanna sapling-to-adult "fire" recruitment rate               #   
# w = wood harvest rate                                                     #
#                                                                           #
# G = grass cover                                                           #
# S = savanna sapling cover                                                 #
# Tr = savanna adult tree cover                                             #
# Ft = forest adult tree cover                                              #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

#~~~~~~~~~~~~~~~~~~~~~~~#
#### LAMBDA FUNCTION ####
#~~~~~~~~~~~~~~~~~~~~~~~#
#Function for determining average lambda (fire frequency)
lambda <- function(G){
  a <- 0.6
  b <- 0.5
  c <- 1.1
  d <- 4
  f <- b + (c/pi)*atan(pi*d*(G-a))
  return(f)
}

#~~~~~~~~~~~~~~~~~~~~~~#
#### MODEL FUNCTION ####
#~~~~~~~~~~~~~~~~~~~~~~#
#Returns a 'time' X 4 matrix of grass, savanna sapling, 
#savanna tree, and forest tree proportional cover through time
savanna.model <- function(G, S, Tr, Ft, time, a, b, mu, v, w, 
                          death.fire, rec.fire, death.intr, rec.intr){
  for (t in 2:time){ #begin loop throught time
    f <- lambda(G[t-1]) #call to fire function (based on G)
    #make sure lambda is between 0 and 1
    f[f<0] <- 0
    f[f>1] <- 1
    
    #Bernoulli trial for fire event based on fire frequency, lambda
    fire <- rbinom(1,1,f)
    
    #set the demographic rates that vary with fire events
    if (fire==1) {f.Ft <- death.fire} else f.Ft <- death.intr
    if (fire==1) {f.S <- rec.fire} else f.S <- rec.intr
    
    #Base model
    G[t] <- G[t-1] + (mu*S[t-1] + v*Tr[t-1] + f.Ft*Ft[t-1] + w*Ft[t-1] - b*G[t-1]*Tr[t-1] - a*G[t-1]*Ft[t-1])
    
    S[t] <- S[t-1] + (b*G[t-1]*Tr[t-1] + w*Tr[t-1] - f.S*S[t-1] - mu*S[t-1] - a*S[t-1]*Ft[t-1])
    
    Tr[t] <- Tr[t-1] + (f.S*S[t-1] - v*Tr[t-1] - a*Tr[t-1]*Ft[t-1] - w*Tr[t-1])
    
    Ft[t] <- Ft[t-1] + ((a*(1-Ft[t-1]) - f.Ft - w)*Ft[t-1])
    
  }  
  #Store results of simulation
  results = matrix(ncol=4, nrow=time)
  results[,1] <- G
  results[,2] <- S
  results[,3] <- Tr
  results[,4] <- Ft
  return(results)
} #End of model function
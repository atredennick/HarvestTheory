#############################################################################
# Parameter definitions                                                     #
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
#############################################################################

################################ FUNCTIONS ##################################
#############################################################################
#Function for determining average lambda (fire frequency)
lambda <- function(G){
  a <- 0.6
  b <- 0.5
  c <- 1.1
  d <- 4
  f <- b + (c/3.14)*atan(3.14*d*(G-a))
  return(f)
}

#Model function; returns a 'time' X 4 matrix of grass, savanna sapling, 
#savanna tree, and forest tree proportional cover through time
savanna.model <- function(G, S, Tr, Ft, time, a, b, mu, v, w, 
                          death.fire, rec.fire, death.intr, rec.intr){
  for (t in 2:time){ #begin loop throught time
    f <- lambda(G[t-1])
    #make sure lambda is between 0 and 1
    f[f<0] <- 0
    f[f>1] <- 1
    
    #Bernoulli trial for fire event based on fire frequency, lambda
    fire <- rbinom(1,1,f)
    
    #set the demographic rates that vary with fire events
    if (fire==1) {f.Ft <- death.fire} else f.Ft <- death.intr
    if (fire==1) {f.S <- rec.fire} else f.S <- rec.intr
    #     if (G[t-1] > 0.6) {f.Ft <- death.fire} else f.Ft <- death.intr
    #     if (G[t-1] > 0.6) {f.S <- rec.fire} else f.S <- rec.intr
    
    #Base model
    dTr <-  (f.S*S[t-1] - v*Tr[t-1] - a*Tr[t-1]*Ft[t-1]) 
    dFt <- ((a*(1-Ft[t-1]) - f.Ft)*Ft[t-1])
    dG <- (mu*S[t-1] + v*Tr[t-1] + f.Ft*Ft[t-1] - b*G[t-1]*Tr[t-1] - a*G[t-1]*Ft[t-1])
    dS <- (b*G[t-1]*Tr[t-1] - f.S*S[t-1] - mu*S[t-1] - a*S[t-1]*Ft[t-1])
    
    rhoF <- 0
    rhoT <- 0
    #set up constant harvest offtake
    totT <- (Tr[t-1]+Ft[t-1])
    if(totT > 0){
      rhoDiv <- Tr[t-1]/totT
      rhoT <- w*rhoDiv
      rhoF <- w*(1-rhoDiv)
      
      #rescale harvest to avoid negatives and over-pulses to grass and saplings
      tmpT <- Tr[t-1] + dTr - rhoT
      tmpRhoT <- rhoT + tmpT
      if (tmpT < 0) {rhoT <- tmpRhoT} else rhoT <- rhoT
      
      tmpF <- Ft[t-1] + dFt - rhoF
      tmpRhoF <- rhoF + tmpF
      if (tmpF < 0) {rhoF <- tmpRhoF} else rhoF <- rhoF
    }
    
    
    #Now take off harvest fractions and add where necessary
    G[t] <- G[t-1] + dG + rhoF
    S[t] <- S[t-1] + dS + rhoT
    Tr[t] <- Tr[t-1] + dTr - rhoT
    Ft[t] <- Ft[t-1] + dFt - rhoF
    
    if (G[t]<0) {G[t] <- 0} else G[t] <- G[t]
    if (S[t]<0) {S[t] <- 0} else S[t] <- S[t]
    if (Tr[t]<0) {Tr[t] <- 0} else Tr[t] <- Tr[t]
    if (Ft[t]<0) {Ft[t] <- 0} else Ft[t] <- Ft[t]
    
  }  
  results = matrix(ncol=4, nrow=time)
  results[,1] <- G
  results[,2] <- S
  results[,3] <- Tr
  results[,4] <- Ft
  return(results)
}
#End of model function
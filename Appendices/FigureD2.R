# Figures for Appendix #, Tredennick and Hanan
# Contact: atredenn@gmail.com
# See license at: http://github.com/atredennick/HarvestTheory

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
####                   Parameter definitions                             ####
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

#Clear everything
rm(list=ls())

#Load libraries
library(grDevices)
library(gplots)

#Set output file names for the plots -------------------
figureSavanna2Grass_MuOmega <- "/Users/atredenn/Dropbox/HarvestTheory/BivariatePlots/Savanna2Grass_MuOmega.pdf"
figureSavanna2Grass_MuNu <- "/Users/atredenn/Dropbox/HarvestTheory/BivariatePlots/Savanna2Grass_MuNu.pdf"
figureSavanna2Grass_MuBeta <- "/Users/atredenn/Dropbox/HarvestTheory/BivariatePlots/Savanna2Grass_MuBeta.pdf"
figureSavanna2Grass_BetaOmega <- "/Users/atredenn/Dropbox/HarvestTheory/BivariatePlots/Savanna2Grass_BetaOmega.pdf"
figureSavanna2Grass_NuBeta <- "/Users/atredenn/Dropbox/HarvestTheory/BivariatePlots/Savanna2Grass_NuBeta.pdf"
figureSavanna2Grass_NuOmega <- "/Users/atredenn/Dropbox/HarvestTheory/BivariatePlots/Savanna2Grass_NuOmega.pdf"

tOut <- 5000
demLengths <- 50

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
##### Figure A#: Mu v Omega ######
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#load function files
source("/Users/atredenn/Dropbox/HarvestTheory/Code/SFG_ModelFunction_StepFireFxn.R")

### Set-up simulation for a plot of harvest intensity vs. average G over 10,000 years
w.s = seq(0,1,by=0.02)

a=0.3
b = 0.05
mu = seq(0,0.1, length.out=demLengths)
v = 0.005
time=tOut
t.min=time/2
death.fire = 0.3
recF = seq(0,0.5, length.out=demLengths)
death.intr = 0.005
rec.intr = 0.3

#State variable storage vectors
Tr=numeric(time)
S=numeric(time)
G=numeric(time)
Ft=numeric(time)
# #Starting values
Tr[1] = 0.4
S[1] = 0.4
G[1] = 0.2
Ft[1] = 0

#Set up matrix to hold the value of rho at which a permanent transition occurs
pStorage <- matrix(ncol=length(recF), nrow=length(mu))

#Loop through fire-year death rates
for(k in 1:length(recF)){
  rec.fire <- recF[k] #set current fire-year death rate
  outAvg <- array(dim=c(length(w.s), 4, length(mu))) #array to store simulation state variables
  #Loop through forest tree birth rates
  for(j in 1:length(mu)){
    #Loop through harvest rates
    for(i in 1:length(w.s)){
      outs <- savanna.model(G=G,
                            S=S,
                            Tr=Tr,
                            Ft=Ft,
                            time=time,
                            a=a,
                            b=b,
                            mu=mu[j],
                            v=v,
                            w=w.s[i],
                            death.fire=death.fire,
                            rec.fire=rec.fire,
                            death.intr=death.intr,
                            rec.intr=rec.intr)
      outAvg[i,1,j] <- mean(outs[t.min:time,1])
      outAvg[i,2,j] <- mean(outs[t.min:time,2])
      outAvg[i,3,j] <- mean(outs[t.min:time,3])
      outAvg[i,4,j] <- mean(outs[t.min:time,4])
    }
  }
  pStar <- numeric(length(mu)) #storage vector for rho
  
  #Loop through mu and w.s to find rho at which transition occurs
  for(i in 1:length(mu)){
    tmpD <- outAvg[,,i]
    tmpAvg <- length(w.s)
    for(j in 1:length(w.s)){
      tmpAvg[j] <- mean(tmpD[j:length(tmpD[,1]), 1])
    }
    index <- which(tmpAvg>0.99) #NOTE: this is not 0 because of rounding errors (e.g., 
                                  #some states returned as 1.19e-20, which is effectively 0)
    pStar[i] <- w.s[index[1]]
  }  
  pStorage[,k] <- pStar
}

levs <- 20

pdf(file=figureSavanna2Grass_MuOmega, width=5, height=4)
filled.contour(mu, recF, pStorage, nlevels=levs, zlim=c(0,1),
               col=colorpanel(levs,"beige", "darkblue"),
               xlab=expression(mu), ylab=expression(omega[1]), 
               key.title=title(main=expression(rho[transition])))
dev.off()



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
##### Figure A#: Mu v Nu ######
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#load function files
source("/Users/atredenn/Dropbox/HarvestTheory/Code/SFG_ModelFunction_StepFireFxn.R")

### Set-up simulation for a plot of harvest intensity vs. average G over 10,000 years
w.s = seq(0,1,by=0.02)

a=0.3
b = 0.05
mu = seq(0,0.1, length.out=demLengths)
vVec = seq(0,0.1, length.out=demLengths)
time=tOut
t.min=time/2
death.fire = 0.3
rec.fire = 0.05
death.intr = 0.005
rec.intr = 0.3

#State variable storage vectors
Tr=numeric(time)
S=numeric(time)
G=numeric(time)
Ft=numeric(time)
# #Starting values
Tr[1] = 0.4
S[1] = 0.4
G[1] = 0.2
Ft[1] = 0

#Set up matrix to hold the value of rho at which a permanent transition occurs
pStorage <- matrix(ncol=length(vVec), nrow=length(mu))

#Loop through fire-year death rates
for(k in 1:length(vVec)){
  v <- vVec[k] #set current fire-year death rate
  outAvg <- array(dim=c(length(w.s), 4, length(mu))) #array to store simulation state variables
  #Loop through forest tree birth rates
  for(j in 1:length(mu)){
    #Loop through harvest rates
    for(i in 1:length(w.s)){
      outs <- savanna.model(G=G,
                            S=S,
                            Tr=Tr,
                            Ft=Ft,
                            time=time,
                            a=a,
                            b=b,
                            mu=mu[j],
                            v=v,
                            w=w.s[i],
                            death.fire=death.fire,
                            rec.fire=rec.fire,
                            death.intr=death.intr,
                            rec.intr=rec.intr)
      outAvg[i,1,j] <- mean(outs[t.min:time,1])
      outAvg[i,2,j] <- mean(outs[t.min:time,2])
      outAvg[i,3,j] <- mean(outs[t.min:time,3])
      outAvg[i,4,j] <- mean(outs[t.min:time,4])
    }
  }
  pStar <- numeric(length(mu)) #storage vector for rho
  
  #Loop through a and w.s to find rho at which transition occurs
  for(i in 1:length(mu)){
    tmpD <- outAvg[,,i]
    tmpAvg <- length(w.s)
    for(j in 1:length(w.s)){
      tmpAvg[j] <- mean(tmpD[j:length(tmpD[,1]), 1])
    }
    index <- which(tmpAvg>0.99) #NOTE: this is not 0 because of rounding errors (e.g., 
    #some states returned as 1.19e-20, which is effectively 0)
    pStar[i] <- w.s[index[1]]
  }  
  pStorage[,k] <- pStar
}

levs <- 20

pdf(file=figureSavanna2Grass_MuNu, width=5, height=4)
filled.contour(mu, vVec, pStorage, nlevels=levs, zlim=c(0,1),
               col=colorpanel(levs,"beige", "darkblue"),
               xlab=expression(mu), ylab=expression(nu), 
               key.title=title(main=expression(rho[transition])))
dev.off()





#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
##### Figure A#: Mu v Beta ######
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#load function files
source("/Users/atredenn/Dropbox/HarvestTheory/Code/SFG_ModelFunction_StepFireFxn.R")

### Set-up simulation for a plot of harvest intensity vs. average G over 10,000 years
w.s = seq(0,1,by=0.02)

a=0.3
bVec = seq(0,0.3, length.out=demLengths)
mu = seq(0,0.3, length.out=demLengths)
v = 0.005
time=tOut
t.min=time/2
death.fire = 0.3
rec.fire = 0.05
death.intr = 0.005
rec.intr = 0.3

#State variable storage vectors
Tr=numeric(time)
S=numeric(time)
G=numeric(time)
Ft=numeric(time)
# #Starting values
Tr[1] = 0.4
S[1] = 0.4
G[1] = 0.2
Ft[1] = 0

#Set up matrix to hold the value of rho at which a permanent transition occurs
pStorage <- matrix(ncol=length(bVec), nrow=length(mu))

#Loop through fire-year death rates
for(k in 1:length(bVec)){
  b <- bVec[k] #set current fire-year death rate
  outAvg <- array(dim=c(length(w.s), 4, length(mu))) #array to store simulation state variables
  #Loop through forest tree birth rates
  for(j in 1:length(mu)){
    #Loop through harvest rates
    for(i in 1:length(w.s)){
      outs <- savanna.model(G=G,
                            S=S,
                            Tr=Tr,
                            Ft=Ft,
                            time=time,
                            a=a,
                            b=b,
                            mu=mu[j],
                            v=v,
                            w=w.s[i],
                            death.fire=death.fire,
                            rec.fire=rec.fire,
                            death.intr=death.intr,
                            rec.intr=rec.intr)
      outAvg[i,1,j] <- mean(outs[t.min:time,1])
      outAvg[i,2,j] <- mean(outs[t.min:time,2])
      outAvg[i,3,j] <- mean(outs[t.min:time,3])
      outAvg[i,4,j] <- mean(outs[t.min:time,4])
    }
  }
  pStar <- numeric(length(mu)) #storage vector for rho
  
  #Loop through a and w.s to find rho at which transition occurs
  for(i in 1:length(mu)){
    tmpD <- outAvg[,,i]
    tmpAvg <- length(w.s)
    for(j in 1:length(w.s)){
      tmpAvg[j] <- mean(tmpD[j:length(tmpD[,1]), 1])
    }
    index <- which(tmpAvg>0.99) #NOTE: this is not 0 because of rounding errors (e.g., 
    #some states returned as 1.19e-20, which is effectively 0)
    pStar[i] <- w.s[index[1]]
  }  
  pStorage[,k] <- pStar
}

levs <- 20

pdf(file=figureSavanna2Grass_MuBeta, width=5, height=4)
filled.contour(mu, bVec, pStorage, nlevels=levs, zlim=c(0,1),
               col=colorpanel(levs,"beige", "darkblue"),
               xlab=expression(mu), ylab=expression(beta), 
               key.title=title(main=expression(rho[transition])))
dev.off()



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
##### Figure A#: Beta v Omega ######
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#load function files
source("/Users/atredenn/Dropbox/HarvestTheory/Code/SFG_ModelFunction_StepFireFxn.R")

### Set-up simulation for a plot of harvest intensity vs. average G over 10,000 years
w.s = seq(0,1,by=0.02)

a=0.3
bVec = seq(0,0.4, length.out=demLengths)
mu = 0.08
v = 0.005
time=tOut
t.min=time/2
death.fire = 0.3
recF = seq(0,0.4, length.out=demLengths)
death.intr = 0.005
rec.intr = 0.3

#State variable storage vectors
Tr=numeric(time)
S=numeric(time)
G=numeric(time)
Ft=numeric(time)
# #Starting values
Tr[1] = 0.4
S[1] = 0.4
G[1] = 0.2
Ft[1] = 0

#Set up matrix to hold the value of rho at which a permanent transition occurs
pStorage <- matrix(ncol=length(bVec), nrow=length(recF))

#Loop through fire-year death rates
for(k in 1:length(recF)){
  rec.fire <- recF[k] #set current fire-year death rate
  outAvg <- array(dim=c(length(w.s), 4, length(bVec))) #array to store simulation state variables
  #Loop through forest tree birth rates
  for(j in 1:length(bVec)){
    #Loop through harvest rates
    for(i in 1:length(w.s)){
      outs <- savanna.model(G=G,
                            S=S,
                            Tr=Tr,
                            Ft=Ft,
                            time=time,
                            a=a,
                            b=bVec[j],
                            mu=mu,
                            v=v,
                            w=w.s[i],
                            death.fire=death.fire,
                            rec.fire=rec.fire,
                            death.intr=death.intr,
                            rec.intr=rec.intr)
      outAvg[i,1,j] <- mean(outs[t.min:time,1])
      outAvg[i,2,j] <- mean(outs[t.min:time,2])
      outAvg[i,3,j] <- mean(outs[t.min:time,3])
      outAvg[i,4,j] <- mean(outs[t.min:time,4])
    }
  }
  pStar <- numeric(length(bVec)) #storage vector for rho
  
  #Loop through a and w.s to find rho at which transition occurs
  for(i in 1:length(bVec)){
    tmpD <- outAvg[,,i]
    tmpAvg <- length(w.s)
    for(j in 1:length(w.s)){
      tmpAvg[j] <- mean(tmpD[j:length(tmpD[,1]), 1])
    }
    index <- which(tmpAvg>0.99) #NOTE: this is not 0 because of rounding errors (e.g., 
    #some states returned as 1.19e-20, which is effectively 0)
    pStar[i] <- w.s[index[1]]
  }  
  pStorage[,k] <- pStar
}

levs <- 20

pdf(file=figureSavanna2Grass_BetaOmega, width=5, height=4)
filled.contour(recF, bVec, pStorage, nlevels=levs, zlim=c(0,1),
               col=colorpanel(levs,"beige", "darkblue"),
               xlab=expression(omega[1]), ylab=expression(beta), 
               key.title=title(main=expression(rho[transition])))
dev.off()







#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
##### Figure A#: Nu v Beta ######
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#load function files
source("/Users/atredenn/Dropbox/HarvestTheory/Code/SFG_ModelFunction_StepFireFxn.R")

### Set-up simulation for a plot of harvest intensity vs. average G over 10,000 years
w.s = seq(0,1,by=0.02)

a=0.3
bVec = seq(0,0.3, length.out=demLengths)
mu = 0.08
vVec = seq(0,0.3, length.out=demLengths)
time=tOut
t.min=time/2
death.fire = 0.3
rec.fire = 0.05
death.intr = 0.005
rec.intr = 0.3

#State variable storage vectors
Tr=numeric(time)
S=numeric(time)
G=numeric(time)
Ft=numeric(time)
# #Starting values
Tr[1] = 0.4
S[1] = 0.4
G[1] = 0.2
Ft[1] = 0

#Set up matrix to hold the value of rho at which a permanent transition occurs
pStorage <- matrix(ncol=length(bVec), nrow=length(vVec))

#Loop through fire-year death rates
for(k in 1:length(vVec)){
  v <- vVec[k] #set current fire-year death rate
  outAvg <- array(dim=c(length(w.s), 4, length(bVec))) #array to store simulation state variables
  #Loop through forest tree birth rates
  for(j in 1:length(bVec)){
    #Loop through harvest rates
    for(i in 1:length(w.s)){
      outs <- savanna.model(G=G,
                            S=S,
                            Tr=Tr,
                            Ft=Ft,
                            time=time,
                            a=a,
                            b=bVec[j],
                            mu=mu,
                            v=v,
                            w=w.s[i],
                            death.fire=death.fire,
                            rec.fire=rec.fire,
                            death.intr=death.intr,
                            rec.intr=rec.intr)
      outAvg[i,1,j] <- mean(outs[t.min:time,1])
      outAvg[i,2,j] <- mean(outs[t.min:time,2])
      outAvg[i,3,j] <- mean(outs[t.min:time,3])
      outAvg[i,4,j] <- mean(outs[t.min:time,4])
    }
  }
  pStar <- numeric(length(bVec)) #storage vector for rho
  
  #Loop through a and w.s to find rho at which transition occurs
  for(i in 1:length(bVec)){
    tmpD <- outAvg[,,i]
    tmpAvg <- length(w.s)
    for(j in 1:length(w.s)){
      tmpAvg[j] <- mean(tmpD[j:length(tmpD[,1]), 1])
    }
    index <- which(tmpAvg>0.99) #NOTE: this is not 0 because of rounding errors (e.g., 
    #some states returned as 1.19e-20, which is effectively 0)
    pStar[i] <- w.s[index[1]]
  }  
  pStorage[,k] <- pStar
}

levs <- 20

pdf(file=figureSavanna2Grass_NuBeta, width=5, height=4)
filled.contour(vVec, bVec, pStorage, nlevels=levs, zlim=c(0,1),
               col=colorpanel(levs,"beige", "darkblue"),
               xlab=expression(nu), ylab=expression(beta), 
               key.title=title(main=expression(rho[transition])))
dev.off()



# 
# 
# #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# ##### Figure A#: Nu v Omega ######
# #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# #load function files
# source("/Users/atredenn/Dropbox/HarvestTheory/Code/SFG_ModelFunction_StepFireFxn.R")
# 
# ### Set-up simulation for a plot of harvest intensity vs. average G over 10,000 years
# w.s = seq(0,1,by=0.02)
# 
# a=0.3
# b = 0.05
# mu = 0.08
# vVec = seq(0,0.1, length.out=30)
# time=500
# t.min=time/2
# death.fire = 0.3
# recF = seq(0,0.1, length.out=30)
# death.intr = 0.005
# rec.intr = 0.3
# 
# #State variable storage vectors
# Tr=numeric(time)
# S=numeric(time)
# G=numeric(time)
# Ft=numeric(time)
# # #Starting values
# Tr[1] = 0.4
# S[1] = 0.4
# G[1] = 0.2
# Ft[1] = 0
# 
# #Set up matrix to hold the value of rho at which a permanent transition occurs
# pStorage <- matrix(ncol=length(recF), nrow=length(vVec))
# 
# #Loop through fire-year death rates
# for(k in 1:length(vVec)){
#   v <- vVec[k] #set current fire-year death rate
#   outAvg <- array(dim=c(length(w.s), 4, length(recF))) #array to store simulation state variables
#   #Loop through forest tree birth rates
#   for(j in 1:length(recF)){
#     #Loop through harvest rates
#     for(i in 1:length(w.s)){
#       outs <- savanna.model(G=G,
#                             S=S,
#                             Tr=Tr,
#                             Ft=Ft,
#                             time=time,
#                             a=a,
#                             b=b,
#                             mu=mu,
#                             v=v,
#                             w=w.s[i],
#                             death.fire=death.fire,
#                             rec.fire=recF[j],
#                             death.intr=death.intr,
#                             rec.intr=rec.intr)
#       outAvg[i,1,j] <- mean(outs[t.min:time,1])
#       outAvg[i,2,j] <- mean(outs[t.min:time,2])
#       outAvg[i,3,j] <- mean(outs[t.min:time,3])
#       outAvg[i,4,j] <- mean(outs[t.min:time,4])
#     }
#   }
#   pStar <- numeric(length(recF)) #storage vector for rho
#   
#   #Loop through a and w.s to find rho at which transition occurs
#   for(i in 1:length(recF)){
#     tmpD <- outAvg[,,i]
#     tmpAvg <- length(w.s)
#     for(j in 1:length(w.s)){
#       tmpAvg[j] <- mean(tmpD[j:length(tmpD[,1]), 1])
#     }
#     index <- which(tmpAvg>0.99) #NOTE: this is not 0 because of rounding errors (e.g., 
#     #some states returned as 1.19e-20, which is effectively 0)
#     pStar[i] <- w.s[index[1]]
#   }  
#   pStorage[,k] <- pStar
# }
# 
# levs <- 20
# 
# # pdf(file=figureSavanna2Grass_NuOmega, width=5, height=4)
# filled.contour(vVec, recF, pStorage, nlevels=levs, zlim=c(0,1),
#                col=colorpanel(levs,"beige", "darkblue"),
#                xlab=expression(nu), ylab=expression(omega[1]), 
#                key.title=title(main=expression(rho[transition])))
# # dev.off()
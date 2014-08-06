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

#Set output file names for the plots
figureForest2Grass <- "/Users/atredenn/Dropbox/HarvestTheory/BivariatePlots/Forest2Grass.pdf"
figureForest2Savanna <- "/Users/atredenn/Dropbox/HarvestTheory/BivariatePlots/Forest2Savanna.pdf"


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
##### Figure A#: 2D Bivariate for Stable Forest Transition to Grassland ######
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#load function files
source("SFG_ModelFunction_StepFireFxn.R")

### Set-up simulation for a plot of harvest intensity vs. average G over 10,000 years
w.s = seq(0,1,by=0.02)

# Parameter values for stable forest
a = seq(0.1, 1, 0.05)
# a=0.3
b = 0.4
mu = 0.3
v = 0.15
time=10000
t.min=time/2
# death.fire = 0.5
deathF = seq(0.1, 1, 0.05)
rec.fire = 0.4
death.intr = 0.005
rec.intr = 0.9

#State variable storage vectors
Tr=numeric(time)
S=numeric(time)
G=numeric(time)
Ft=numeric(time)
# #Starting values
Tr[1] = 0
S[1] = 0
G[1] = 0
Ft[1] = 1

#Set up matrix to hold the value of rho at which a permanent transition occurs
pStorage <- matrix(ncol=length(deathF), nrow=length(a))

#Loop through fire-year death rates
for(k in 1:length(deathF)){
  death.fire <- deathF[k] #set current fire-year death rate
  outAvg <- array(dim=c(length(w.s), 4, length(a))) #array to store simulation state variables
  #Loop through forest tree birth rates
  for(j in 1:length(a)){
    #Loop through harvest rates
    for(i in 1:length(w.s)){
      outs <- savanna.model(G=G,
                            S=S,
                            Tr=Tr,
                            Ft=Ft,
                            time=time,
                            a=a[j],
                            b=b,
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
  pStar <- numeric(length(a)) #storage vector for rho
  
  #Loop through a and w.s to find rho at which transition occurs
  for(i in 1:length(a)){
    tmpD <- outAvg[,,i]
    tmpAvg <- length(w.s)
    for(j in 1:length(w.s)){
      tmpAvg[j] <- mean(tmpD[j:length(tmpD[,1]), 4])
    }
    index <- which(tmpAvg<0.0001) #NOTE: this is not 0 because of rounding errors (e.g., 
                                  #some states returned as 1.19e-20, which is effectively 0)
    pStar[i] <- w.s[index[1]]
  }  
  pStorage[,k] <- pStar
}

levs <- 20

pdf(file=figureForest2Grass, width=5, height=4)
filled.contour(a, deathF, pStorage, nlevels = levs,
               col=colorpanel(levs,"beige", "darkblue"),
               xlab=expression(alpha), ylab=expression(phi[1]), 
               key.title=title(main=expression(rho[transition])))
# mar.orig <- par("mar")
# w <- (3 + mar.orig[2]) * par("csi") * 2.54
# layout(matrix(c(2, 1), nc = 2), widths = c(1, lcm(w)))
# contour(a, deathF, pStorage, add=T, drawlabels = TRUE, nlevels = levs, lwd=0)
# contour(a, deathF, pStorage, add=T, drawlabels = TRUE, nlevels = levs, lwd=0)
dev.off()




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#####  Figure A#: 2D Bivariate for Stable Forest Transition to Savanna  ######
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#load function files
source("/Users/atredenn/Dropbox/HarvestTheory/Code/SFG_SavannaInvasionModule.R")

### Set-up simulation for a plot of harvest intensity vs. average G over 10,000 years
w.s = seq(0,1,by=0.02)

# Parameter values for stable forest
a = seq(0.1, 1, 0.05)
# a=0.3
b = 0.3
mu = 0.15
v = 0.005
time=10000
t.min=time/2
# death.fire = 0.5
deathF = seq(0.1, 1, 0.05)
rec.fire = 0.1
death.intr = 0.005
rec.intr = 0.4
tree.start = 0.01

#State variable storage vectors
Tr=numeric(time)
S=numeric(time)
G=numeric(time)
Ft=numeric(time)
# #Starting values
Tr[1] = 0
S[1] = 0
G[1] = 0
Ft[1] = 1

#Set up matrix to hold the value of rho at which a permanent transition occurs
pStorage <- matrix(ncol=length(deathF), nrow=length(a))

#Loop through fire-year death rates
for(k in 1:length(deathF)){
  death.fire <- deathF[k] #set current fire-year death rate
  outAvg <- array(dim=c(length(w.s), 4, length(a))) #array to store simulation state variables
  #Loop through forest tree birth rates
  for(j in 1:length(a)){
    #Loop through harvest rates
    for(i in 1:length(w.s)){
      outs <- savanna.invasion(G=G,
                            S=S,
                            Tr=Tr,
                            Ft=Ft,
                            time=time,
                            a=a[j],
                            b=b,
                            mu=mu,
                            v=v,
                            w=w.s[i],
                            death.fire=death.fire,
                            rec.fire=rec.fire,
                            death.intr=death.intr,
                            rec.intr=rec.intr,
                            tree.start=tree.start)
      outAvg[i,1,j] <- mean(outs[t.min:time,1])
      outAvg[i,2,j] <- mean(outs[t.min:time,2])
      outAvg[i,3,j] <- mean(outs[t.min:time,3])
      outAvg[i,4,j] <- mean(outs[t.min:time,4])
    }
  }
  pStar <- numeric(length(a)) #storage vector for rho
  
  #Loop through a and w.s to find rho at which transition occurs
  for(i in 1:length(a)){
    tmpD <- outAvg[,,i]
    tmpAvg <- length(w.s)
    for(j in 1:length(w.s)){
      tmpAvg[j] <- mean(tmpD[j:length(tmpD[,1]), 4])
    }
    index <- which(tmpAvg<0.0001) #NOTE: this is not 0 because of rounding errors (e.g., 
    #some states returned as 1.19e-20, which is effectively 0)
    pStar[i] <- w.s[index[1]]
  }  
  pStorage[,k] <- pStar
}

levs <- 20

pdf(file=figureForest2Savanna, width=5, height=4)
filled.contour(a, deathF, pStorage, nlevels = levs, zlim = c(0,1),
               col=colorpanel(levs,"beige", "darkblue"),
               xlab=expression(alpha), ylab=expression(phi[1]), 
               key.title=title(main=expression(rho[transition])))
dev.off()





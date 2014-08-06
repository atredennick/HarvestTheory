##Script to create Figures 2B and 2C from Tredennick and Hanan 2014

# Parameter definitions--------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
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


rm(list=ls()) #clear workspace

# load function files----------------------
source("SFG_ModelFunction.R")

# load libraries---------------------------
library(ggplot2)
library(RColorBrewer)
library(grid)
library(plyr)
library(reshape2)



# Figure 2B and 2C simulations-------------

# Set-up simulation for a plot of harvest intensity vs. average S+T over 10,000 years
w.s = seq(0,0.1,by=0.005)

# Parameter values for stable savanna
a = 0.3
b = 0.05
mu = 0.1
v = 0.005
death.fire = 0.9
death.intr = 0.005
rec.vec = c(0.01, 0.05, 0.1, 0.15, 0.2) #vector of fire-year recruitment
rec.intr = 0.2
time = 10000
t.min = time/2

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

#Storage matrices for simulation outputs
savanna.avg <- matrix(nrow=length(w.s), ncol=length(rec.vec))
s.ratio <- matrix(nrow=length(w.s), ncol=length(rec.vec))

for (i in 1: length(w.s)){
  for (j in 1:length(rec.vec)){
    outs <- savanna.model(G=G,
                          S=S,
                          Tr=Tr,
                          Ft=Ft,
                          time=time,
                          a=a,
                          b=b,
                          mu=mu,
                          v=v,
                          w=w.s[i],
                          death.fire=death.fire,
                          rec.fire=rec.vec[j],
                          death.intr=death.intr,
                          rec.intr=rec.intr)
    savanna.avg[i,j] <- mean(outs[t.min:time,2] + outs[t.min:time,3])
    s.ratio[i,j] <- mean(outs[t.min:time,3])/mean(outs[t.min:time,2])
  }
}

df.avg <- data.frame(alpha1=savanna.avg[,1],
                     alpha2=savanna.avg[,2],
                     alpha3=savanna.avg[,3],
                     alpha4=savanna.avg[,4],
                     alpha5=savanna.avg[,5])

df.ratio <- data.frame(alpha1=s.ratio[,1],
                       alpha2=s.ratio[,2],
                       alpha3=s.ratio[,3],
                       alpha4=s.ratio[,4],
                       alpha5=s.ratio[,5])


df.m <- melt(data=df.avg)
df.m$harvest=w.s

df.r <- melt(data=df.ratio)
df.r$harvest=w.s



# Plotting set up-------------------------
labs = as.character(rec.vec)
y.labs = rep(1.02,7)
x.labs = c(0.05, 0.1, 0.19, 0.3, 0.37, 0.5, 0.6)
colors = c("black", "darkorange", "steelblue", "purple", "grey45")

# Figure 2B---------------------------------
ggplot(data=df.m, aes(x=harvest, y=value)) +
  geom_line(aes(color=variable), alpha=0.3) +
  geom_point(color="white", size=6) +
  geom_point(aes(shape=variable, color=variable), size=3) +
  scale_color_manual(name = "Fire-year sapling recruitment rate",
                     values = colors,
                     labels=labs) +
  scale_shape_manual(name = "Fire-year sapling recruitment rate",
                     values = c(1,2,3,4,5),
                     labels=labs) +
  scale_x_continuous(limits = c(0, 0.1)) + 
  scale_y_continuous(limits = c(0, 1)) +
  theme_bw() +
  xlab(expression(paste("Tree Harvest Rate (", rho, ")"))) + 
  ylab("Average Savanna Cover (S+T)") +
  theme(axis.title.x = element_text(size=14),
        axis.title.y = element_text(size=14, angle=90), 
        axis.text.x = element_text(size=12), 
        axis.text.y = element_text(size=12), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = c(0.6,0.8),
        legend.key = element_blank(),
        legend.background = element_rect(colour = NA),
        legend.key.height = unit(4, "mm")
  )


# Figure 2B---------------------------------
ggplot(data=df.r, aes(x=harvest, y=value)) +
  geom_point(aes(shape=variable, color=variable), size=3) +
  scale_color_manual(name = "Fire-year sapling recruitment rate",
                     values = colors,
                     labels=labs) +
  scale_shape_manual(name = "Fire-year sapling recruitment rate",
                     values = c(1,2,3,4,5),
                     labels=labs) +
  scale_x_continuous(limits = c(0, 0.1)) + 
  theme_bw() +
  xlab(expression(paste("Tree Harvest Rate (", rho, ")"))) + 
  ylab("T:S Ratio") +
  theme(axis.title.x = element_text(size=14),
        axis.title.y = element_text(size=14, angle=90), 
        axis.text.x = element_text(size=12), 
        axis.text.y = element_text(size=12), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = c(0.6,0.8),
        legend.key = element_blank(),
        legend.background = element_rect(colour = NA),
        legend.key.height = unit(4, "mm")
  )


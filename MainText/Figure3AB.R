##Script to create Figures 3A and 3B from Tredennick and Hanan 2014

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

#load function files-----------------
source("SFG_ModelFunction.R")

#load libraries----------------------
library(ggplot2)
library(ggthemes)
library(grid)
library(reshape2)


## Figure 3A-------------------------

#Set-up simulation for a plot of harvest intensity vs. average G over 10,000 years
w.s = seq(0,0.1,by=0.005)

# Parameter values for "normal" year
a = 0.3
b = 0.05
mu = 0.08
v = 0.005
time = 10000
t.min = time/2
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

outAvg <- matrix(ncol=4, nrow=length(w.s))

for(i in 1:length(w.s)){
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
                        rec.fire=rec.fire,
                        death.intr=death.intr,
                        rec.intr=rec.intr)
  outAvg[i,1] <- mean(outs[t.min:time,1])
  outAvg[i,2] <- mean(outs[t.min:time,2])
  outAvg[i,3] <- mean(outs[t.min:time,3])
  outAvg[i,4] <- mean(outs[t.min:time,4])
}

df <- data.frame(Grass = outAvg[,1],
                 Sapling = outAvg[,2],
                 Savanna = outAvg[,3],
                 Forest = outAvg[,4],
                 Harvest = w.s)

df.m <- melt(data=df, id.vars="Harvest")


cols <- c("#999999", "#E69F00", "#56B4E9", "darkgreen")
ggplot(data=df.m, aes(x=Harvest, y=value)) +
  geom_line(aes(color=variable))+
  theme_few()+
  ylab("Proportional Abundance") +
  xlab(expression(paste("Harvest Rate (", rho, ")"))) +
  scale_color_manual(values=cols) +
  theme(axis.title.x = element_text(size=14),
        axis.title.y = element_text(size=14, angle=90), 
        axis.text.x = element_text(size=12), 
        axis.text.y = element_text(size=12), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = c(0.8,0.75),
        legend.key = element_blank(),
        legend.background = element_rect(colour = NA),
        legend.key.height = unit(4, "mm"),
        legend.title = element_blank()
  )



## Figure 3B------------------------

# Parameter values for "drought" year
a = 0.3
b = 0.05
mu = 0.15
v = 0.005
time = 10000
t.min = time/2
death.fire = 0.3
rec.fire = 0.05
death.intr = 0.005
rec.intr = 0.1


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

outAvg <- matrix(ncol=4, nrow=length(w.s))

for(i in 1:length(w.s)){
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
                        rec.fire=rec.fire,
                        death.intr=death.intr,
                        rec.intr=rec.intr)
  outAvg[i,1] <- mean(outs[t.min:time,1])
  outAvg[i,2] <- mean(outs[t.min:time,2])
  outAvg[i,3] <- mean(outs[t.min:time,3])
  outAvg[i,4] <- mean(outs[t.min:time,4])
}

df <- data.frame(Grass = outAvg[,1],
                 Sapling = outAvg[,2],
                 Savanna = outAvg[,3],
                 Forest = outAvg[,4],
                 Harvest = w.s)
df.m <- melt(data=df, id.vars="Harvest")

cols <- c("#999999", "#E69F00", "#56B4E9", "darkgreen")
ggplot(data=df.m, aes(x=Harvest, y=value)) +
  geom_line(aes(color=variable))+
  theme_few()+
  ylab("Proportional Abundance") +
  xlab(expression(paste("Harvest Rate (", rho, ")"))) +
  scale_color_manual(values=cols) +
  theme(axis.title.x = element_text(size=14),
        axis.title.y = element_text(size=14, angle=90), 
        axis.text.x = element_text(size=12), 
        axis.text.y = element_text(size=12), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = c(0.8,0.75),
        legend.key = element_blank(),
        legend.background = element_rect(colour = NA),
        legend.key.height = unit(4, "mm"),
        legend.title = element_blank()
  )




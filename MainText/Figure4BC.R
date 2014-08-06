##Script to create Figures 4B and 4C from Tredennick and Hanan 2014

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

# load function files---------------------
source("SFG_ModelFunction.R")

# load libraries
library(plyr)
library(reshape2)
library(ggplot2)
library(ggthemes)

# Figure 4B-------------------------------
### Set-up simulation for a plot of harvest intensity vs. average G over 10,000 years
w.s = seq(0,0.1,by=0.0005)

# Parameter values for stable forest
a = 0.3
b = 0.4
mu = 0.3
v = 0.15
time = 10000
t.min = time/2
death.fire = 0.5
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


# Figure 4C------------------------
#load the savanna invasion module of the model
source("SFG_SavannaInvasionModule.R")

w.s = 0.05 #set wood harvest scalar

# Parameter values for stable forest
a = 0.3
b = 0.3
mu = 0.15
v = 0.005
time = 500
death.fire = 0.5
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
outs <- savanna.invasion(G=G,
                      S=S,
                      Tr=Tr,
                      Ft=Ft,
                      time=time,
                      a=a,
                      b=b,
                      mu=mu,
                      v=v,
                      w=w.s,
                      death.fire=death.fire,
                      rec.fire=rec.fire,
                      death.intr=death.intr,
                      rec.intr=rec.intr,
                         tree.start=tree.start)
tc <- outs[,2]+outs[,3]+outs[,4]
plot(density(tc), lwd=3)

# matplot(c(1:time),outs, type="l")
df <- data.frame(Grass = outs[,1],
                 Sapling = outs[,2],
                 Savanna = outs[,3],
                 Forest = outs[,4])


library(reshape2)
library(plyr)
library(ggplot2)
library(grid)

df.m <- melt(df)
df.m$Time <- seq(1,time,1)

cols <- c("#999999", "#E69F00", "#56B4E9", "darkgreen")

ggplot(data=df.m, aes(x=Time, y=value)) +
  geom_line(aes(color=variable)) +
  theme_bw() + 
  ylab("Proportional Abundance") +
  scale_y_continuous(limits=c(0,1.2)) +
  scale_color_manual(values=cols) +
  theme(axis.title.x = element_text(size=14),
        axis.title.y = element_text(size=14, angle=90), 
        axis.text.x = element_text(size=12), 
        axis.text.y = element_text(size=12), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = c(0.8,0.86),
        legend.key = element_blank(),
        legend.background = element_rect(colour = NA),
        legend.key.height = unit(4, "mm"),
        legend.title = element_blank()
  )


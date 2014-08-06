#Code for Figure 2A; Tredennick and Hanan 2014

rm(list=ls())

#load function files
source("SFG_ModelFunction.R") #loads 'savanna.model' and 'lambda' functions

#Set parameter values
mu = 0.1
beta = 0.2
v = 0.05
p = c(0, 0.2, 0.5, 0.8)
G = seq(0,1.1,0.02)

#Calculate isoclines based on equilibrium solution (see Appendix B)
res1 <- (mu*(v+p[1]))/((beta*G)-(v))
res2 <- (mu*(v+p[2]))/((beta*G)-(v))
res3 <- (mu*(v+p[3]))/((beta*G)-(v))
res4 <- (mu*(v+p[4]))/((beta*G)-(v))

#Get rid of nonsensical values
res1[res1<0] <- NA
res2[res2<0] <- NA
res3[res3<0] <- NA
res4[res4<0] <- NA

#run the lambda function for fire probabilities
f <- lambda(G)

# make sure lambda is between 0 and 1 (normally done within the 'savanna.model' function)
f[f<0] <- 0
f[f>1] <- 1

#Set the sapling recruitment values
rec.fire <- 0.2
rec.intr <- 0.7
f.S = numeric(length(G)) #storage vector for sapling recruitment over G values

#fill the vector
for(i in 1:length(G)){
  if (G[i] > 0.6) {f.S[i] <- rec.fire} else f.S[i] <- rec.intr
}

#set up data frame for ggplot
df <- data.frame(NoRho = res1,
                 LowRho = res2,
                 MedRho = res3,
                 HiRho = res4,
                 Grass = G)

#'melt' the dataframe for plotting
library(reshape2)
dfm <- melt(df, id.vars="Grass")

#set up data frame for sapling recruitment values
dfb <- data.frame(fS = f.S,
                  Grass = G)

#make the plot
library(ggplot2)
library(ggthemes)
library(grid)
ggplot()+
  geom_line(data=dfb, aes(x=Grass, y=fS), size=1)+
  geom_line(data=dfm, aes(x=Grass, y=value, group=variable), color="grey", size=1)+
  geom_vline(xintercept=1, color="grey", linetype="dashed")+
  scale_y_continuous(limits=c(0,1))+
  scale_x_continuous(breaks=c(0,0.5,1))+
  xlab("Grass Proportional Abundance") + ylab("")+
  theme_few()+
  guides(linetype=FALSE)+
  theme(axis.title.x = element_text(size=14),
        axis.title.y = element_text(size=14, angle=90), 
        axis.text.x = element_text(size=12), 
        axis.text.y = element_text(size=12), 
        #         axis.ticks = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = c(0.2,0.2),
        legend.key = element_blank(),
        legend.background = element_rect(colour = NA),
        legend.key.height = unit(4, "mm"),
        legend.title = element_blank()
  )



#Code for Figure 4A; Tredennick and Hanan 2014

rm(list=ls())

#load function files
source("SFG_ModelFunction.R") #loads 'savanna.model' and 'lambda' functions

#Set parameter values
p = c(0, 0.2, 0.5, 0.8)
G = seq(0,1.1,0.02)

#run the lambda function for fire probabilities
f <- lambda(G)

# make sure lambda is between 0 and 1 (normally done within the 'savanna.model' function)
f[f<0] <- 0
f[f>1] <- 1

#Set the forest tree death rates for fire and no-fire years
death.fire <- 0.4
death.intr <- 0.1
f.Ft =numeric(length(G)) #storage vector for forest tree death over G values

#fill in the vector
for(i in 1:length(G)){
  if (G[i] > 0.6) {f.Ft[i] <- death.fire} else f.Ft[i] <- death.intr
}

#set forest tree birth rate for equilibrium condition (see Appendix B)
a=0.5

#isoclines are set within the dataframes
df <- data.frame(Low = f.Ft+p[1],
                 High = f.Ft+p[3],
                 Grass = G)
dfAG <- data.frame(AG = a[1]*G,
                   Grass=G)

#melt the data frame for plotting
library(reshape2)
dfm <- melt(df, id.vars="Grass")

#make the plot
library(ggplot2)
library(ggthemes)
ggplot()+
  geom_line(data=dfm, aes(x=Grass, y=value, group=variable), size=1)+
  geom_line(data=dfAG, aes(x=Grass, y=AG), color="grey", size=1)+
  geom_vline(xintercept=1, color="grey", linetype="dashed")+
  scale_y_continuous(limits=c(0,1))+
  scale_x_continuous(breaks=c(0,0.5,1))+
  xlab("Grass Proportional Abundance") + ylab("")+
  theme_few()+
  theme(axis.title.x = element_text(size=14),
        axis.title.y = element_text(size=14, angle=90), 
        axis.text.x = element_text(size=12), 
        axis.text.y = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = c(0.8,0.86),
        legend.key = element_blank(),
        legend.background = element_rect(colour = NA),
        legend.key.height = unit(4, "mm"),
        legend.title = element_blank()
  )

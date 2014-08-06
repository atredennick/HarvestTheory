rm(list=ls())

#load function files
source("SFG_ModelFunction.R") #loads 'savanna.model' and 'lambda' functions

G=seq(0,1,0.001) #to calculate fire probabilities across the range of grass cover possibilities
f <- lambda(G) #run the lambda function

#set up as data frame for ggplot
df <- data.frame(fire=f,
                 grass=G)

#extra data frame for showing inflection point at 0.6 grass cover
df.line <- data.frame(x=c(0.6, 0.6),
                      y=c(0, 0.5))

#load the ggplot2 library
library(ggplot2) 

#make the plot
ggplot(data=df) +
  geom_line(aes(x=grass, y=fire), size=2) +
  geom_line(data=df.line, aes(x=x, y=y), color="grey", size=2) +
  geom_point(aes(x=0.6, y=0.5), size=6, color="grey") +
  theme_bw() +
  xlab("Grass Cover (G)") + 
  ylab(expression(paste("Fire Probability (", lambda, ")"))) +
  theme(axis.title.x = element_text(size=14),
        axis.title.y = element_text(size=14, angle=90), 
        axis.text.x = element_text(size=12), 
        axis.text.y = element_text(size=12), 
        legend.position = "right",
        legend.key = element_blank() 
  )

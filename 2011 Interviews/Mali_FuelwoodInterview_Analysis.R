rm(list=ls())
library(ggplot2)
library(plyr)
library(metafor)

site.labels = c("Korkodio", "Lakamane", "Negela", "Fiela", "Tiorola", "Tiendega")

#Bring in the data
data.raw <- as.data.frame(read.csv("/Users/atredenn/Documents/Projects/SSDE-Project/Data/2011/2011 Interviews/FuelwoodInterviews-Summer2011.csv", 
                                   na.strings="#DIV/0!"))
names(data.raw)

data.raw <- arrange(data.raw, desc(MAP))

#Remove NA and incomplete records
data <- data.raw[complete.cases(data.raw$Annual.Wood.Demand..kg.cap.year.),]
dataL <- subset(data, Site=="BLakamane")
dataT <- subset(data, Site=="ETiendega")
data <- rbind(dataL, dataT)

#ANOVA
use.aov <- aov(Annual.Wood.Demand..kg.cap.year.~Site, data=data)
summary(use.aov)
TukeyHSD(use.aov)

trans.arcsine <- function(x){
  asin(sign(x) * sqrt(abs(x)))
}

data.sub <- data[is.na(data$Prop..Live.Cut)==FALSE,]
perc.aov <- aov(trans.arcsine(Prop..Live.Cut/100)~Site, data=data.sub)
summary(perc.aov)
TukeyHSD(perc.aov)

#summarize the dataset
df.use <- ddply(data, .(Site), summarise, 
                sd = sd(Annual.Wood.Demand..kg.cap.year., na.rm=TRUE),
                mu = mean(Annual.Wood.Demand..kg.cap.year., na.rm=TRUE),
                se = sd(Annual.Wood.Demand..kg.cap.year.)/20
                )


df.perc <- ddply(data, .(Site), summarise, 
                sd = sd(Prop..Live.Cut, na.rm=TRUE),
                mu = mean(Prop..Live.Cut, na.rm=TRUE),
                se = sd(Prop..Live.Cut)/20
)


barplot(df.use$mu)

#set upper and lower limits for error bars
limit.low <- df.use$mu-df.use$se
limit.hi <- df.use$mu+df.use$se

x.text=seq(1,6)
y.text=limit.hi+30
text.lab=c("ab","ab","a","ab","ab","b")

##Plot the means and standard errors for each site
ggplot(df.use, aes(x=Site, y=mu)) + 
  stat_summary(fun.y = "mean", fill = "grey75", color="black", geom = "bar") +
  geom_errorbar(aes(ymin = limit.low, ymax = limit.hi), width=0.15, size=0.7) +
  geom_text(aes(x.text, y.text, label=text.lab)) +
  ylab(expression(paste("Annual Fuelwood Demand (kg ", cap^-1, " ", y^-1, ")" )))+
  scale_x_discrete(labels=site.labels) +
  theme_bw() +
  opts(axis.title.x = theme_text(size=16),
       axis.title.y = theme_text(size=16, angle=90), 
       axis.text.x = theme_text(size=12), 
       axis.text.y = theme_text(size=12), 
       panel.grid.major = theme_blank(),
       panel.grid.minor = theme_blank()
      )
  

##Plot the data as boxplots
use <- ggplot(data, aes(Site_code, Annual.Wood.Demand..kg.cap.year.)) + 
  geom_boxplot(fill="grey80") + 
#   coord_flip() +
  ylab(expression(paste("Annual Fuelwood Demand (kg ", cap^-1, " ", y^-1, ")" ))) +
  ylim(0,3000) +
  xlab("Site") +
  scale_x_discrete(labels=c("Lakamane", "Tiendega"))+
  theme_bw() 
# +
#   opts(axis.title.x = theme_text(size=12),
#        axis.title.y = theme_text(size=12, angle=90), 
#        axis.text.x = theme_text(size=10), 
#        axis.text.y = theme_text(size=10), 
#        panel.grid.major = theme_blank(),
#        panel.grid.minor = theme_blank()
#   )
ggsave(file="/users/atredenn/desktop/boxplot_fuelwooduse.pdf", width = 5, height = 4)


site.maps <- c("400", "600", "1,000", "1,200", "1,200", "1,400")
prop <- ggplot(data, aes(Site, Prop..Live.Cut)) +
  geom_boxplot(fill="grey80") + 
  #   coord_flip() +
  ylab("Percent Live-Cut") +
  xlab("Site") +
  ylim(0,100) +
  scale_x_discrete(labels=c("Lakamane", "Tiendega")) +
  theme_bw() +
  opts(axis.title.x = theme_text(size=12),
       axis.title.y = theme_text(size=12, angle=90), 
       axis.text.x = theme_text(size=10), 
       axis.text.y = theme_text(size=10), 
       panel.grid.major = theme_blank(),
       panel.grid.minor = theme_blank()
  )
ggsave(file="/users/atredenn/desktop/boxplot_fuelwoodprop.pdf", width = 5, height = 4)

library(gridExtra)
grid.arrange(use, prop, ncol=2, nrow=1)
# ggsave(file="/users/atredenn/desktop/boxplot_proportion-livecut.eps")




##Plot boxplots for data at experiment sites
data.exps <- data[data$Site_code != "a",]  
data.exps <- data.exps[data.exps$Site_code != "c",]  

#ANOVA
use.aov <- aov(Annual.Wood.Demand..kg.cap.year.~Site_code, data=data.exps)
summary(use.aov)
TukeyHSD(use.aov)

trans.arcsine <- function(x){
  asin(sign(x) * sqrt(abs(x)))
}

data.sub <- data.exps[is.na(data.exps$Prop..Live.Cut)==FALSE,]
perc.aov <- aov(trans.arcsine(Prop..Live.Cut/100)~Site_code, data=data.sub)
summary(perc.aov)
TukeyHSD(perc.aov)

site.labels = c("Lakamane", "Tiorola", "Tiendega")

library(ggthemes)

ggplot(data.exps, aes(Site_code, Annual.Wood.Demand..kg.cap.year.)) + 
  geom_boxplot(fill="grey80", outlier.shape=1) + 
  #   coord_flip() +
  ylab(expression(paste("Annual Fuelwood Demand (kg ", cap^-1, " ", y^-1, ")" ))) +
  xlab("Site") +
  ylim(0,3000) +
  scale_x_discrete(labels=site.labels) +
  theme_few()

ggplot(data.exps, aes(Site_code, Prop..Live.Cut)) + 
  geom_boxplot(fill="grey80", outlier.shape=1) + 
  #   coord_flip() +
  ylab("Percent Live-Cut") +
  xlab("Site") +
  ylim(0,100) +
  scale_x_discrete(labels=site.labels) +
  theme_few()




##Script to create Figure 5 from Tredennick and Hanan 2014
#Note: this is pretty messy, and just used to create the conceptual basins,
#there is no real analysis here.

rm(list=ls()) #clear workspace

#Figures 5A----------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
###         Forest-Savanna Alt States           ###
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

Distribution.function=function(params){
  # Stochastic vairability around mean in 2-D space
  Meanx = params[1]		# mean x
  SDx = params[2]			# sd x
  Meany = params[3]		# mean y
  SDy = params[4]			# sd y
  newx = rnorm(1, Meanx, SDx)
  newx <- ifelse(newx<0,(-1*newx),newx)
  newx <- ifelse(newx>100,(100-(newx-100)),newx)
  newy = rnorm(1, Meany, SDy)
  newy <- ifelse(newy<0,(-1*newy),newy)
  output <- c(newx,newy)
  return(output) }
### END FUNCTION

end1 = 5000 		# Number of random numbers (point 1)
end2 = 5000			# Number of random numbers (point 2)

## Define the shape and size of our output array
Data_Table_Dual = matrix(0,nrow=end1+end2,ncol = 2) 	# Creates an array of 0's

### POINT 1: Define means and sd
### For a "SINGLE-ATTRACTOR" system make X2 and Y2 = X1 and Y1 
### For a "DUAL-ATTRACTOR# system have different x and y values
Xbar = 65; Xbar2 = 15	# X-Mean (Points 1 & 2, respectively in X dimension) 
Xsd = 10.0; Xsd2 = 20.0	# X-SD (standard deviations in X)
Ybar = 75; Ybar2 = 25	# Y-Mean (Points in Y)
Ysd = 16.0; Ysd2 = 15.0	# Y-SD (SD in Y)

### Generate random numbers for point 1
set.seed(20)
params1 <- c(Xbar,Xsd,Ybar,Ysd) # params to function
for (i in 1:end1) {
  random <- Distribution.function(params1)	# Calls 2-D random draw Function 
  Data_Table_Dual[i,1:2] <- random	# Place x & y into Data_Table_Dual array
}
### Generate random numbers for point 2
params2 <- c(Xbar2,Xsd2,Ybar2,Ysd2) # params to function
#times=seq(1,end1,by=1) 	# Save time steps in a vector
for (i in (1+end1):(end1+end2)) {
  random <- Distribution.function(params2)	# Calls 2-D random draw Function 
  Data_Table_Dual[i,1:2] <- random	# Place x & y into Data_Table_Dual array
}
xmax <- max(Data_Table_Dual[,1]); xmin=min(Data_Table_Dual[,1])
ymax <- max(Data_Table_Dual[,2]); ymin=min(Data_Table_Dual[,2])


d2 <- density(Data_Table_Dual[,1], adjust=2)

##add harvest layer
### POINT 1: Define means and sd
### For a "SINGLE-ATTRACTOR" system make X2 and Y2 = X1 and Y1 
### For a "DUAL-ATTRACTOR# system have different x and y values
Xbar = 62; Xbar2 = 15  # X-Mean (Points 1 & 2, respectively in X dimension) 
Xsd = 22.0; Xsd2 = 20.0	# X-SD (standard deviations in X)
Ybar = 75; Ybar2 = 85	# Y-Mean (Points in Y)
Ysd = 16.0; Ysd2 = 15.0	# Y-SD (SD in Y)

### Generate random numbers for point 1
set.seed(30)
params1 <- c(Xbar,Xsd,Ybar,Ysd) # params to function
for (i in 1:end1) {
  random <- Distribution.function(params1)	# Calls 2-D random draw Function 
  Data_Table_Dual[i,1:2] <- random	# Place x & y into Data_Table_Dual array
}
### Generate random numbers for point 2
params2 <- c(Xbar2,Xsd2,Ybar2,Ysd2) # params to function
#times=seq(1,end1,by=1) 	# Save time steps in a vector
for (i in (1+end1):(end1+end2)) {
  random <- Distribution.function(params2)	# Calls 2-D random draw Function 
  Data_Table_Dual[i,1:2] <- random	# Place x & y into Data_Table_Dual array
}
xmax <- max(Data_Table_Dual[,1]); xmin=min(Data_Table_Dual[,1])
ymax <- max(Data_Table_Dual[,2]); ymin=min(Data_Table_Dual[,2])
d3 <- density(Data_Table_Dual[,1], adjust=2)


##add harvest layer 2
### POINT 1: Define means and sd
### For a "SINGLE-ATTRACTOR" system make X2 and Y2 = X1 and Y1 
### For a "DUAL-ATTRACTOR# system have different x and y values
Xbar = 65; Xbar2 = 15  # X-Mean (Points 1 & 2, respectively in X dimension) 
Xsd = 15; Xsd2 = 20.0  # X-SD (standard deviations in X)
Ybar = 75; Ybar2 = 85	# Y-Mean (Points in Y)
Ysd = 16.0; Ysd2 = 15.0	# Y-SD (SD in Y)

### Generate random numbers for point 1
set.seed(30)
params1 <- c(Xbar,Xsd,Ybar,Ysd) # params to function
for (i in 1:end1) {
  random <- Distribution.function(params1)	# Calls 2-D random draw Function 
  Data_Table_Dual[i,1:2] <- random	# Place x & y into Data_Table_Dual array
}
### Generate random numbers for point 2
params2 <- c(Xbar2,Xsd2,Ybar2,Ysd2) # params to function
#times=seq(1,end1,by=1) 	# Save time steps in a vector
for (i in (1+end1):(end1+end2)) {
  random <- Distribution.function(params2)	# Calls 2-D random draw Function 
  Data_Table_Dual[i,1:2] <- random	# Place x & y into Data_Table_Dual array
}
xmax <- max(Data_Table_Dual[,1]); xmin=min(Data_Table_Dual[,1])
ymax <- max(Data_Table_Dual[,2]); ymin=min(Data_Table_Dual[,2])
d4 <- density(Data_Table_Dual[,1], adjust=2)

##add harvest layer 2
### POINT 1: Define means and sd
### For a "SINGLE-ATTRACTOR" system make X2 and Y2 = X1 and Y1 
### For a "DUAL-ATTRACTOR# system have different x and y values
Xbar = 65; Xbar2 = 15  # X-Mean (Points 1 & 2, respectively in X dimension) 
Xsd = 12; Xsd2 = 20.0  # X-SD (standard deviations in X)
Ybar = 75; Ybar2 = 85  # Y-Mean (Points in Y)
Ysd = 16.0; Ysd2 = 15.0	# Y-SD (SD in Y)

### Generate random numbers for point 1
set.seed(30)
params1 <- c(Xbar,Xsd,Ybar,Ysd) # params to function
for (i in 1:end1) {
  random <- Distribution.function(params1)	# Calls 2-D random draw Function 
  Data_Table_Dual[i,1:2] <- random	# Place x & y into Data_Table_Dual array
}
### Generate random numbers for point 2
params2 <- c(Xbar2,Xsd2,Ybar2,Ysd2) # params to function
#times=seq(1,end1,by=1) 	# Save time steps in a vector
for (i in (1+end1):(end1+end2)) {
  random <- Distribution.function(params2)	# Calls 2-D random draw Function 
  Data_Table_Dual[i,1:2] <- random	# Place x & y into Data_Table_Dual array
}
xmax <- max(Data_Table_Dual[,1]); xmin=min(Data_Table_Dual[,1])
ymax <- max(Data_Table_Dual[,2]); ymin=min(Data_Table_Dual[,2])
d5 <- density(Data_Table_Dual[,1], adjust=2)



par(mgp=c(0,0.3,0), tck=-0.05)
plot(d2$x, (d2$y*-1), type="l", xlim=c(-60,90), ylim=c(-0.02,-0.002), lwd=2, axes=FALSE,
     ylab="", xlab="")

y.fill = numeric(length(d2$y))
y.fill[] = -1
polygon(c(d3$x, rev(d3$x)), c(d3$y*-1,y.fill), col="grey90", border="white")
lines(d5$x, (d5$y*-1), lty="dashed", lwd=1.5, col="grey60")
lines(d4$x, (d4$y*-1), lty="dashed", lwd=1.5, col="grey60")
lines(d3$x, (d3$y*-1), lty="dashed", lwd=3, col="grey60")
polygon(c(d2$x, rev(d2$x)), c(d2$y*-1,y.fill), col="grey80")
lines(d2$x, (d2$y*-1), lwd=3)
#Code for marbles
# points(15.4, -0.0195, cex=3, pch=19, col="black")
# points(14, -0.019, cex=0.8, pch=19, col="white")
points(62, -0.008, cex=2, pch=19, col="black")
points(61, -0.0076, cex=0.5, pch=19, col="grey100")

fill.seq.x <- seq(-20,-80,-1) 
fill.seq.y1 <- numeric(length(fill.seq.x))
fill.seq.y1[] <- -1
fill.seq.y2 <- numeric(length(fill.seq.x))
fill.seq.y2[] <- 0
polygon(c(fill.seq.x, rev(fill.seq.x)), c(fill.seq.y2,fill.seq.y1), 
        col="grey80", border="grey80")
axis(1, at=c(-35, 15, 65), labels=c("G", "T+S", "F"), cex.axis=0.7)
box()




#Figures 5B----------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
###             'Stable' savannas               ###
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

rm(list=ls())   		# start with the workspace clean
#par(mfrow = c(2, 2))  		# Number of rows, cols to plot

Distribution.function=function(params){
  # Stochastic vairability around mean in 2-D space
  Meanx = params[1]		# mean x
  SDx = params[2]			# sd x
  Meany = params[3]		# mean y
  SDy = params[4]			# sd y
  newx = rnorm(1, Meanx, SDx)
#   newx <- ifelse(newx<0,(-1*newx),newx)
  newx <- ifelse(newx>100,(100-(newx-100)),newx)
  newy = rnorm(1, Meany, SDy)
  newy <- ifelse(newy<0,(-1*newy),newy)
  output <- c(newx,newy)
  return(output) }
### END FUNCTION

end1 = 5000 		# Number of random numbers (point 1)
end2 = 5000			# Number of random numbers (point 2)

## Define the shape and size of our output array
Data_Table = matrix(0,nrow=end1+end2,ncol = 2) 	# Creates an array of 0's

### POINT 1: Define means and sd
### For a "SINGLE-ATTRACTOR" system make X2 and Y2 = X1 and Y1 
### For a "DUAL-ATTRACTOR# system have different x and y values
Xbar = 15; Xbar2 = 15	# X-Mean (Points 1 & 2, respectively in X dimension) 
Xsd = 20.0; Xsd2 = 20.0	# X-SD (standard deviations in X)
Ybar = 85; Ybar2 = 85	# Y-Mean (Points in Y)
Ysd = 15.0; Ysd2 = 15.0	# Y-SD (SD in Y)

### Generate random numbers for point 1
set.seed(50)
params1 <- c(Xbar,Xsd,Ybar,Ysd) # params to function
for (i in 1:end1) {
  random <- Distribution.function(params1)	# Calls 2-D random draw Function 
  Data_Table[i,1:2] <- random	# Place x & y into Data_Table array
}
### Generate random numbers for point 2
params2 <- c(Xbar2,Xsd2,Ybar2,Ysd2) # params to function
#times=seq(1,end1,by=1) 	# Save time steps in a vector
for (i in (1+end1):(end1+end2)) {
  random <- Distribution.function(params2)	# Calls 2-D random draw Function 
  Data_Table[i,1:2] <- random	# Place x & y into Data_Table array
}
xmax <- max(Data_Table[,1]); xmin=min(Data_Table[,1])
ymax <- max(Data_Table[,2]); ymin=min(Data_Table[,2])

d2 <- density(Data_Table[,1], adjust=6)


#quick d3 for wood harvest basin
### POINT 1: Define means and sd
### For a "SINGLE-ATTRACTOR" system make X2 and Y2 = X1 and Y1 
### For a "DUAL-ATTRACTOR# system have different x and y values
Xbar = 5; Xbar2 = 5  # X-Mean (Points 1 & 2, respectively in X dimension) 
Xsd = 30.0; Xsd2 = 30.0	# X-SD (standard deviations in X)
Ybar = 85; Ybar2 = 85	# Y-Mean (Points in Y)
Ysd = 15.0; Ysd2 = 15.0	# Y-SD (SD in Y)

### Generate random numbers for point 1
set.seed(50)
params1 <- c(Xbar,Xsd,Ybar,Ysd) # params to function
for (i in 1:end1) {
  random <- Distribution.function(params1)	# Calls 2-D random draw Function 
  Data_Table[i,1:2] <- random	# Place x & y into Data_Table array
}
### Generate random numbers for point 2
params2 <- c(Xbar2,Xsd2,Ybar2,Ysd2) # params to function
#times=seq(1,end1,by=1) 	# Save time steps in a vector
for (i in (1+end1):(end1+end2)) {
  random <- Distribution.function(params2)	# Calls 2-D random draw Function 
  Data_Table[i,1:2] <- random	# Place x & y into Data_Table array
}
xmax <- max(Data_Table[,1]); xmin=min(Data_Table[,1])
ymax <- max(Data_Table[,2]); ymin=min(Data_Table[,2])

d3 <- density(Data_Table[,1], adjust=6)
##

#quick d4 for wood harvest basin
### POINT 1: Define means and sd
### For a "SINGLE-ATTRACTOR" system make X2 and Y2 = X1 and Y1 
### For a "DUAL-ATTRACTOR# system have different x and y values
Xbar = 12; Xbar2 = 12  # X-Mean (Points 1 & 2, respectively in X dimension) 
Xsd = 22; Xsd2 = 22  # X-SD (standard deviations in X)
Ybar = 85; Ybar2 = 85	# Y-Mean (Points in Y)
Ysd = 15.0; Ysd2 = 15.0	# Y-SD (SD in Y)

### Generate random numbers for point 1
set.seed(50)
params1 <- c(Xbar,Xsd,Ybar,Ysd) # params to function
for (i in 1:end1) {
  random <- Distribution.function(params1)	# Calls 2-D random draw Function 
  Data_Table[i,1:2] <- random	# Place x & y into Data_Table array
}
### Generate random numbers for point 2
params2 <- c(Xbar2,Xsd2,Ybar2,Ysd2) # params to function
#times=seq(1,end1,by=1) 	# Save time steps in a vector
for (i in (1+end1):(end1+end2)) {
  random <- Distribution.function(params2)	# Calls 2-D random draw Function 
  Data_Table[i,1:2] <- random	# Place x & y into Data_Table array
}
xmax <- max(Data_Table[,1]); xmin=min(Data_Table[,1])
ymax <- max(Data_Table[,2]); ymin=min(Data_Table[,2])
d4 <- density(Data_Table[,1], adjust=6)


#quick d4 for wood harvest basin
### POINT 1: Define means and sd
### For a "SINGLE-ATTRACTOR" system make X2 and Y2 = X1 and Y1 
### For a "DUAL-ATTRACTOR# system have different x and y values
Xbar = 10; Xbar2 = 10  # X-Mean (Points 1 & 2, respectively in X dimension) 
Xsd = 26.0; Xsd2 = 26.0  # X-SD (standard deviations in X)
Ybar = 85; Ybar2 = 85  # Y-Mean (Points in Y)
Ysd = 15.0; Ysd2 = 15.0	# Y-SD (SD in Y)

### Generate random numbers for point 1
set.seed(50)
params1 <- c(Xbar,Xsd,Ybar,Ysd) # params to function
for (i in 1:end1) {
  random <- Distribution.function(params1)	# Calls 2-D random draw Function 
  Data_Table[i,1:2] <- random	# Place x & y into Data_Table array
}
### Generate random numbers for point 2
params2 <- c(Xbar2,Xsd2,Ybar2,Ysd2) # params to function
#times=seq(1,end1,by=1) 	# Save time steps in a vector
for (i in (1+end1):(end1+end2)) {
  random <- Distribution.function(params2)	# Calls 2-D random draw Function 
  Data_Table[i,1:2] <- random	# Place x & y into Data_Table array
}
xmax <- max(Data_Table[,1]); xmin=min(Data_Table[,1])
ymax <- max(Data_Table[,2]); ymin=min(Data_Table[,2])
d5 <- density(Data_Table[,1], adjust=6)


par(mgp=c(0,0.3,0), tck=-0.05)
plot(d2$x, (d2$y*-1), type="l", xlim=c(-60,90), ylim=c(-0.02, -0.002), lwd=2, axes=FALSE,
     ylab="", xlab="")
y.fill = numeric(length(d2$y))
y.fill[] = -1
polygon(c(d3$x, rev(d3$x)), c(d3$y*-1,y.fill), col="grey90", border="white")
lines(d3$x, (d3$y*-1), lty="dashed", lwd=3, col="grey60")
lines(d4$x, (d4$y*-1), lty="dashed", lwd=1.5, col="grey60")
lines(d5$x, (d5$y*-1), lty="dashed", lwd=1.5, col="grey60")
polygon(c(d2$x, rev(d2$x)), c(d2$y*-1,y.fill), col="grey80", border="grey80")
lines(d2$x, (d2$y*-1), lwd=3)
#Code for marbles
# points(15.4, -0.0195, cex=3, pch=19, col="black")
# points(14, -0.019, cex=0.8, pch=19, col="white")
points(15, -0.0087, cex=2, pch=19, col="black")
points(14, -0.0082, cex=0.5, pch=19, col="grey100")

fill.seq.x <- seq(-45,-80,-1) 
fill.seq.y1 <- numeric(length(fill.seq.x))
fill.seq.y1[] <- -1
fill.seq.y2 <- numeric(length(fill.seq.x))
fill.seq.y2[] <- 0
polygon(c(fill.seq.x, rev(fill.seq.x)), c(fill.seq.y2,fill.seq.y1), 
        col="grey80", border="grey80")
axis(1, at=c(-35, 15, 65), labels=c("G", "T+S", "F"), cex.axis=0.7)
box()



#Figures 5C----------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
###              Arid savannas                  ###
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

rm(list=ls())     	# start with the workspace clean
#par(mfrow = c(2, 2))  		# Number of rows, cols to plot

Distribution.function=function(params){
  # Stochastic vairability around mean in 2-D space
  Meanx = params[1]		# mean x
  SDx = params[2]			# sd x
  Meany = params[3]		# mean y
  SDy = params[4]			# sd y
  newx = rnorm(1, Meanx, SDx)
  #   newx <- ifelse(newx<0,(-1*newx),newx)
  newx <- ifelse(newx>100,(100-(newx-100)),newx)
  newy = rnorm(1, Meany, SDy)
  newy <- ifelse(newy<0,(-1*newy),newy)
  output <- c(newx,newy)
  return(output) }
### END FUNCTION

end1 = 5000 		# Number of random numbers (point 1)
end2 = 5000			# Number of random numbers (point 2)

## Define the shape and size of our output array
Data_Table = matrix(0,nrow=end1+end2,ncol = 2) 	# Creates an array of 0's

### POINT 1: Define means and sd
### For a "SINGLE-ATTRACTOR" system make X2 and Y2 = X1 and Y1 
### For a "DUAL-ATTRACTOR# system have different x and y values
Xbar = 10; Xbar2 = 10	# X-Mean (Points 1 & 2, respectively in X dimension) 
Xsd = 30.0; Xsd2 = 30.0	# X-SD (standard deviations in X)
Ybar = 85; Ybar2 = 85	# Y-Mean (Points in Y)
Ysd = 15.0; Ysd2 = 15.0	# Y-SD (SD in Y)

### Generate random numbers for point 1
set.seed(50)
params1 <- c(Xbar,Xsd,Ybar,Ysd) # params to function
for (i in 1:end1) {
  random <- Distribution.function(params1)	# Calls 2-D random draw Function 
  Data_Table[i,1:2] <- random	# Place x & y into Data_Table array
}
### Generate random numbers for point 2
params2 <- c(Xbar2,Xsd2,Ybar2,Ysd2) # params to function
#times=seq(1,end1,by=1) 	# Save time steps in a vector
for (i in (1+end1):(end1+end2)) {
  random <- Distribution.function(params2)	# Calls 2-D random draw Function 
  Data_Table[i,1:2] <- random	# Place x & y into Data_Table array
}
xmax <- max(Data_Table[,1]); xmin=min(Data_Table[,1])
ymax <- max(Data_Table[,2]); ymin=min(Data_Table[,2])

d2 <- density(Data_Table[,1], adjust=6)


#quick d3 for wood harvest basin
### POINT 1: Define means and sd
### For a "SINGLE-ATTRACTOR" system make X2 and Y2 = X1 and Y1 
### For a "DUAL-ATTRACTOR# system have different x and y values
Xbar = 5; Xbar2 = 5  # X-Mean (Points 1 & 2, respectively in X dimension) 
Xsd = 30.0; Xsd2 = 30.0	# X-SD (standard deviations in X)
Ybar = 85; Ybar2 = 85	# Y-Mean (Points in Y)
Ysd = 15.0; Ysd2 = 15.0	# Y-SD (SD in Y)

### Generate random numbers for point 1
set.seed(50)
params1 <- c(Xbar,Xsd,Ybar,Ysd) # params to function
for (i in 1:end1) {
  random <- Distribution.function(params1)	# Calls 2-D random draw Function 
  Data_Table[i,1:2] <- random	# Place x & y into Data_Table array
}
### Generate random numbers for point 2
params2 <- c(Xbar2,Xsd2,Ybar2,Ysd2) # params to function
#times=seq(1,end1,by=1) 	# Save time steps in a vector
for (i in (1+end1):(end1+end2)) {
  random <- Distribution.function(params2)	# Calls 2-D random draw Function 
  Data_Table[i,1:2] <- random	# Place x & y into Data_Table array
}
xmax <- max(Data_Table[,1]); xmin=min(Data_Table[,1])
ymax <- max(Data_Table[,2]); ymin=min(Data_Table[,2])

d3 <- density(Data_Table[,1], adjust=6)
##

#quick d4 for wood harvest basin
### POINT 1: Define means and sd
### For a "SINGLE-ATTRACTOR" system make X2 and Y2 = X1 and Y1 
### For a "DUAL-ATTRACTOR# system have different x and y values
Xbar = 12; Xbar2 = 12  # X-Mean (Points 1 & 2, respectively in X dimension) 
Xsd = 22; Xsd2 = 22  # X-SD (standard deviations in X)
Ybar = 85; Ybar2 = 85	# Y-Mean (Points in Y)
Ysd = 15.0; Ysd2 = 15.0	# Y-SD (SD in Y)

### Generate random numbers for point 1
set.seed(50)
params1 <- c(Xbar,Xsd,Ybar,Ysd) # params to function
for (i in 1:end1) {
  random <- Distribution.function(params1)	# Calls 2-D random draw Function 
  Data_Table[i,1:2] <- random	# Place x & y into Data_Table array
}
### Generate random numbers for point 2
params2 <- c(Xbar2,Xsd2,Ybar2,Ysd2) # params to function
#times=seq(1,end1,by=1) 	# Save time steps in a vector
for (i in (1+end1):(end1+end2)) {
  random <- Distribution.function(params2)	# Calls 2-D random draw Function 
  Data_Table[i,1:2] <- random	# Place x & y into Data_Table array
}
xmax <- max(Data_Table[,1]); xmin=min(Data_Table[,1])
ymax <- max(Data_Table[,2]); ymin=min(Data_Table[,2])
d4 <- density(Data_Table[,1], adjust=6)


#quick d4 for wood harvest basin
### POINT 1: Define means and sd
### For a "SINGLE-ATTRACTOR" system make X2 and Y2 = X1 and Y1 
### For a "DUAL-ATTRACTOR# system have different x and y values
Xbar = 10; Xbar2 = 10  # X-Mean (Points 1 & 2, respectively in X dimension) 
Xsd = 26.0; Xsd2 = 26.0  # X-SD (standard deviations in X)
Ybar = 85; Ybar2 = 85  # Y-Mean (Points in Y)
Ysd = 15.0; Ysd2 = 15.0	# Y-SD (SD in Y)

### Generate random numbers for point 1
set.seed(50)
params1 <- c(Xbar,Xsd,Ybar,Ysd) # params to function
for (i in 1:end1) {
  random <- Distribution.function(params1)	# Calls 2-D random draw Function 
  Data_Table[i,1:2] <- random	# Place x & y into Data_Table array
}
### Generate random numbers for point 2
params2 <- c(Xbar2,Xsd2,Ybar2,Ysd2) # params to function
#times=seq(1,end1,by=1) 	# Save time steps in a vector
for (i in (1+end1):(end1+end2)) {
  random <- Distribution.function(params2)	# Calls 2-D random draw Function 
  Data_Table[i,1:2] <- random	# Place x & y into Data_Table array
}
xmax <- max(Data_Table[,1]); xmin=min(Data_Table[,1])
ymax <- max(Data_Table[,2]); ymin=min(Data_Table[,2])
d5 <- density(Data_Table[,1], adjust=6)


par(mgp=c(0,0.3,0), tck=-0.05)
plot(d2$x, (d2$y*-1), type="l", xlim=c(-60,90), ylim=c(-0.02, -0.002), lwd=2, axes=FALSE,
     ylab="", xlab="")
y.fill = numeric(length(d2$y))
y.fill[] = -1
polygon(c(d3$x, rev(d3$x)), c(d3$y*-1,y.fill), col="grey90", border="white")
lines(d3$x, (d3$y*-1), lty="dashed", lwd=3, col="grey60")
lines(d4$x, (d4$y*-1), lty="dashed", lwd=1.5, col="grey60")
lines(d5$x, (d5$y*-1), lty="dashed", lwd=1.5, col="grey60")
polygon(c(d2$x, rev(d2$x)), c(d2$y*-1,y.fill), col="grey80", border="grey80")
lines(d2$x, (d2$y*-1), lwd=3)
#Code for marbles
# points(15.4, -0.0195, cex=3, pch=19, col="black")
# points(14, -0.019, cex=0.8, pch=19, col="white")
points(15, -0.0087, cex=2, pch=19, col="black")
points(14, -0.0082, cex=0.5, pch=19, col="grey100")

fill.seq.x <- seq(-45,-80,-1) 
fill.seq.y1 <- numeric(length(fill.seq.x))
fill.seq.y1[] <- -1
fill.seq.y2 <- numeric(length(fill.seq.x))
fill.seq.y2[] <- 0
polygon(c(fill.seq.x, rev(fill.seq.x)), c(fill.seq.y2,fill.seq.y1), 
        col="grey80", border="grey80")
axis(1, at=c(-35, 15, 65), labels=c("G", "T+S", "F"), cex.axis=0.7)
box()






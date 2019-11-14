###################################################
install.packages("cts")
library("cts")
library(tidyverse)
data("V22174")

### plot the oxygen isotope time series
# We have 164 observations in a time period of approximately 800 kiloyears.
plot(V22174,type="l",xlab="Time in kiloyears", ylab="", col = "blue", lwd = 2)
rug(V22174[,1], col="red")

time <- system.time(V22174.car14 <- car(V22174,scale=0.2,order=14))[1]


### fit the modified form of a continuous AR(14) model
V22174.car14 <- car(V22174,scale=0.2,order=14)
summary(V22174.car14)
tab1 <- cbind(V22174.car14$tnit, V22174.car14$ss, V22174.car14$bit[,14])
#?car
#tnit - no. of iterations, $ss - Sum of squares, $bit - matrix of #iterations + estimates
colnames(tab1) <- c("Iteration","Sum of Squares","phi_14")

print(as.data.frame(round(tab1,5)),row.names=FALSE, print.gap=8)

### AIC output based on Belcher et. al (1994)
AIC(V22174.car14)
?AIC

### fit the modified form of a continuous AR(7) model
V22174.car7 <- car(V22174,scale=0.2,order=7)
summary(V22174.car7)

### classical AIC and BIC results for model selection
norder <- 14
V22174.aic <- V22174.bic <- rep(NA, norder)
for (i in 1:norder){
fit <- car(V22174,scale=0.2,order=i)
V22174.aic[i] <- fit$aic
V22174.bic[i] <- fit$bic
}
res <- data.frame(order=1:norder, AIC=V22174.aic, BIC=V22174.bic)
print(res, row.names=FALSE, print.gap=8)

### plot the spectrum for the modified contiuous AR(14) and AR(7) models
par(mfrow=c(2,1))
spectrum(V22174.car14)
spectrum(V22174.car7)

### model diagnostics check
tsdiag(V22174.car14)
?tsdiag

###################################################
data("asth")

### plot of the lung function time series
plot(asth,type="l",xlab="Time in hours", ylab="")
rug(asth[,1], col="red")

### fit the modified form of a continuous AR(4) model
asth.car4 <- car(asth,scale=0.25,order=4, ctrl=car_control(n.ahead=10))
summary(asth.car4)

### fit the modified form of a continuous AR(4) model with measurement error
asth.vri <- car(asth,scale=0.25,order=4, ctrl=car_control(vri=TRUE))
summary(asth.vri)

### plot of spectrum for the continuous AR(4) model without/with measurement error
par(mfrow=c(2,1))
spectrum(asth.car4)
spectrum(asth.vri)

### determine the zeros of equation (3)
factab(asth.car4)

### decompose the original time series into three corresponding components 
### via the Kalman smoother 
asth.kalsmo <- kalsmo(asth.car4)
par(mfrow=c(3,1))
kalsmoComp(asth.kalsmo,comp=1, xlab="Time in hours", col = "blue")
kalsmoComp(asth.kalsmo,comp=c(2,3), xlab="Time in hours", col = "blue")
kalsmoComp(asth.kalsmo,comp=4,xlab="Time in hours", col = "blue")
edit(kalsmoComp)
### predict the last 10 steps past the end of time series
predict(asth.car4, xlab="Time in hours")


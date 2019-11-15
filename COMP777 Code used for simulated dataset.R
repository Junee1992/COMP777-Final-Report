##The code used to simulate the data
ornstein_uhlenbeck <- function(T, n, mu, lambda, sigma, X0){
  dw <- rnorm(n, 0, sqrt(T/n))
  dt <- T/n
  x <- c(X0)
  for (i in 2:(n+1)){
    x[i] <- x[i-1] + lambda * (mu-x[i-1])*dt + sigma * sqrt(x[i-1]) * dw[i-1]
  }
  return(x);
}

a <- ornstein_uhlenbeck(1,899,25,3,0.5,25)
plot(a, type = "l")
data <- a[-c(seq(1:100))]
t <- c(seq(1:800))
usedata <- cbind(t, data)
data1 <- usedata[-c(sample(1:800, 600, replace=F)),]

##This data was saved as data.RData.

## First load the data
load("data.RData")
colnames(data) <- c("Time", "Measure")

#Plot of the simulated data
plot(data, type = "l", main = "Simulated Path of Xt Process")
rug(data[,1], col="red")

#To determine the model with best fit, using AIC and BIC.
norder <- 13
data.aic <- data.bic <- rep(NA, norder)
for(i in 1:norder){
  fit <- car(data, scale = 0.25, order = i)
  data.aic[i] <- fit$aic
  data.bic[i] <- fit$bic
}
res <- data.frame(order = 1:norder, AIC = data.aic, BIC = data.bic)
print(res)

#Fit the model CAR(4)
data.car4 <- car(data, scale = 0.25, order = 4)
summary(data.car4)

#Model Diagnostic
tsdiag(data.car4)

#Kalman smoothing
data.kalsmo <- kalsmo(data.car4)
par(mfrow = c(3,1))
kalsmoComp(data.kalsmo, comp = 1, xlab = "Time")
kalsmoComp(data.kalsmo, comp = c(2,3), xlab = "Time")
kalsmoComp(data.kalsmo, comp = 4, xlab = "Time")

#Forecast
predict(data.car4, xlab = "Time")

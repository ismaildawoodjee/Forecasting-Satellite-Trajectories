# find frequencies of all columns from x to e_Vz
freq <- unlist(map(df, findfrequency))

train1 <- read.csv('train1.csv')
test1 <- read.csv('test1.csv')

install.packages("forecast")

library(forecast)
library(fpp2)

# convert to time series
findfrequency(train1[,'y'])
train1[,'y'] <- ts(train1[,'y'], frequency = 24)

# quick plot
autoplot(train1[,"y"]) +
  ggtitle("Position y (km)") +
  xlab("Epoch") +
  ylab("Position (km")

# moving average plot m-MA
autoplot(train1[,"x"], series="Data") +
  autolayer(ma(train1[,"x"], 12), series = "12-MA") +
  ggtitle("Position x (km)") +
  xlab("Epoch") +
  ylab("Position (km")

# classical decomposition
train1[,"x"] %>% decompose(type = "multiplicative") %>% 
  autoplot() +
  xlab("Epoch") +
  ylab("Position (km")

# STL decomposition
# t/s.window is trend/seasonal window
# and make it robust
train1[,"x"] %>% stl(s.window = "periodic", robust = TRUE) %>%
  autoplot()

# mstl() decomposition?

# naive forecast
fit <- stl(train1[,"x"], s.window = "periodic", robust = TRUE)
fit %>% seasadj() %>% naive() %>%
  autoplot() +
  ggtitle("Position x (km)") +
  xlab("Epoch") +
  ylab("Position (km)")

# naively adjusted seasonality forecast
fit %>% forecast(method = "naive") %>%
  autoplot() +
  xlab("Epoch") +
  ylab("Position (km)")

# quick forecast using stlf()
fcast <- stlf(train1[,"x"], robust = TRUE)
fcast %>% forecast() %>%
  autoplot() +
  xlab("Epoch") +
  ylab("Position (km)")

################################################ START HERE ###################################################

# holt-winters method
# cannot fit multiplicative seasonality to negative data
fit1 <- hw(traints, seasonal = "additive", h=1400, damped=TRUE)
#fit2 <- hw(train1[,"x"], seasonal = "multiplicative")

autoplot(traints) +
  autolayer(fit1, series="HW", PI=FALSE) +
  xlab("Epoch") +
  ylab("Position (km)")
  
# HW method with damped trend and additive seasonality
# forecasting for the last 48 hours
fit2 <- hw(subset(traints, end=length(traints)-48), 
           damped=TRUE, seasonal = "additive",
           h = 1400)
autoplot(traints) + 
  autolayer(fit2, series = "HW a", PI=FALSE) +
  xlab("Epoch") +
  ylab("Position (km)")

# offset x by some amount to make all values > 0
train1 <- read.csv("train1.csv")
y_off <- as.integer(max(train1$y) - min(train1$y))
train1$y_off <- train1$y + y_off
train1 <- ts(train1, frequency = 24)

# damped additive trend, multiplicative seasonality
fit3 <- hw(subset(train1[,"y_off"], end=length(train1[,"y_off"])), 
           damped=TRUE, seasonal = "multiplicative",
           h = 1400)
autoplot(train1[,"y_off"]) + 
  autolayer(fit3, series = "HW m", PI=FALSE) +
  xlab("Epoch") +
  ylab("Position (km)")

# need to calculate RMSE/sMAPE to compare the two approaches

# use ets() to select the appropriate model
# can also specify desired parameters instead of automatically letting ets choose them

# for x with 0 and negative values
# additive model, no trend, additive seasonality
fit <- ets(train1[,"y"])
summary(fit)
autoplot(fit)

# for x_off with strictly positive values
# multip model, no trend, multip seasonality
fit2 <- ets(train1[,"y_off"])
summary(fit2)
autoplot(fit2)

# forecasting using ETS models
fit %>% forecast(h=48, level=c(95)) %>%
  autoplot() + 
  xlab("Epoch") +
  ylab("Position (km)")

fit2 %>% forecast(h=48) %>%
  autoplot() + 
  xlab("Epoch") +
  ylab("Position (km)")

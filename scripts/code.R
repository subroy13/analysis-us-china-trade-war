#################################################
# LOADING THE LIBRARIES
#################################################

library(forecast)
#library(vars)
library(readr)
library(tseries)
#library(fpp)
require(graphics)


#################################################
# US Data  -- all from NASDAQ
###############################################

# Reading the Data

GOOGL <- read_csv('./datesets/GOOGL.csv')
AMZN <- read_csv('./datesets/AMZN.csv')
AAPL <- read_csv('./datesets/AAPL.csv')
FB <- read_csv('./datesets/FB.csv')

tsGoogl <- ts(log(GOOGL$`Adj Close`))
tsAmzn <- ts(log(AMZN$`Adj Close`))
tsAapl <- ts(log(AAPL$`Adj Close`))
tsFb <- ts(log(FB$`Adj Close`))

par(mfrow = c(1,2), mar = rep(2.5,4))
plot(tsGoogl, ylab = "", main = "Observed series for GOOGL")
plot(tsAmzn, ylab = "", main = "Observed series for AMZN")


par(mfrow = c(1,2), mar = c(4.5,4.5,1,1), cex.lab = 1, cex.axis = 1, las = 1)
plot(exp(tsGoogl), lwd = 2, xlab = "Day",ylab = "Adj. Close Price of Google")
lines(exp(fitted(model1)), col = "green", lwd = 2)
legend("topleft", legend = c("Observed","Fitted"), col = c("black","green"), lwd = 2, cex = 0.8)

plot(exp(tsFb), lwd = 2, xlab = "Day",ylab = "Adj. Close Price of Facebook")
lines(exp(fitted(model2)), col = "green", lwd = 2)
legend("bottomleft", legend = c("Observed","Fitted"), col = c("black","green"), lwd = 2, cex = 0.9)



# Checking the Correlation between series
cor(cbind(tsGoogl, tsAmzn, tsAapl, tsFb))


# Checking whether stationary, p < 0.05 indicates stationary
adf.test(tsGoogl)
adf.test(tsAmzn)
adf.test(tsAapl)
adf.test(tsFb)

# taking first difference and checking stationarity
diffts1 <- diff(tsGoogl)
diffts2 <- diff(tsAmzn)
diffts3 <- diff(tsAapl)
diffts4 <- diff(tsFb)

adf.test(diffts1)
adf.test(diffts2)
adf.test(diffts3)
adf.test(diffts4)

data <- cbind(diffts1,diffts2,diffts3,diffts4)

# Fitting var model
var <- VAR(data,type = "const",lag.max = 10,ic="AIC")
#summary(var)  

# Prediction from this
x=predict(var,n.ahead = 90)
y = x$fcst
pred1=numeric(91)
pred2=numeric(91)
pred3=numeric(91)
pred4=numeric(91)

pred1[1]=tsGoogl[252]
pred2[1]=tsAmzn[252]
pred3[1]=tsAapl[252]
pred4[1]=tsFb[252]

for(i in 1:90){
    pred1[(i+1)]=pred1[i]+y[[1]][i,1]
    pred2[(i+1)]=pred2[i]+y[[2]][i,1]
    pred3[(i+1)]=pred3[i]+y[[3]][i,1]
    pred4[(i+1)]=pred4[i]+y[[4]][i,1]
}

pred1=pred1[-1]
pred2=pred2[-1]
pred3=pred3[-1]
pred4=pred4[-1]

# Using Log transformation
tsGoogl <- log(tsGoogl)
tsAmzn <- log(tsAmzn)
tsAapl <- log(tsAapl)
tsFb <- log(tsFb)

optimal_ARIMA(tsGoogl)
optimal_ARIMA(tsAmzn)
optimal_ARIMA(tsAapl)
optimal_ARIMA(tsFb)

#fitting ARIMA model  <- ------------- CHECK HERE
model <- Arima(tsGoogl, order = c(2,1,2))

plot(exp(tsGoogl))
lines(exp(fitted(model)), col = "blue", lty = 2)

preds <- forecast(model, h = 90)
preds$mean <- exp(preds$mean)
preds$lower <- exp(preds$lower)
preds$upper <- exp(preds$upper)

plot(exp(tsGoogl), xlim = c(1,350), ylim = c(700, 1500))
lines(exp(fitted(model1)), lty = 2, col = "blue")
polygon(c(253:342, 342:253), c(preds$lower[,2], rev(preds$upper[,2])), col = rgb(0,0,0,0.2), border = NA)
polygon(c(253:342, 342:253), c(preds$lower[,1], rev(preds$upper[,1])), col = rgb(0,0,0,0.3), border = NA)
points(253:342, preds$mean, lty = 2, type = "l", col = "blue")





# for Amzn
model <- Arima(tsAmzn, order = c(3,1,2))

plot(exp(tsAmzn))
lines(exp(fitted(model)), col = "blue", lty = 2)

preds <- forecast(model, h = 90)
preds$mean <- exp(preds$mean)
preds$lower <- exp(preds$lower)
preds$upper <- exp(preds$upper)

plot(exp(tsAmzn), xlim = c(1,350), ylim = c(1000, 2500))
lines(exp(fitted(model)), lty = 2, col = "blue")
polygon(c(253:342, 342:253), c(preds$lower[,2], rev(preds$upper[,2])), col = rgb(0,0,0,0.2), border = NA)
polygon(c(253:342, 342:253), c(preds$lower[,1], rev(preds$upper[,1])), col = rgb(0,0,0,0.3), border = NA)
points(253:342, preds$mean, lty = 2, type = "l", col = "blue")











###############################################################
# Ploting the predictions
# plot GOOGL
plot(tsGoogl, xlim = c(1,342), ylim = c(650,1400), main = "Prediction for stock GOOGL", xlab = "", ylab = "Price", las = 1)
polygon(c(253:342, 342:253), c(pred1 - sqrt(cumsum(y[[1]][,2]^2)), rev(pred1 + sqrt(cumsum(y[[1]][,3]^2)))), col = "grey", border = NA)
points(253:342, pred1, type = "l", col = "blue", lty = 2)

#plot AMZN
plot(tsAmzn, xlim = c(1,342), ylim = c(1000,2450), main = "Prediction for stock AMZN", xlab = "", ylab = "Price", las = 1)
polygon(c(253:342, 342:253), c(pred2 - sqrt(cumsum(y[[2]][,2]^2)), rev(pred2 + sqrt(cumsum(y[[2]][,3]^2)))), col = "grey", border = NA)
points(253:342, pred2, type = "l", col = "blue", lty = 2)

#plot AAPL
plot(tsAapl, xlim = c(1,342), ylim = c(70,240), main = "Prediction for stock AAPL", xlab = "", ylab = "Price")
polygon(c(253:342, 342:253), c(pred3 - sqrt(cumsum(y[[3]][,2]^2)), rev(pred3 + sqrt(cumsum(y[[3]][,3]^2)))), col = "grey", border = NA)
points(253:342, pred3, type = "l", col = "blue", lty = 2)

#plot FB
plot(tsFb, xlim = c(1,342), ylim = c(50,230), main = "Prediction for stock FB", xlab = "", ylab = "Price")
polygon(c(253:342, 342:253), c(pred4 - sqrt(cumsum(y[[4]][,2]^2)), rev(pred4 + sqrt(cumsum(y[[4]][,3]^2)))), col = "grey", border = NA)
points(253:342, pred4, type = "l", col = "blue", lty = 2)


#get returns
returns <- matrix(NA, nrow = 4, ncol = 3)
colnames(returns) <- c("Lower","Mean","Upper")
rownames(returns) <- c("GOOGL","AMZN","AAPL","FB")

returns[1,]<- 100*(c((pred1 - sqrt(cumsum(y[[1]][,2]^2)))[90], pred1[90], (pred1 + sqrt(cumsum(y[[1]][,3]^2)))[90]) - tsGoogl[252])/tsGoogl[252]
returns[2,]<- 100*(c((pred2 - sqrt(cumsum(y[[2]][,2]^2)))[90], pred2[90], (pred2 + sqrt(cumsum(y[[2]][,3]^2)))[90]) - tsAmzn[252])/tsAmzn[252]
returns[3,]<- 100*(c((pred3 - sqrt(cumsum(y[[3]][,2]^2)))[90], pred3[90], (pred3 + sqrt(cumsum(y[[3]][,3]^2)))[90]) - tsAapl[252])/tsAapl[252]
returns[4,]<- 100*(c((pred4 - sqrt(cumsum(y[[4]][,2]^2)))[90], pred4[90], (pred4 + sqrt(cumsum(y[[4]][,3]^2)))[90]) - tsFb[252])/tsFb[252]
print(returns)


#################################################
# CHINESE STOCKS

BABA <- read_csv('./datesets/BABA.csv')
BIDU <- read_csv('./datesets/BIDU.csv')
TENCENT <- read_csv('./datesets/TENCENT.csv')
XIAOMI <- read_csv('./datesets/XIAOMI.csv')

tsBaba <- ts(log(BABA$`Adj Close`))
tsBidu <- ts(log(BIDU$`Adj Close`))
tsTencent <- ts(log(TENCENT$`Adj Close`))
tsXiaomi <- ts(log(XIAOMI$`Adj Close`))

par(mfrow = c(1,2), mar = rep(2.5,4))
plot(tsBidu, ylab = "", main = "Observed series for Baidu")
plot(tsTencent, ylab = "", main = "Observed series for Tencent")


par(mfrow = c(1,2), mar = c(4.5,4.5,1,1), cex.lab = 1, cex.axis = 1, las = 1)
plot(exp(tsXiaomi), lwd = 2, xlab = "Day",ylab = "Adj. Close Price of Xiaomi")
lines(exp(fitted(model1)), col = "green", lwd = 2)
legend("topright", legend = c("Observed","Fitted"), col = c("black","green"), lwd = 2, cex = 0.9)

plot(exp(tsTencent), lwd = 2, xlab = "Day",ylab = "Adj. Close Price of Tencent")
lines(exp(fitted(model2)), col = "green", lwd = 2)
legend("topright", legend = c("Observed","Fitted"), col = c("black","green"), lwd = 2, cex = 0.85)





#checking stationarity
adf.test(tsBaba)
adf.test(tsBidu)
adf.test(tsTencent)
adf.test(tsXiaomi)

#checking differenced stationarity
adf.test(diff(tsBaba))
adf.test(diff(tsBidu))
adf.test(diff(tsTencent))
adf.test(diff(tsXiaomi))


#model fitting
optimal_ARIMA <- function(ts, fun = AIC){
    v=c(1,1,1)
    model<-arima(ts,order=v, method = "ML")
    min = fun(model)
    for(p in 1:5){
        for(q in 1:5){
        model=arima(ts, order = c(p,d=1,q),method = "ML")
        if((fun(model))<min) {
            min=fun(model)
            v=c(p,1,q)
            }
        }
    }
    return(paste("order is",v))
}

#for alibaba
optimal_ARIMA(tsBaba)
optimal_ARIMA(tsBaba, fun = BIC)

acf(diff(tsBaba))
pacf(diff(tsBaba))

#for Baidu
optimal_ARIMA(tsBidu)
optimal_ARIMA(tsBidu, fun = BIC)

acf(diff(tsBidu))
pacf(diff(tsBidu))

#for tencent
optimal_ARIMA(tsTencent)
optimal_ARIMA(tsTencent, fun = BIC)

acf(diff(tsTencent))
pacf(diff(tsTencent))

#for xiaomi
optimal_ARIMA(tsXiaomi)
optimal_ARIMA(tsXiaomi, fun = BIC)

acf(diff(tsXiaomi))
pacf(diff(tsXiaomi))


#predictions 
model = Arima(tsBaba, order = c(2,1,2))
pred1 <- forecast(model, h=90)
plot(pred1, main = "Predictions for stock BABA", las = 1, ylab="Price")

model = Arima(tsBidu, order = c(2,1,2))
pred2 <- forecast(model, h=90)
plot(pred2, main = "Predictions for stock BIDU", las = 1, ylab="Price")

model = Arima(tsTencent, order = c(5,1,5))
pred3a <- forecast(model, h=90)
plot(pred3a, main = "Predictions for stock Tencent", las = 1, ylab="Price")

model = Arima(tsTencent, order = c(1,1,1))
pred3b <- forecast(model, h=90)
plot(pred3b, main = "Predictions for stock Tencent", las = 1, ylab="Price")


model = Arima(tsXiaomi, order = c(3,1,1))
pred4 <- forecast(model, h=90)
plot(pred4, main = "Predictions for stock Xiaomi", las = 1, ylab="Price")


# getting the turnover ratios

returns <- matrix(NA, nrow = 5, ncol = 5)
colnames(returns) <- c("Lower-95","Lower-80","Mean","Upper-80","Upper-95")
rownames(returns) <- c("ALIBABA","BAIDU","TENCENT(Model 5,1,5)","TENCENT (Model 1,1,1)","XIAOMI")

returns[1,] <- 100*(c(pred1$lower[90,c(2,1)],pred1$x[90],pred1$upper[90,])-tsBaba[length(tsBaba)])/tsBaba[length(tsBaba)]
returns[2,] <- 100*(c(pred2$lower[90,c(2,1)],pred2$x[90],pred2$upper[90,])-tsBidu[length(tsBidu)])/tsBidu[length(tsBidu)]
returns[3,] <- 100*(c(pred3a$lower[90,c(2,1)],pred3a$x[90],pred3a$upper[90,])-tsTencent[length(tsTencent)])/tsTencent[length(tsTencent)]
returns[4,] <- 100*(c(pred3b$lower[90,c(2,1)],pred3b$x[90],pred3b$upper[90,])-tsTencent[length(tsTencent)])/tsTencent[length(tsTencent)]
returns[5,] <- 100*(c(pred4$lower[90,c(2,1)],pred4$x[90],pred4$upper[90,])-tsXiaomi[length(tsXiaomi)])/tsXiaomi[length(tsXiaomi)]

print(returns)



######################################
data <- read_csv('./datesets/BIDU.csv')
times <- ts(log(data$`Adj Close`))

model <- Arima(times, order = c(2,1,2))
preds <- forecast(model, h = 90)

print(exp(times[length(times)]))
print(exp(preds$mean[90]))

returns <- 100*(exp(preds$mean[90]) - exp(times[length(times)]))/exp(times[length(times)])
print(returns)



par(mfrow = c(1,2), mar = c(2.5,4,2.5,2.5))
plot(diffts1, main = "Differenced Series for Apple", xlab = "", 
     ylab = "", las = 1)
abline(h = 0, col = "red", lty = 2, lwd = 2)
plot(diffts2, main = "Differenced Series for Baidu", xlab = "", 
     ylab = "", las = 1)
abline(h = 0, col = "red", lty = 2, lwd = 2)








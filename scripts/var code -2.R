library(forecast)
library(vars)
library(readr)
library(tseries)
library(fpp)
require(graphics)


#data input
GOOGL <- read_csv("./datasets time series returns/GOOG.csv") #change acc to the location of file in your PC
AMZN <- read_csv("./datasets time series returns/AMZN.csv")
ts1 <- GOOGL$`Adj Close`
ts2 <-AMZN$`Adj Close`
ts1 <-ts1[-1]
ts2 <-ts2[-1] #we remove the first entry NULL
ts1 <- as.numeric(ts1)
ts2 <- as.numeric(ts2)
ts1 <- ts(ts1,frequency=1)#putting ts1 , ts2 to a monthly ts object
ts2 <- ts(ts2,frequency=1)



#checking stationary behaviour 
adf.test(ts1)
adf.test(ts2) #look at the p-values . p-values larger than 0.05 shows that the original series are not stationary 
diffts1 <- diff(ts1)
diffts2 <- diff(ts2) #differencing for making ts1 ,ts2 stationary 
adf.test(diffts1)
adf.test(diffts2) # small p-values <0.05 indicate stationary behaviour
T <- cbind(diffts1,diffts2)


#VAR model creation
var <- VAR(T,type = "const",lag.max = 10,ic="AIC")
#this is our VAR model. We allow lag length <= 10 . 
#among the models with lag length <= 10 , we'll choose the best one by 
#choosing the one with minimum AIC

summary(var) #gives the analysis of the regression . look atthe p-values 


##############################################

GOOGL <- read_csv("./datasets time series returns/GOOGL.csv") #change acc to the location of file in your PC
AMZN <- read_csv("./datasets time series returns/AMZN.csv")
AAPL <- read_csv("./datasets time series returns/AAPL.csv")
FB <- read_csv("./datasets time series returns/FB.csv")

ts1 <- GOOGL$`Adj Close`
ts2 <-AMZN$`Adj Close`
ts3 <- AAPL$`Adj Close`
ts4=FB$`Adj Close`


ts1 <- ts(ts1,frequency=1)#putting ts1 , ts2 to a monthly ts object
ts2 <- ts(ts2,frequency=1)
ts3 <- ts(ts3,frequency=1)
ts4 <- ts(ts4,frequency=1)


plot(ts1,ylim=c(0,2050))
points(ts2,col="red",type="l")
points(ts3,col="blue",type="l")
points(ts4,col="green",type="l")



df=data.frame("GOOGL"=ts1,"AMZN"=ts2,"AAPL"=ts3,"FB"=ts4)

cor(df)

diffts1 <- diff(ts1)
diffts2 <- diff(ts2)
diffts3=diff(ts3)
diffts4=diff(ts4)

#T <- cbind(diffts1,diffts2,diffts3,diffts4)
T <- cbind(diffts1,diffts2,diffts3)


var <- VAR(T,type = "const",lag.max = 10,ic="AIC")

x=predict(var,n.ahead = 365)
par(mar=c(1,1,1,1))
plot(x)
y=x$fcst

pred1=numeric(366)
pred2=numeric(366)
pred3=numeric(366)
#pred4=numeric(366)

pred1[1]=ts1[1040]pred2[1]=ts2[1040]
pred3[1]=ts3[1040]


#pred1[1]=ts1[1258]
#pred2[1]=ts2[1258]
#pred3[1]=ts3[1258]
#pred4[1]=ts4[1258]


for(i in 1:365){
  pred1[(i+1)]=pred1[i]+y[[1]][i,1]
  pred2[(i+1)]=pred2[i]+y[[2]][i,1]
  pred3[(i+1)]=pred3[i]+y[[3]][i,1]
  pred4[(i+1)]=pred4[i]+y[[4]][i,1]
}

pred1=pred1[-1]
pred2=pred2[-1]
pred3=pred3[-1]
pred4=pred4[-1]

plot(ts1,xlim=c(1,1623))
points(1259:1623,pred1,type="l",col="blue")

(pred1[365]-ts1[1258])/ts1[1258]*100   #return 12.78
(pred2[365]-ts2[1258])/ts2[1258]*100   #return 19.23
(pred3[365]-ts3[1258])/ts3[1258]*100   #around 15
(pred4[365]-ts4[1258])/ts4[1258]*100   #around 15


#####################################

ALIBABA <- read_csv("./datasets time series returns/BABA.csv") #change acc to the location of file in your PC
BAIDU <- read_csv("./datasets time series returns/BIDU.csv")
TENCENT <- read_csv("./datasets time series returns/TENCENT.csv")

ALIBABA=ALIBABA[,c(1,6)]
BAIDU=BAIDU[,c(1,6)]
TENCENT=TENCENT[,c(1,6)]

data=merge(ALIBABA,BAIDU,by="Date")
data=merge(data,TENCENT,by="Date")

A=as.matrix(data[,c(2,3,4)])
colnames(A)=c("ALIBABA","BAIDU","TENCENT")

cor(A)


#testSTATIONARITY
adf.test(ts(A[,1]))
adf.test(ts(A[,2]))
adf.test(ts(A[,3]))

adf.test(diff(ts(A[,1])))
adf.test(diff(ts(A[,2])))
adf.test(diff(ts(A[,3])))#stationary


ts1=ts(A[,1])
ts2=ts(A[,2])
ts3=ts(A[,3])

(pred1[365]-ts1[1040])/ts1[1040]*100   #return 14.32205
(pred2[365]-ts2[1040])/ts2[1040]*100   #return -9.585805
(pred3[365]-ts3[1040])/ts3[1040]*100   #return  22.72831

ts1=ts(ALIBABA$`Adj Close`)
ts2=ts(BAIDU$`Adj Close`)
ts3=ts(TENCENT$`Adj Close`)

#minimiseAIC
#Given ts , find optimal ARIMA

optimal_ARIMA <- function(ts){
min=0
v=c(0,0,0)
model<-arima(ts,order=v)
for(p in 1:5){for(q in 1:5){
  
  model=arima(ts, order = c(p,d=1,q))
  if(AIC(model)<min) {
    
    min=AIC(model)
    v=c(p,1,q)
  }
  
}
}

return(paste("order is",v))
}

















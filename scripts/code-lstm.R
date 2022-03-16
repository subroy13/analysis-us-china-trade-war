#################################################
# LOADING THE LIBRARIES
#################################################

#library(forecast)
#library(vars)
library(readr)
library(tseries)
library(tensorflow)
library(keras)


###############################################

data <- read_csv('./datesets/GOOGL.csv')  #change accordingly

series <- ts(data$`Adj Close`)
#adf.test(series)  #checking stationarity

#diffed <- diff(series)  #take first difference to make stationary
#adf.test(diffed)

# perform scaling and location transform to fit into range [-1,1]
scale_transform <- function(x, feature_range = c(0,1)) {
    fr_min = feature_range[1]
    fr_max = feature_range[2]
    std = ((x - min(x) ) / (max(x) - min(x)))
    scaled = std *(fr_max -fr_min) + fr_min
    return(list("Scaled" = scaled, "Scalars" = c(min(x), max(x))))
}

x <- scale_transform(series, c(-1,1))
series <- x$Scaled
scalar <- x$Scalars


invert_scale_transform <- function(scaled, scalars, feature_range = c(0,1)) {
    min = scalars[1]
    max = scalars[2]
    mins = feature_range[1]
    maxs = feature_range[2]
    X = (scaled- mins)/(maxs - mins)
    rawValues = X *(max - min) + min
    return(rawValues)
}



generate_df <- function(series) {
    l <- length(series)
    lag3 <- series[1:(l-3)]
    lag2 <- series[2:(l-2)]
    lag1 <- series[3:(l-1)]
    df <- as.data.frame(cbind(lag3,lag2,lag1, series[4:l]))
    colnames(df) <- c("x-3","x-2","x-1","x")
    return(df)
}

data <- generate_df(series)
x_train <- data[,1:3]
y_train <- data[,4]

x_train <- as.matrix(x_train)
dim(x_train) <- c(nrow(x_train), 3,1)


batch_size <- 1
timesteps <- 3
features <- 1

## Building Model

model <- keras_model_sequential() 
model %>%
    layer_lstm(units            = 10, 
               input_shape      = c(timesteps, 1), 
               batch_size       = batch_size,
               return_sequences = FALSE, 
               stateful         = TRUE) %>% 
    layer_dense(units = 1)

model %>% 
    compile(loss = 'mse', optimizer = 'adam')
summary(model)

#model %>% fit(x_train, y_train, epochs=50, batch_size=batch_size, verbose=1, shuffle=FALSE)

#training model
Epochs = 50
for(i in 1:Epochs ){
    model %>% fit(x_train, y_train, epochs=1, batch_size=batch_size, verbose=1, shuffle=FALSE)
    model %>% reset_states()
    message(paste('Epoch -',i,'has been completed.......'))
}

data$fitted <- numeric(nrow(data))
for (i in 1:nrow(data)) {
    X <- as.matrix(data[i,1:3])
    dim(X) = c(1,3,1)
    data$fitted[i] = model %>% predict(X, batch_size = batch_size)
    #model %>% reset_states()
}

#plot(ts(invert_scale_transform(data$x, scalar, c(-1,1))))
#lines(ts(invert_scale_transform(data$fitted, scalar, c(-1,1))), col = "blue")

preds = numeric(93)
preds[1:3] <- tail(series,3)


for(i in 1:90){
    X <- preds[i:(i+2)]
    dim(X) = c(1,3,1)
    preds[i+3] = model %>% predict(X, batch_size = batch_size)
}

preds <- invert_scale_transform(preds[4:93], scalar, c(-1,1))
#preds <- tail(series,1) + cumsum(preds)

plot(1:252, invert_scale_transform(series, scalar, c(-1,1)), type = "l", xlim = c(1,343), ylab = "",
     main = "LSTM Fitting for GOOGL")
lines(4:252,ts(invert_scale_transform(data$fitted, scalar, c(-1,1))), col = "blue")
lines(253:342, preds, col = "blue")















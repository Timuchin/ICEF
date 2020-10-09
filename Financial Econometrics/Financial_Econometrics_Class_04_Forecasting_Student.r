# Libraries
# For those who has a problem like: package ‘package_name’ is not available (for R version x.x.x)
# install.packages('package_name', dependencies=TRUE, repos='http://cran.rstudio.com/')

library(quantmod)
library(multDM)
library(forecast) #I highly recommend to visit the link above. It explains basics of forecasting and time-series modeling in R

# For those who have operational system in Russian but wants it in English
Sys.setlocale("LC_TIME", "C")
format(Sys.Date(), format = "%Y-%b-%d")

sp <- getSymbols("^GSPC", scr = "yahoo", auto.assign = FALSE)

sp.price <- sp$GSPC.Adjusted
sp.ret <- diff(log(sp.price))[-1] * 100
names(sp.ret) <- c('S&P500 log returns')

N <- length(sp.ret) # let's have a variable with the size\length of our data
N_OOS <- round(x = 0.3 * N, digits = 0) # Usually, 25%-30% of data are used for the prediction period
N_sample <- N - N_OOS

y_train <- vector(mode = 'logical', length = N_sample)
y_pred_mean <- vector(mode = 'logical', length = N_OOS)
y_pred_ar <- vector(mode = 'logical', length = N_OOS)
y_true <- sp.ret[(N_sample + 1):N]

for (i in 1:N_OOS){
    y_train <- sp.ret[i:(N_sample + i - 1)] #It is a good practice to store your in-sample data
    y_pred_mean[i] <- mean(y_train) # Because then you can just call the function on it
    ar <- lm(y_train ~ lag(y_train))
    y_pred_ar[i] <- ar$coefficients[1] + ar$coefficients[2] * y_train[N_sample]
}

y_pred_mean <- as.xts(x = y_pred_mean, order.by = index(y_true))
y_pred_ar <- as.xts(x = y_pred_ar, order.by = index(y_true))

mse_mean <- mean((y_true - y_pred_mean)^2)
mse_ar <- mean((y_true - y_pred_ar)^2)

par(xpd = T)
plot(x = cbind(y_true, y_pred_mean, y_pred_ar), 
     main = 'Returns and predictions', 
     y = index(y_true), 
     col = c('black', 'red', 'blue'),
     lwd = c(1, 2, 2))
legend('bottomleft', 
       legend = c('Returns', 'Mean model predictions', 'AR(1) model predictions'), 
       col = c('black', 'red', 'blue'),
       lty = c(1, 1, 1),
       inset = 0.05)

mse_mean; mse_ar

# Let's go
L.mean_model = (y_true - y_pred_mean)^2
L.ar = (y_true - y_pred_ar)^2

d = L.mean_model - L.ar

d.mean = mean(d)

d.var <- var(d)

dm <- d.mean/sqrt(d.var/length(d))

dm

2 * pnorm(q = -abs(dm))

T <- length(d)
h <- 1
k <- ((T + 1 - 2 * h + (h / T) * (h - 1)) / T) ^ (1 / 2)

hln <- dm*k

2 * pt(-abs(hln), df = T - 1)

#checking ourselves
dm.test(e1 = L.mean_model, e2 = L.ar, power = 1) #We have already squared our errors, that's why power = 1

# A small difference is only because we used unbiased estimator of variance, while in the function a biased one is used
biased_var <- function(x){
    m <- mean(x)
    result <- sum((x - m)^2)/length(x)
    result
}

d.mean/sqrt(biased_var(d)/T)*k; 2*pt(q = -abs(d.mean/sqrt(biased_var(d)/T)*k), df = T - 1)

# First, is fitting the model
test_model <- Arima(y = sp.ret, order = c(1, 0, 0))

summary(test_model)

str(test_model)

# And the 1-step ahead forecasting from the model
test_pred <- forecast(object = test_model, h = 1)

summary(test_pred)

str(test_pred)

# Probably, we want the point forecats value
test_pred$mean[1]

# Now, let's get predictions for MA(1) model
y_pred_ma <- # initialiaze a vector

for (i in 1:N_OOS){
    y_train <- # keep your train data
    ma <- #fit the model on your train data
    y_pred_ma[i] <- #make a forecast
}

y_pred_ma <- as.xts(y_pred_ma, order.by = index(y_true))

par(xpd = T)
plot(x = cbind(y_true, y_pred_mean, y_pred_ar, y_pred_ma), 
     main = 'Returns and predictions', 
     y = index(y_true), 
     col = c('black', 'red', 'blue', 'green'),
     lwd = c(1, 2, 2, 2))
legend('bottomleft', 
       legend = c('Returns', 'Mean model predictions', 'AR(1) model predictions', 'MA(1) model predictions'), 
       col = c('black', 'red', 'blue', 'green'),
       lty = c(1, 1, 1, 1),
       inset = 0.05)

# let's compute MSE for MA(1) model also
mse_ma <- #your code

mse_mean;mse_ar;mse_ma

# computing squared forecasting errors
L.ma <- #your code

# Let's compare the MA(1) model with the constant
# We've already computed DM test by ourselves, so we can legitimately use written function
dm.test(e1 = L.mean_model, e2 = L.ma, h = 1, power = 1)

# And for curiosity do DM test for AR(1) and MA(1) models
dm.test(e1 = L.ar, e2 = L.ma, h = 1, power = 1)

# the length of our forecats is basically the number of out-of-sample observations
T <- N_OOS

# We have already calculated all losses
# We need to calculate differences
# We have three models -> we need two difference series
d_1 <- #difference between mean model and AR(1) model
d_2 <- #difference between AR(1) model and MA(1) model
d_multiple <- cbind(d_1, d_2)

# Let's put them into one vector
d_multiple.mean <- #mean of the columns

omega <- #consistent variance estimator for 1-step ahead forecast

# Our statistic is then
S <- #your code

# use pchisq() function in order to get p-value
pchisq()

# checking ourselves
library(multDM)
MDM.test(realized = y_true, 
         evaluated = t(cbind(y_pred_mean, y_pred_ar, y_pred_ma)), 
         q = 0, 
         statistic = 'S', 
         loss.type = 'SE')

y_pred_arma <- #initialiaze a vector

for (i in 1:N_OOS){
    y_train <- # keep your train data
    arma <- #fit the model
    y_pred_arma[i] <- #make a forecast
}

y_pred_arma <- as.xts(y_pred_arma, order.by = index(y_true))

par(xpd = T)
plot(x = cbind(y_true, y_pred_mean, y_pred_ar, y_pred_ma, y_pred_arma), 
     main = 'Returns and predictions', 
     y = index(y_true), 
     col = 1:5,
     lwd = c(1, 2, 2, 2, 2))
legend('bottomleft', 
       legend = c('Returns', 'Mean model predictions', 'AR(1) model predictions', 'MA(1) model predictions', 'ARMA(1, 1) model predictions'), 
       col = 1:5,
       lty = c(1, 1, 1, 1),
       inset = 0.05)

# let's compute MSE for MA(1) model also
mse_arma <- #your code

mse_mean;mse_ar;mse_ma;mse_arma

L.arma <- #calculate squared forecasting errors

d_gw <- #calculate the difference series between ARMA(1, 1) model and AR(1) model

TT <- T-1
h <- #bing vector of ones of length (T-1) and difference series from 1 to (T-1)

d_gw <- #re-write your difference series from 2 to T

# As we have 1-step ahead forecast we can this

hL <- matrix(0, nrow = nrow(h), ncol = ncol(h))

for (i in 1:ncol(h)){
    hL[, i] <- # multiply your h and new difference series. Be careful with xts. Better to convert to vectors and multiply them
}

omega_gw <- #calculate consistent variance estimator

gw <- #use formula for the statistic

gw

#calculate p-value
pchisq()

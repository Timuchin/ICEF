# Libraries
# For those who has a problem like: package ‘package_name’ is not available (for R version x.x.x)
# install.packages('package_name', dependencies=TRUE, repos='http://cran.rstudio.com/')

library(quantmod)
library(moments)
library(forecast) #I highly recommend to visit the link above. It explains basics of forecasting and time-series modeling in R

# For those who have operational system in Russian but wants it in English
Sys.setlocale("LC_TIME", "C")
format(Sys.Date(), format = "%Y-%b-%d")

sp <- getSymbols("^GSPC", scr = "yahoo", auto.assign = FALSE)

# choose adjusted prices


# make log returns


# Plot your returns


# Let's see summary of the returns


#let's plot the histogram of returns and normal distribution


# What aboit skewness and kurtosis


# let's plot autocorrelations


# test if returns are normally distributed


# what about unit roots


# For those of you who suddenly forgot what is in-sample fit
summary(lm(sp.ret ~ 1))

N <- length(sp.ret) # let's have a variable with the size\length of our data
# Now we can explicitly say what our window size is
window_size <- 250
# Or we can say long should our prediction be
N_OOS <- round(x = 0.3 * N, digits = 0) # Usually, 25%-30% of data are used for the prediction period
N_sample <- N - N_OOS

y_train <- # initialize vector with length of N_sample
y_pred <- # initialize vector with length of N_OOS
y_true <- # get your actual returns

for (i in 1:N_OOS){
    y_train <- #put in your training data
    y_pred[i] <- #make a prediction from your model
}

y_pred <- as.xts(x = y_pred, order.by = index(y_true)) #change the type of variable to xts (time-series)

# plot actual returns and your forecasts


mse <- # calculate mean squared prediction error

y_pred_ar <- # initialize vector with length of N_OOS

for (i in 1:N_OOS){
    y_train <- #put in your training data
    ar <- #fit a model
    y_pred_ar[i] <- #make a prediction
}

y_pred_ar <- as.xts(x = y_pred_ar, order.by = index(y_true)) #change the type of variable to xts (time-series)

# plot actual returns and your forecasts

mse_ar <- # calculate mean squared prediction error

mse; mse_ar

# Let's go
L.mean_model <- # calculate the squared forecasting errors
L.ar <- # calculate the squared forecasting errors

d <- # calculate the difference

d.mean <- #calculate the mean of difference

d.var <- #calculate the unbiased estimator of variance of difference series

dm <- # calculate DM statistics

# let's look at it


# calculate p-value of the test assuming, that DM follows standard normal distribution
# in R you use pnorm() function in order to get cdf
# see more here http://seankross.com/notes/dpqr/


T <- # put in the length of difference series
h <- # how many steps ahead forecast are we doing
k <- # write formula for k

hln <- # calculate corrected statistics

# calculate p-value for the test
# for Student's t distribution cdf use pt() function

#checking ourselves
dm.test(e1 = L.mean_model, e2 = L.ar, power = 1) #We have already squared our errors, that's why power = 1

# A small difference is only because we used unbiased estimator of variance, while in the function a biased one is used
biased_var <- function(x){
    m <- mean(x)
    result <- sum((x - m)^2)/length(x)
    result
}

d.mean/sqrt(biased_var(d)/n)*k; 2*pt(q = -abs(d.mean/sqrt(biased_var(d)/n)*k), df = T - 1)

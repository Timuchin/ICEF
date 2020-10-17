x <- c(10, 15, 25, 37, 48, 23, 44, 19, 32, 20)
set.seed(123)

# We want to randomly choose values from x in order to create the new vector

# I highly recommend you to use function sample() -> in order to get help: use command ?sample

# At first let's randomly pick indices of initial values in x
# Don't forget that you resampling with replacement
indices <- ###
 
x.bootstrap <- # Now we should pick values by indices from x
x.bootstrap

# your code
# it is random, so do not be surprised that result is different

# Remember the last class?
# We have some huge matrix
# We want to calculate mean of each column

big.matrix <- matrix(rnorm(50000), ncol = 5, nrow = 10000)

head(big.matrix)

columns.mean <- # use the apply() function

# checking
round(columns.mean, 10) == round(colMeans(big.matrix), 10)

?lapply

big.list <- list(big.matrix[,1], big.matrix[,2], big.matrix[,3], big.matrix[,4], big.matrix[,5])

columns.mean.list <- # use the lapply() function

# checking
round(unlist(columns.mean.list, use.names = FALSE), 10) == round(colMeans(big.matrix), 10)

columns.mean.simple <- # use the sapply() function

round(columns.mean.simple, 10) == round(colMeans(big.matrix), 10)

# Basically, your X is a generator, like in loop, from 1 to 5000
# Do not forget to convert sample() function result into a function
bootstrap.samples <- #

# Now, let's calculate mean for each of 5000 bootstrapped samples
bootstrap.samples.mean <- #

# Let's calculate quantiles for our vector of 5000 means
CI <- #

cat('Mean of x:', mean(x))
CI

# Libraries
# For those who has a problem like: package ‘package_name’ is not available (for R version x.x.x)
# install.packages('package_name', dependencies=TRUE, repos='http://cran.rstudio.com/')

library(boot) # great library for bootstrap
library(MCS)
library(quantmod)
library(forecast) #I highly recommend to visit the link above. It explains basics of forecasting and time-series modeling in R

# For those who have operational system in Russian but wants it in English
Sys.setlocale("LC_TIME", "C")
format(Sys.Date(), format = "%Y-%b-%d")

head(trees)

?trees

# plot volume against height


# plot volume against girth


# First, define the function
reg <- function(data, indices){
    d <-  # we are picking bootstrapped indices
    
    H_relationship <-  # use linear regression of volume on height
    H_r_sq <-  # get the R-squared
    
    G_relationship <-  # use linear regression of volume on girth
    G_r_sq <-  # get the R-square
    
    combined_relationship <-  # use linear regression of volume on height and girth
    combined_r_sq <-  # get the R-square
    
    relationships <-  # put all R-square into one vector
    
    return(relationships)
    
}

lm_boot <- matrix(NA, nrow = 5000, ncol = 3)

for (i in 1:5000){
    lm_boot[i, ] <- # use reg function defined earlier and sample() function
}

# plot histogram of R-squared for height regression
# plot line of the density of the bootstrapped R-squared for height regression
# add horizontal lines for 5% and 95% percentiles


results <- boot(data = trees, statistic = reg, R = 5000)
print(results)

CI <- boot.ci(results, index = 1)
print(CI)

?lynx

time.data <- lynx
plot(time.data, main = "Annual Canadian Lynx trappings")

?tsboot

tsboot(lynx, mean, R = 1000, sim = "fixed", l = ar(lynx)$order)

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
y_pred_ma <- vector(mode = 'logical', length = N_OOS)
y_pred_arma <- vector(mode = 'logical', length = N_OOS)

y_true <- sp.ret[(N_sample + 1):N]

for (i in 1:N_OOS){
    y_train <- sp.ret[i:(N_sample + i - 1)] #It is a good practice to store your in-sample data
    
    y_pred_mean[i] <- mean(y_train) # Because then you can just call the function on it
    
    ar <- Arima(y_train, order = c(1, 0, 0))
    y_pred_ar[i] <- forecast(object = ar, h = 1)$mean[1]
    
    ma <- Arima(y = y_train, order = c(0, 0, 1))
    y_pred_ma[i] <- forecast(object = ma, h = 1)$mean[1]
    
    arma <- Arima(y = y_train, order = c(1, 0, 1))
    y_pred_arma[i] <- forecast(object = arma, h = 1)$mean[1]

}

y_pred_mean <- as.xts(x = y_pred_mean, order.by = index(y_true))
y_pred_ar <- as.xts(x = y_pred_ar, order.by = index(y_true))
y_pred_ma <- as.xts(x = y_pred_ma, order.by = index(y_true))
y_pred_arma <- as.xts(x = y_pred_arma, order.by = index(y_true))

mse_mean <- mean((y_true - y_pred_mean)^2)
mse_ar <- mean((y_true - y_pred_ar)^2)
mse_ma <- mean((y_true - y_pred_ma)^2)
mse_arma <- mean((y_true - y_pred_arma)^2)

L.mean_model <- (y_true - y_pred_mean)^2
L.ar <- (y_true - y_pred_ar)^2
L.ma <- (y_true - y_pred_ma)^2
L.arma <- (y_true - y_pred_arma)^2

names(L.mean_model) <- 'Mean'
names(L.ma) <- 'MA'
names(L.ar) <- 'AR'
names(L.arma) <- 'ARMA'

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

Loss <- cbind(L.mean_model, L.ma, L.ar, L.arma)

?MCSprocedure

MCSprocedure(Loss = Loss)

MCSprocedure(Loss = Loss, alpha = 0.25)

MCSprocedure(Loss = Loss, alpha = 0.35)

MCSprocedure(Loss = Loss, alpha = 0.95)

# Examples of installing packages
# install.packages("rusquant", repos="http://R-Forge.R-project.org")
# install.packages('tseries')
# install.packages('moments')

# For those who has non english OS
# Read ?Locales for more.
# Sys.setlocale("LC_TIME", "C")

# Let's use some libraries
library(quantmod)
library(rusquant) 
library(tseries) 
library(moments)
library(ggplot2)
library(repr)

Apple <- quantmod::getSymbols("AAPL", scr = "yahoo", auto.assign = FALSE)
Aeroflot <- getSymbols("AFLT", src = "Finam", auto.assign = FALSE)

# Let's look into what we downloaded
head(Apple)

head(Aeroflot)

# In most cases we are interested in the last price of the trading day (or close price)
Apple_price <- Apple$AAPL.Adjusted # Yahoo adjusts price of the companies to the dividends
Aeroflot_price <- Aeroflot$AFLT.Close # Unfortunately Finam does not

options(repr.plot.width=15, repr.plot.height=8) #In order to make the plots smaller
#par(mfrow = c(2, 1)) # Mostly for those who uses RStudio
plot(Apple_price, main = 'Apple price')
plot(Aeroflot_price, main = 'Aeroflot price')

acf(Apple_price, main = 'Apple price ACF')
acf(Aeroflot_price, main = 'Aeroflot price ACF')

pacf(Apple_price, main = 'Apple price PACF')
pacf(Aeroflot_price, main = 'Aeroflot price PACF')

adf.test(Apple_price)
adf.test(Aeroflot_price)

# We will use log returns as they are... (please read lecture notes)
Apple_return <- diff(log(Apple_price))[-1] * 100
Aeroflot_return <- diff(log(Aeroflot_price))[-1] * 100

plot(Apple_return, main = 'Apple returns')
plot(Aeroflot_return, main = 'Aeroflot returns')

acf(Apple_return, main = 'Apple returns ACF')
acf(Aeroflot_return, main = 'Aeroflot returns ACF')

pacf(Apple_return, main = 'Apple returns PACF')
pacf(Aeroflot_return, main = 'Aeroflot returns PACF')

adf.test(Apple_return)
adf.test(Aeroflot_return)

skewness(Apple_return) # should be equal to 0
skewness(Aeroflot_return) # should be equal to 0
kurtosis(Apple_return) # should be equal to 3
kurtosis(Aeroflot_return) # should be equal to 3

options(repr.plot.width=14, repr.plot.height=10)
hist(Apple_return, freq = FALSE, breaks = 'FD', main = 'Histogram of Apple returns', xlab = NULL)
curve(dnorm(x, mean = mean(Apple_return), sd = sd(Apple_return)), col="blue", lwd=2, add=TRUE)
legend("topright", legend = "Normal Distribution", col = "blue", lty=1:2, cex = 0.9)

options(repr.plot.width=13, repr.plot.height=10)
hist(Aeroflot_return, freq = FALSE, breaks = 'FD', main = 'Histogram of Aeroflot returns', xlab = NULL)
curve(dnorm(x, mean = mean(Aeroflot_return), sd = sd(Aeroflot_return)), col="blue", lwd=2, add=TRUE)
legend("topright", legend = "Normal Distribution", col = "blue", lty=1:2, cex = 0.9)

jarque.bera.test(Apple_return)
jarque.bera.test(Aeroflot_return)

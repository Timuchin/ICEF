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

# Let's choose two companies: one russian and one other
# in order to get tickets go to https://finance.yahoo.com/

other_company <- quantmod::getSymbols("AAPL", scr = "yahoo", auto.assign = FALSE)
russian_company <- getSymbols("AFLT", src = "Finam", auto.assign = FALSE)

# Let's look into what we downloaded
# R has function head() 
# try to use it

head(russian_company)


head(other_company)

# In most cases we are interested in the last price of the trading day (or close price)
# try to choose particular column of you data


russian_company_price <- russian_company$AFLT.Close
other_company_price <-  other_company$AAPL.Adjusted

options(repr.plot.width=15, repr.plot.height=8) #In order to make the plots smaller
#par(mfrow = c(2, 1)) # Mostly for those who uses RStudio

# Try to plot prices

### Your code here ###

# R has built-in function acf()
# Try to use it
# What can you say?

### Your code ###

# R has built-in function pacf()
# Try to use it
# What can you say?

### Your code ###

# What is the most common test for unit root?
# Do you think that R has it?
# Try to find the name of the function 

# Hint: the package tseries has it
# You can always read the manual on CRAN: https://cran.r-project.org/web/packages/tseries/tseries.pdf

### Your code here ###

# What can you say?

# We will use log returns as they are... (please read lecture notes)
russian_company_return <- diff(log(russian_company_price))[-1] * 100
other_company_return <- diff(log(other_company_price))[-1] * 100

# Plot returns

### Your code here ###

# Use acf() on returns
# What can you say?

# Use pacf() on returns
# What can you say?

# Use unit root test on returns
# What can you say?

skewness(russian_company_return) # should be equal to 0
skewness(other_company_return) # should be equal to 0
kurtosis(russian_company_return) # should be equal to 3
kurtosis(other_company_return) # should be equal to 3

# Some pretty plots
options(repr.plot.width=14, repr.plot.height=10)
hist(Apple_return, freq = FALSE, breaks = 'FD', main = 'Histogram of Russian company returns', xlab = NULL)
curve(dnorm(x, mean = mean(Apple_return), sd = sd(Apple_return)), col="blue", lwd=2, add=TRUE)
legend("topright", legend = "Normal Distribution", col = "blue", lty=1:2, cex = 0.9)

options(repr.plot.width=13, repr.plot.height=10)
hist(Aeroflot_return, freq = FALSE, breaks = 'FD', main = 'Histogram of Other company returns', xlab = NULL)
curve(dnorm(x, mean = mean(Aeroflot_return), sd = sd(Aeroflot_return)), col="blue", lwd=2, add=TRUE)
legend("topright", legend = "Normal Distribution", col = "blue", lty=1:2, cex = 0.9)

# Try to find the implementation of this test in R
# Hint: you can again look into tseries package

### Your Code ###

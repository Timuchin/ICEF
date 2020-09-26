options(warn = -1)
library(xts)
library(tidyquant)
library(dplyr)
library(readr)

startDate <- "2014-05-01"
endDate <- "2015-12-31"

# Firm Data
firmSymbols <- c("VOW3.DE", "NSU.DE", "PAH3.DE", "BMW.DE", "DAI.DE")
firmNames <- c("VW preferred", "Audi", "Porsche Automobil Hld", "BMW", "Daimler")
firms <- getSymbols(Symbols = firmSymbols, from = startDate, to = endDate)
firms.prices <- cbind(VOW3.DE$VOW3.DE.Adjusted, PAH3.DE$PAH3.DE.Adjusted, NSU.DE$NSU.DE.Adjusted,
DAI.DE$DAI.DE.Adjusted, BMW.DE$BMW.DE.Adjusted)
head(firms.prices)

# Index Data
indexSymbol <- c("^GDAXI")
indexName <- c("DAX")
index <- getSymbols(Symbols = indexSymbol, from = startDate, to = endDate, auto.assign =  F)
head(index)

# Making log returns
firms.ret <- ### your code
index.ret <- ### your code

startpoint = which(index(index.ret) == "2015-09-18")
estimation_window <- 250
event_window <- 20

fitted.models <- list() 

est_window_market <- # we need to take observations of the market that took place before window

for(i in colnames(firms.ret)){
    est_window_returns <- # we need to take observations of the firms returns that took place before window
    fitted.models[[i]] <- # linear model is already incorporated in R. How we can use it?
    print(i)
    print(summary(fitted.models[[i]]))
}

predicted <- matrix(NA, nrow = event_window, ncol = length(firmSymbols))
colnames(predicted) <- c("VOW3.DE", "NSU.DE", "PAH3.DE", "BMW.DE", "DAI.DE")
for(i in 1:length(firmSymbols)){
    for(j in 1:event_window){
        predicted[j, i] <- # the easiest way is to take const of the model + beta * observation
    }
}

# What about re-writing it in a "normal", not eye-bleeding loop... I mean matrices, of course
# Try to do it at home

predicted <- as.xts(predicted, order.by = as.Date(index(index.ret)[(startpoint - event_window/ 2):(startpoint + event_window/2-1)]))

y_true <- matrix(NA, nrow = event_window, ncol = length(firmSymbols))
colnames(y_true) <- c("VOW3.DE", "NSU.DE", "PAH3.DE", "BMW.DE", "DAI.DE")
y_true <- firms.ret[(startpoint - event_window/2):(startpoint + event_window/2-1)]

ab.ret <- matrix(NA, nrow = NROW(predicted), ncol = ncol(predicted))
colnames(ab.ret) <- colnames(predicted)

for(i in 1:ncol(predicted)){
    ab.ret[, i] <- y_true[, i] - predicted[, i]
}

ab.ret <- as.data.frame(ab.ret)
rownames(ab.ret) <- as.Date(index(predicted))

J_3 <- vector()
p_value <- vector()
for (i in colnames(ab.ret)){
    J_3[i] <- # try to code it
    p_value[i] <- # how can we calculate p-value
}
print(firmSymbols)
print(J_3)
sprintf("%.3f", p_value)

vow <- firms.ret$VOW3.DE.Adjusted # let's pick VW

event_returns <- # your code

event_market <- # your code

est_window_vow <- # your code

vow_model <- # linear model

summary(vow_model)

coeff_vow <- as.numeric(vow_model$coefficients)

out_of_sample <- as.matrix(cbind(rep(1, event_window), event_market))

in_sample <- as.matrix(cbind(rep(1, estimation_window), est_window_market))

predictions <- # your code

abnorm_returns <- event_returns - predictions

abnorm_returns

ab.ret[1]

ones <- diag(event_window)

var_abnorm <- (summary(vow_model)$sigma)^2*(ones + out_of_sample %*% solve(t(in_sample) %*% in_sample)%*% t(out_of_sample))

c_matrix <- t(lower.tri(matrix(1,event_window,event_window),diag = TRUE))

event_car <- c(rep(0, event_window))

event_car[event_window/2] <- 1

c_matrix <- cbind(c_matrix, event_car)

CARs_for_each_c <- t(c_matrix)%*%abnorm_returns

Var_for_each_c <- diag(t(c_matrix)%*%var_abnorm%*%c_matrix)

SCAR_for_each_c <- CARs_for_each_c/sqrt(Var_for_each_c)

SCAR_for_each_c

p_vals_asym <- # your code

p_vals_student <- 2*(1-pt(abs(SCAR_for_each_c), df=estimation_window-2))

p_vals <- cbind(p_vals_asym, p_vals_student)

colnames(p_vals) <- c("Asymptotic SCARs p-val", "Student-t SCARs p-val")

p_vals

abnorm_returns[,1] <- round(as.numeric(abnorm_returns),5)

colnames(abnorm_returns) <- c("Abnormal Returns")

colnames(CARs_for_each_c) <- c("CARs for each c")

p_vals <- round(p_vals, 3)

results <- list(summary(vow_model), abnorm_returns, CARs_for_each_c, p_vals)

results

plot(vow[(startpoint-event_window/2):(startpoint+event_window/2-1)], 
     main = 'Plot of returns and abnormal returns', 
     ylim = c(-50, 10), 
     col = 'black')

lines(cumsum(abnorm_returns), 
      col = 'red', 
      lwd = 3)

legend('bottomleft',
       legend = c('Returns', 'Cumulative Abnormal Returns'), 
       col = c('black', 'red'), 
       lty=1, 
       lwd = 1)

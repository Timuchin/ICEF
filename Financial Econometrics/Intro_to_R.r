x <- c(10.4, 5.6, 3.1, 6.4, 21.7)
x

1/x

x*2

v <- 2*x+1
v

length(x); mean(x)

MSE <- sum((x-mean(x))^2)/length(x)
MSE

x <- seq(-5, 4, by = .2)
y <- seq(length = 51, from = -5, by = .2)
z <- rep(x, times = 5)
k <- rep(x, each = 5)

x

y

z

k

tmp <- x > 0
labs <- paste(c("X", "Y"), 1:10, sep = ".")

tmp

labs

x <- array(1:25, dim = c(5, 5))
x

x[6]

x[1, 2]

X <- matrix(rnorm(n = 100*100, mean = 0, sd = 1), 100, 100)
Y <- matrix(rnorm(n = 100*100, mean = 1, sd = 4), 100, 100)
Z <- X + Y
a <- as.vector(rnorm(n = 100))
W <- cbind(Y, a)

head(X)

head(Y)

head(Z)

head(W)

N <- X%*%Y
T <- X * Y
Y_1 <- solve(Y)
b <- a %*% Y #Be carefull with dimensions

head(N)

head(T)

head(Y_1)

b

Lst <- list(name = "Tim", education = "HSE", job = "Avito", mood = "sleeping")
str(Lst)

X <- as.data.frame(X)

head(X)

attach(faithful)
summary(eruptions)
hist(eruptions, seq(1.6, 5.2, .2), probability = TRUE)
lines(density(eruptions, bw = 0.1))
qqnorm(eruptions); qqline(eruptions)

set.seed(123)

x1 <- 5 + 8*rnorm(n = 1000) + rnorm(n = 1000)
x2 <- 8 + 10*rnorm(n = 1000, mean = 1, sd = 4) + rnorm(n = 1000)
y <- 4 + 5*x1 - 6*x2 + rnorm(n = 1000)
model <- lm(y ~ x1 + x2); summary(model)

a <- as.vector(rep(1, times = length(x1)))

X <- as.matrix(cbind(a, x1, x2))
Y <- as.matrix(y)

beta <- solve(t(X) %*% X) %*% (t(X) %*% Y); beta

for (i in 1:10) {
  print("I do not like to study on Saturday")
}

b <- 0

while (b < 10) {
  print("I do not like to study on Saturday")
  b <- b + 1
}

HW_mark <- 5

if (HW_mark < 35){
  print("Ops, you fail")
} else if (HW_mark < 55){
  print("Pass but not good")
} else if (HW_mark < 75){
  print("Good result")
} else {
  print("Wow, awesome")
}

marktransition <- function(HW_mark){
  if (HW_mark < 35){
    print("Ops, you fail")
  } else if (HW_mark < 55){
    print("Pass but not good")
  } else if (HW_mark < 75){
    print("Good result")
  } else {
    print("Wow, awesome")
  }
}

marktransition(98)  

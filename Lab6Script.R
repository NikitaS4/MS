# includes
library(ggplot2)
library(L1pack)

# linear function with normal random noize
baseline <- function(x) {
  return (2 + 2*x + rnorm(1))
}


# create outliers for the first and the last elements 
create_outliers <- function(y_vec) {
  y_vec[1] <- y_vec[1] + 10
  y_vec[-1] <- y_vec[-1] - 10
}


# generate correlated pairs
dataset <- function(size, from, to) {
  x <- seq(from, to, length.out = size)
  y <- baseline(x)
  return (data.frame(x = x, y = y))
}


# generate pairs with outliers
dataset_outliers <- function(size, from, to) {
  x <- seq(from, to, length.out = size)
  y <- baseline(x)
  create_outliers(y)
  return (data.frame(x = x, y = y))
}


# create dfs

dfGood <- dataset(size = 20, from = -1.8, to = 2)
dfBad <- dataset_outliers(size = 20, from = -1.8, to = 2)

fitGood <- lm(y ~ x, dfGood)
fitBad <- lm(y ~ x, dfBad)

#summary(fitGood)
#summary(fitBad)

print(fitGood$coefficients)

print(fitBad$coefficients)


fitGoodLAD <- lad(y ~ x, dfGood)
print(fitGoodLAD$coefficients)
ladGood0 <- fitGoodLAD$coefficients[1]
ladGood1 <- fitGoodLAD$coefficients[2]

fitBadLAD <- lad(y ~ x, dfBad)
print(fitBadLAD$coefficients)
ladBad0 <- fitBadLAD$coefficients[1]
ladBad1 <- fitBadLAD$coefficients[2]


# plots
ggplot(fitGood, aes(x, y))+
  geom_point() +
  geom_smooth(method = "lm") +
  stat_function(fun = function (x) (ladGood0 + ladGood1 * x), col = "red")

ggplot(fitBad, aes(x, y)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_function(fun = function (x) (ladBad0 + ladBad1 * x), col = "red")
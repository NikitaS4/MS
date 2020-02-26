library(stringr)
library(LaplacesDemon)


size = 10
size = 50
size = 1000

#Normal

bord_Gauss <- 5 #borders of the interval

Gauss <- (rnorm(size, 0, 1))
Gauss_hist_name <- str_c("Gauss, ", toString(size), " elements")

hist(Gauss, prob=TRUE, main=Gauss_hist_name, xlim=c(-bord_Gauss, bord_Gauss))
curve(dnorm(x, 0, 1), add=TRUE, col="red")


#Cauchy

bord_Cauchy <- 150

Cauchy <- (rcauchy(size, 0, 1))
Cauchy_hist_name <- str_c("Cauchy, ", toString(size), " elements")

hist(Cauchy, breaks=200, prob=TRUE, main=Cauchy_hist_name, xlim=c(-bord_Cauchy, bord_Cauchy))
curve(dcauchy(x, 0, 1), add=TRUE, col="red")


#Laplace

bord_Laplace <- 6

Laplace <- (rlaplace(size, 0, 1/sqrt(2)))
Laplace_hist_name <- str_c("Laplace, ", toString(size), " elements")

hist(Laplace, breaks=12, prob=TRUE, main=Laplace_hist_name, xlim=c(-bord_Laplace, bord_Laplace))
curve(dlaplace(x, 0, 1/sqrt(2)), add=TRUE, col="red")


#Poisson

bord_Poisson <- 25
Poisson <- (rpois(size, 10))
Poisson_hist_name <- str_c("Poisson, ", toString(size), " elements")

hist(Poisson, breaks=16, prob=TRUE, main=Poisson_hist_name, xlim=c(0, bord_Poisson))
interval <- seq(0, bord_Poisson, 1)
vals <- numeric(length=length(interval))
for (i in 1:length(interval)) {
vals[i] <- dpois(interval[i], 10)
}
lines(interval, vals, col="red")
#curve(dpois(0:bord_Poisson, 10), add=TRUE, col="red")


#Unif

bord_Unif <- 3

Unif <- (runif(size, -sqrt(3), sqrt(3)))
Unif_hist_name <- str_c("Unif, ", toString(size), " elements")

hist(Unif, breaks=20, prob=TRUE, main=Unif_hist_name, xlim=c(-bord_Unif, bord_Unif))
curve(dunif(x, -sqrt(3), sqrt(3)), add=TRUE, col="red")


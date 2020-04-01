# includes
library(stringr)
library(LaplacesDemon)

# chooze needed size
size1 <- 20
size2 <- 100

# iterations for approx ouliers probability
iters <- 1000

# Gauss
boxplot(rnorm(size1, 0, 1), rnorm(size2, 0, 1), horizontal = TRUE, outline = TRUE, names = c('n = 20', 'n = 100'))

# Cauchy
cauchy1 <- rcauchy(size1, 0, 1) 
cauchy2 <- rcauchy(size2, 0, 1)
# with outlines
boxplot(cauchy1, cauchy2, horizontal = TRUE, outline = TRUE, names = c('n = 20', 'n = 100'))
# without outlines
boxplot(cauchy1, cauchy2, horizontal = TRUE, outline = FALSE, names = c('n = 20', 'n = 100'))

# Laplace
boxplot(rlaplace(size, 0, 1/sqrt(2)), rlaplace(size, 0, 1/sqrt(2)), horizontal = TRUE, outline = TRUE, names = c('n = 20', 'n = 100'))

# Poisson
boxplot(rpois(size, 10), rpois(size, 10), horizontal = TRUE, outline = TRUE, names = c('n = 20', 'n = 100'))

# Unif
boxplot(runif(size, -sqrt(3), sqrt(3)), runif(size, -sqrt(3), sqrt(3)), horizontal = TRUE, outline = TRUE, names = c('n = 20', 'n = 100'))

# outliers

# Gauss

outCount1 <- 0
outCount2 <- 0
wholeSize1 <- 0
wholeSize2 <- 0

for (i in 1:iters) {
  cur1 <- rnorm(size1, 0, 1)
  cur2 <- rnorm(size2, 0, 1)
  quants1 <- quantile(cur1)
  quants2 <- quantile(cur2) 
  LQ1 <- quants1[2]
  UQ1 <- quants1[4]
  LQ2 <- quants2[2]
  UQ2 <- quants2[4]
  LowerBord1 <- LQ1 - 1.5 * (UQ1 - LQ1)
  LowerBord2 <- LQ2 - 1.5 * (UQ2 - LQ2)
  UpperBord1 <- LQ1 + 1.5 * (UQ1 - LQ1)
  UpperBord2 <- LQ2 + 1.5 * (UQ2 - LQ2)
  outliers1 <- cur1[cur1 < LowerBord1 | cur1 > UpperBord1]
  outliers2 <- cur2[cur2 < LowerBord2 | cur2 > UpperBord2]
  outCount1 <- length(outliers1) + outCount1
  outCount2 <- length(outliers2) + outCount2
  wholeSize1 <- wholeSize1 + size1
  wholeSize2 <- wholeSize2 + size2
}

outProb1 <- outCount1 / wholeSize1 # mean outlier prob for 20
outProb2 <- outCount2 / wholeSize2 # mean outlier prob for 100


# Cauchy

outCount1 <- 0
outCount2 <- 0
wholeSize1 <- 0
wholeSize2 <- 0

for (i in 1:iters) {
  cur1 <- rcauchy(size1, 0, 1)
  cur2 <- rcauchy(size2, 0, 1)
  quants1 <- quantile(cur1)
  quants2 <- quantile(cur2) 
  LQ1 <- quants1[2]
  UQ1 <- quants1[4]
  LQ2 <- quants2[2]
  UQ2 <- quants2[4]
  LowerBord1 <- LQ1 - 1.5 * (UQ1 - LQ1)
  LowerBord2 <- LQ2 - 1.5 * (UQ2 - LQ2)
  UpperBord1 <- LQ1 + 1.5 * (UQ1 - LQ1)
  UpperBord2 <- LQ2 + 1.5 * (UQ2 - LQ2)
  outliers1 <- cur1[cur1 < LowerBord1 | cur1 > UpperBord1]
  outliers2 <- cur2[cur2 < LowerBord2 | cur2 > UpperBord2]
  outCount1 <- length(outliers1) + outCount1
  outCount2 <- length(outliers2) + outCount2
  wholeSize1 <- wholeSize1 + size1
  wholeSize2 <- wholeSize2 + size2
}

outProb1 <- outCount1 / wholeSize1 # mean outlier prob for 20
outProb2 <- outCount2 / wholeSize2 # mean outlier prob for 100

# Laplace

outCount1 <- 0
outCount2 <- 0
wholeSize1 <- 0
wholeSize2 <- 0

for (i in 1:iters) {
  cur1 <- rlaplace(size1, 0, 1/sqrt(2))
  cur2 <- rlaplace(size2, 0, 1/sqrt(2))
  quants1 <- quantile(cur1)
  quants2 <- quantile(cur2) 
  LQ1 <- quants1[2]
  UQ1 <- quants1[4]
  LQ2 <- quants2[2]
  UQ2 <- quants2[4]
  LowerBord1 <- LQ1 - 1.5 * (UQ1 - LQ1)
  LowerBord2 <- LQ2 - 1.5 * (UQ2 - LQ2)
  UpperBord1 <- LQ1 + 1.5 * (UQ1 - LQ1)
  UpperBord2 <- LQ2 + 1.5 * (UQ2 - LQ2)
  outliers1 <- cur1[cur1 < LowerBord1 | cur1 > UpperBord1]
  outliers2 <- cur2[cur2 < LowerBord2 | cur2 > UpperBord2]
  outCount1 <- length(outliers1) + outCount1
  outCount2 <- length(outliers2) + outCount2
  wholeSize1 <- wholeSize1 + size1
  wholeSize2 <- wholeSize2 + size2
}

outProb1 <- outCount1 / wholeSize1 # mean outlier prob for 20
outProb2 <- outCount2 / wholeSize2 # mean outlier prob for 100


# Poisson

outCount1 <- 0
outCount2 <- 0
wholeSize1 <- 0
wholeSize2 <- 0

for (i in 1:iters) {
  cur1 <- rpois(size1, 10)
  cur2 <- rpois(size2, 10)
  quants1 <- quantile(cur1)
  quants2 <- quantile(cur2) 
  LQ1 <- quants1[2]
  UQ1 <- quants1[4]
  LQ2 <- quants2[2]
  UQ2 <- quants2[4]
  LowerBord1 <- LQ1 - 1.5 * (UQ1 - LQ1)
  LowerBord2 <- LQ2 - 1.5 * (UQ2 - LQ2)
  UpperBord1 <- LQ1 + 1.5 * (UQ1 - LQ1)
  UpperBord2 <- LQ2 + 1.5 * (UQ2 - LQ2)
  outliers1 <- cur1[cur1 < LowerBord1 | cur1 > UpperBord1]
  outliers2 <- cur2[cur2 < LowerBord2 | cur2 > UpperBord2]
  outCount1 <- length(outliers1) + outCount1
  outCount2 <- length(outliers2) + outCount2
  wholeSize1 <- wholeSize1 + size1
  wholeSize2 <- wholeSize2 + size2
}

outProb1 <- outCount1 / wholeSize1 # mean outlier prob for 20
outProb2 <- outCount2 / wholeSize2 # mean outlier prob for 100


# Unif

outCount1 <- 0
outCount2 <- 0
wholeSize1 <- 0
wholeSize2 <- 0

for (i in 1:iters) {
  cur1 <- runif(size1, -sqrt(3), sqrt(3))
  cur2 <- runif(size2, -sqrt(3), sqrt(3))
  quants1 <- quantile(cur1)
  quants2 <- quantile(cur2) 
  LQ1 <- quants1[2]
  UQ1 <- quants1[4]
  LQ2 <- quants2[2]
  UQ2 <- quants2[4]
  LowerBord1 <- LQ1 - 1.5 * (UQ1 - LQ1)
  LowerBord2 <- LQ2 - 1.5 * (UQ2 - LQ2)
  UpperBord1 <- LQ1 + 1.5 * (UQ1 - LQ1)
  UpperBord2 <- LQ2 + 1.5 * (UQ2 - LQ2)
  outliers1 <- cur1[cur1 < LowerBord1 | cur1 > UpperBord1]
  outliers2 <- cur2[cur2 < LowerBord2 | cur2 > UpperBord2]
  outCount1 <- length(outliers1) + outCount1
  outCount2 <- length(outliers2) + outCount2
  wholeSize1 <- wholeSize1 + size1
  wholeSize2 <- wholeSize2 + size2
}

outProb1 <- outCount1 / wholeSize1 # mean outlier prob for 20
outProb2 <- outCount2 / wholeSize2 # mean outlier prob for 100

# Quartiles for theoretical estimation

# Gauss
LQ <- qnorm(0.25, 0, 1)
UQ <- qnorm(0.75, 0, 1)
X1 <- LQ - 1.5 * (UQ - LQ)
X2 <- LQ + 1.5 * (UQ - LQ)
GaussOutliers <- pnorm(X1, 0, 1) + (1 - pnorm(X2, 0, 1))

# Cauchy
LQ <- qcauchy(0.25, 0, 1)
UQ <- qcauchy(0.75, 0, 1)
X1 <- LQ - 1.5 * (UQ - LQ)
X2 <- LQ + 1.5 * (UQ - LQ)
CauchyOutliers <- pcauchy(X1, 0, 1) + (1 - pcauchy(X2, 0, 1))

# Laplace
LQ <- qlaplace(0.25, 0, 1/sqrt(2))
UQ <- qlaplace(0.75, 0, 1/sqrt(2))
X1 <- LQ - 1.5 * (UQ - LQ)
X2 <- LQ + 1.5 * (UQ - LQ)
LaplaceOutliers <- plaplace(X1, 0, 1/sqrt(2)) + (1 - plaplace(X2, 0, 1/sqrt(2)))

# Poisson
LQ <- qpois(0.25, 10)
UQ <- qpois(0.75, 10)
X1 <- LQ - 1.5 * (UQ - LQ)
X2 <- LQ + 1.5 * (UQ - LQ)
PoissonOutliers <- ppois(X1, 10) - dpois(X1, 10) + (1 - ppois(X2, 10))

# Unif
LQ <- qunif(0.25, -sqrt(3), sqrt(3))
UQ <- qunif(0.75, -sqrt(3), sqrt(3))
X1 <- LQ - 1.5 * (UQ - LQ)
X2 <- LQ + 1.5 * (UQ - LQ)
UnifOutliers <- punif(X1, -sqrt(3), sqrt(3)) + (1 - punif(X2, -sqrt(3), sqrt(3)))
# includes
library(stringr)
library(LaplacesDemon)

statSize <- 1000

# choose needed size
size <- 10
size <- 100
size <- 1000

# mem alloc
distrCnt <- 5
means = array(c(numeric(statSize), numeric(statSize), numeric(statSize), numeric(statSize), numeric(statSize)), 
              dim = c(5, statSize))
meds = array(c(numeric(statSize), numeric(statSize), numeric(statSize), numeric(statSize), numeric(statSize)), 
             dim = c(5, statSize))
zR = array(c(numeric(statSize), numeric(statSize), numeric(statSize), numeric(statSize), numeric(statSize)), 
          dim = c(5, statSize))
zQ = array(c(numeric(statSize), numeric(statSize), numeric(statSize), numeric(statSize), numeric(statSize)), 
           dim = c(5, statSize))
zTr = array(c(numeric(statSize), numeric(statSize), numeric(statSize), numeric(statSize), numeric(statSize)), 
            dim = c(5, statSize))
for (i in 1 : distrCnt) {
  means[i] <- c()#numeric(statSize)
  meds[i] <- c()#numeric(statSize)
  zR[i] <- c()#numeric(statSize)
  zQ[i] <- c()#numeric(statSize)
  zTr[i] <- c()#numeric(statSize)
}

# count statistics
for (i in 1 : statSize) {
  # distributions
  Gauss <- (rnorm(size, 0, 1))
  Cauchy <- (rcauchy(size, 0, 1))
  Laplace <- (rlaplace(size, 0, 1/sqrt(2)))
  Poisson <- (rpois(size, 10))
  Unif <- (runif(size, -sqrt(3), sqrt(3)))
  
  # statistics
  # mean
  means[1,i] <- mean(Gauss)
  means[2,i] <- mean(Cauchy)
  means[3,i] <- mean(Laplace)
  means[4,i] <- mean(Poisson)
  means[5,i] <- mean(Unif)
  # medians
  meds[1,i] <- median(Gauss)
  meds[2,i] <- median(Cauchy)
  meds[3,i] <- median(Laplace)
  meds[4,i] <- median(Poisson)
  meds[5,i] <- median(Unif)
  # zR
  zR[1,i] <- (min(Gauss) + max(Gauss))/2
  zR[2,i] <- (min(Cauchy) + max(Cauchy))/2
  zR[3,i] <- (min(Laplace) + max(Laplace))/2
  zR[4,i] <- (min(Poisson) + max(Poisson))/2
  zR[5,i] <- (min(Unif) + max(Unif))/2
  # zQ
  zQ[1,i] <- (quantile(Gauss, 0.75) + quantile(Gauss, 0.25))/2
  zQ[2,i] <- (quantile(Cauchy, 0.75) + quantile(Cauchy, 0.75))/2
  zQ[3,i] <- (quantile(Laplace, 0.75) + quantile(Laplace, 0.75))/2
  zQ[4,i] <- (quantile(Poisson, 0.75) + quantile(Poisson, 0.75))/2
  zQ[5,i] <- (quantile(Unif, 0.75) + quantile(Unif, 0.75))/2
  # zTrimmed
  zTr[1,i] <- mean(Gauss, trim = 0.25)
  zTr[2,i] <- mean(Cauchy, trim = 0.25)
  zTr[3,i] <- mean(Laplace, trim = 0.25)
  zTr[4,i] <- mean(Poisson, trim = 0.25)
  zTr[5,i] <- mean(Unif, trim = 0.25)
}

names <- c("Gauss", "Cauchy", "Laplace", "Poisson", "Unif")
statNames <- c("mean", "median", "zR", "zQ", "zTr")
statMeans <- c(mean(means[1,]), mean(means[2,]), mean(means[3,]), mean(means[4,]), mean(means[5,]))
statMeansVar <- c(var(means[1,]), var(means[2,]), var(means[3,]), var(means[4,]), var(means[5,]))

statMeds <- c(mean(meds[1,]), mean(meds[2,]), mean(meds[3,]), mean(meds[4,]), mean(meds[5,]))
statMedsVar <- c(var(meds[1,]), var(meds[2,]), var(meds[3,]), var(meds[4,]), var(meds[5,]))

statZr <- c(mean(zR[1,]), mean(zR[2,]), mean(zR[3,]), mean(zR[4,]), mean(zR[5,]))
statZrVar <- c(var(zR[1,]), var(zR[2,]), var(zR[3,]), var(zR[4,]), var(zR[5,]))

statZq <- c(mean(zQ[1,]), mean(zQ[2,]), mean(zQ[3,]), mean(zQ[4,]), mean(zQ[5,]))
statZqVar <- c(var(zQ[1,]), var(zQ[2,]), var(zQ[3,]), var(zQ[4,]), var(zQ[5,]))

statZtr <- c(mean(zTr[1,]), mean(zTr[2,]), mean(zTr[3,]), mean(zTr[4,]), mean(zTr[5,]))
statZtrVar <- c(var(zTr[1,]), var(zTr[2,]), var(zTr[3,]), var(zTr[4,]), var(zTr[5,]))

#Data frames
dfGauss <- data.frame(Chars = c("Mean", "Variance"), Means = c(statMeans[1], statMeansVar[1]), 
                      Medians = c(statMeds[1],  statMedsVar[1]), zR = c(statZr[1], statZrVar[1]), 
                      zQ = c(statZq[1], statZqVar[1]), zTr = c(statZtr[1], statZtrVar[1]))

dfCauchy <- data.frame(Chars = c("Mean", "Variance"), Means = c(statMeans[2], statMeansVar[2]), 
                       Medians = c(statMeds[2], statMedsVar[2]), zR = c(statZr[2], statZrVar[2]), 
                       zQ = c(statZq[2], statZqVar[2]), zTr = c(statZtr[2], statZtrVar[2]))

dfLaplace <- data.frame(Chars = c("Mean", "Variance"), Means = c(statMeans[3], statMeansVar[3]), 
                        Medians = c(statMeds[3], statMedsVar[3]), zR = c(statZr[3], statZrVar[3]), 
                        zQ = c(statZq[3], statZqVar[3]), zTr = c(statZtr[3], statZtrVar[3]))

dfPoisson <- data.frame(Chars = c("Mean", "Variance"), Means = c(statMeans[4], statMeansVar[4]),
                        Medians = c(statMeds[4], statMedsVar[4]), zR = c(statZr[4], statZrVar[4]),
                        zQ = c(statZq[4], statZqVar[4]), zTr = c(statZtr[4], statZtrVar[4]))

dfUnif <- data.frame(Chars = c("Mean", "Variance"), Means = c(statMeans[5], statMeansVar[5]), 
                     Medians = c(statMeds[5], statMedsVar[5]), zR = c(statZr[5], statZrVar[5]), 
                     zQ = c(statZq[5], statZqVar[5]), zTr = c(statZtr[5], statZtrVar[5]))
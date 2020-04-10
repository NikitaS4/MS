# includes
library(stringr)
library(MASS)
library(plotrix)
library(car)

# params
rho <- c(0, 0.5, 0.9)
sizes <- c(20, 60, 100)
num_of_experiments <- 1000


# covariance matrix
null_mtx <- matrix(c(NaN, NaN, NaN, NaN), 2, 2)
null_data = c(null_mtx, null_mtx, null_mtx)
cov_mtxs <- array(data = null_data, dim = c(2, 2, length(rho))) # allocation
for (i in 1:length(rho)) {
  cov_mtxs[,,i] <- matrix(c(1, rho[i], rho[i], 1), 2, 2)
}


# means
means <- c(0, 0)


# init statistics
coeff_name <- c("sign", "pearson", "spearman")
#init_vec <- c(0, 0, 0)
#init_data <- c(init_vec, init_vec, init_vec)
init_data <- numeric(27000)
correlations <- array(data = init_data, dim = c(3, 3, 3, 1000))
# first -- corr coeff type, second -- rho, third -- size, fourth -- num of experiment


# function to calculate correlation
corr_func <- function(c_name, vec) {
  if (c_name == "sign") {
    n1 <- length(vec[vec[,1] >= 0 & vec[,2] >= 0])
    n2 <- length(vec[vec[,1] < 0 & vec[,2] >= 0])
    n3 <- length(vec[vec[,1] < 0 & vec[,2] < 0])
    n4 <- length(vec[vec[,1] >= 0 & vec[,2] < 0])
    
    sum_n <- length(vec)
    return (((n1 + n3) - (n2 + n4))/sum_n)
    
  } else {
    c_mtx <- cor(vec, method = c_name)
    return (c_mtx[1, 2])
  }
}


# count correlations
for (cur_rho_num in 1:length(rho)) {
for (i in 1:length(coeff_name)) {
  for (j in 1:length(sizes)) {
    for (k in 1:num_of_experiments) {
      vec <- mvrnorm(n = sizes[j], mu = means, cov_mtxs[,,cur_rho_num])
      correlations[i, cur_rho_num, j, k] <- corr_func(c_name = coeff_name[i], vec = vec)
    }
  }
}
}


# count statistics

# experimentsData[i, j]: i -- coeff_type, j -- number of experiment
create_df <- function(experimentsData) {
  mean_z <- numeric(length(coeff_name))
  sqr_mean <- numeric(length(coeff_name))
  var_rho <- numeric(length(coeff_name))
  for (i in 1:length(coeff_name)) {
    # E(z)
    mean_z[i] <- mean(experimentsData[i,])
    # E(z^2)
    sqr_mean[i] <- mean(experimentsData[i,] ^ 2)
    # D(z)
    var_rho[i] <- var(experimentsData[i,])
  }
 
  row_names <- coeff_name
  df <- data.frame(Means = mean_z, SqrMeans = sqr_mean, Variances = var_rho, row.names = row_names)  
  return (df)
}


for (i in 1:length(sizes)) {
  for (j in 1:length(rho)) {
    print(sizes[i])
    print(rho[j])
    print(create_df(correlations[,j,i,]))
  }
}


# for complicated formula: f = 0.9N(..., 0.9) + 0.1N(..., -0.9)

# allocate memory
init_data <- numeric(9000)
correlations <- array(data = init_data, dim = c(3, 3, 1000))
# first -- corr coef type, second -- size, third -- num of experiment


# gen function

cov_plus <- cov_mtxs[,,3]
cov_minus <-  matrix(c(1, -0.9, -0.9, 1), 2, 2)

gen_function <- function(size, means) {
  return (0.9 * mvrnorm(size, means, cov_plus) + 0.1 * mvrnorm(size, means, cov_minus))
}


# fill correlations

for (i in 1 : length(coeff_name)) {
  for (j in 1 : length(sizes)) {
    for (k in 1 : num_of_experiments) {
      vec <- gen_function(sizes[j], means)
      correlations[i,j,k] <- corr_func(coeff_name[i], vec)
    }
  }
}


# count statistics
for (i in 1 : length(sizes)) {
  print(sizes[i])
  print(create_df(correlations[i,,]))
}



# Creating ellipses

create_ellipse <- function(size, cov_mtx) {
  vec <- mvrnorm(size, c(0, 0), cov_mtx)
  #vec <- gen_function(size, c(0, 0))
  dataEllipse(vec, levels = 0.95, center.pch = FALSE)
}


for (i in 1:length(sizes)) {
  for (j in 1:length(rho)) {
    rho_val <- cov_mtxs[1,2,j]
    n <- sizes[i]
    plot_name <- str_c("images/lab5/rho=", toString(rho_val), "n=", toString(n), ".png")
    png(plot_name)
    create_ellipse(n, cov_mtxs[,,j])
    dev.off()
  }
}

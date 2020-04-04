# includes
library(stringr)
library(LaplacesDemon)

# sizes
sizes <- c(20, 60, 100)


# Gauss fun approx 

fun_approx <- function(size, rand_func, p_func, func_name, a = -4, b = 4, step = 0.001, param1 = 0, param2 = 1) {
  x <- rand_func(size, param1, param2)
  n <- length(x)
  x <- sort(x); vals <- unique(x)
  rval <- approxfun(vals, cumsum(tabulate(match(x, vals)))/n, method = "constant", yleft = 0, yright = 1, f = 0, ties = "ordered")
  interval <- seq(a, b, step)
  title_name <- str_c(func_name, ",n=", toString(size))
  plot(interval, rval(interval), type = "l", col = "red", xlab = "x", ylab = "F(x)", main = title_name)
  curve(p_func(x, param1, param2), add = TRUE)
}

gauss_approx <- function(size) {
  fun_approx(size, rnorm, pnorm, "Gauss")
}

cauchy_approx <- function(size) {
  fun_approx(size, rcauchy, pcauchy, "Cauchy")
}

laplace_approx <- function(size) {
  fun_approx(size, rlaplace, plaplace, "Laplace", param1 = 0, param2 = 1/sqrt(2))
}

unif_approx <- function(size) {
  fun_approx(size, runif, punif, "Unif", param1 = -sqrt(3), param2 = sqrt(3))
}

choose_approx <- function(name) {
  if (name == "Gauss") {
    return (gauss_approx)
  }
  if (name == "Cauchy") {
    return (cauchy_approx)
  }
  if (name == "Laplace") {
    return (laplace_approx)
  }
  if (name == "Unif") {
    return (unif_approx)
  }
}

for (cur_name in c("Gauss", "Cauchy", "Laplace", "Unif")) {
  for (i in 1:3) {
    file_title = str_c("plots/Func/", cur_name, "/n=", toString(sizes[i]), ".png")
    png(file_title)
    fun <- choose_approx(cur_name)
    fun(sizes[i])
    dev.off()
  }
}

# for Poisson

for (i in 1:3) {
  file_title = str_c("plots/Func/", "Poisson", "/n=", toString(sizes[i]), ".png")
  png(file_title)
  
  x <- rpois(sizes[i], 10)
  n <- length(x)
  x <- sort(x); vals <- unique(x)
  rval <- approxfun(vals, cumsum(tabulate(match(x, vals)))/n, 
                    method = "constant", yleft = 0, yright = 1, f = 0,
                    ties = "ordered")
  left_bord <- 6
  right_bord <- 14
  step <- 0.001
  interval <- seq(left_bord, right_bord, step)
  title_name <- str_c("Poisson", ",n=", toString(sizes[i]))
  plot(interval, rval(interval), type = "l", col = "red", xlab = "x", ylab = "F(x)", main = title_name)
  curve(ppois(x, 10), add = TRUE)
  
  
  dev.off()
}


# Kernel density funcs approx

kernel_func <- function(u) { # kernel function
  return (exp(-u * u / 2) / sqrt(2 * pi))
}

density_estimation <- function(u, x, hn) { # f_n(x)
  n <- length(x)
  mul <- 1 / (n * hn)
  sum <- 0
  for (i in x) {
    sum <- sum + kernel_func((u - i) / hn)
  }
  return (mul * sum)
}

count_h <- function(x) { # count interval
  n <- length(x)
  k <- 1.72 * n ^ (1/3)
  return ((max(x) - min(x)) / k)
}

# Cont distributions

estimate <- function(rand_func, dens_func, func_name, func_param1, func_param2, size, hDivizor, a = -4, b = 4, step = 0.001) {
  interval <- seq(a, b, step)
  x <- rand_func(size, func_param1, func_param2)
  h <- count_h(x)
  nh <- h * hDivizor
  title_name <- str_c(func_name, "n = ", toString(size), ", h_n = h / ", toString(hDivizor))
  y <- density_estimation(interval, x, nh)
  plot(c(interval, NaN), c(y, dens_func(0, func_param1, func_param2)), type = "l", col = "red", xlab = "x", ylab = "f(x)", main = title_name)
  curve(dens_func(x, func_param1, func_param2), add = TRUE)
}

gauss_estimate <- function(size, hDivizor) {
  estimate(rnorm, dnorm, "Gauss", 0, 1, size, hDivizor)
}

cauchy_estimate <- function(size, hDivizor) {
  estimate(rcauchy, dcauchy, "Cauchy", 0, 1, size, hDivizor)
}

laplace_estimate <- function(size, hDivizor) {
  estimate(rlaplace, dlaplace, "Laplace", 0, 1/sqrt(2), size, hDivizor)
}

unif_estimate <- function(size, hDivizor) {
  estimate(runif, dunif, "Unif", -sqrt(3), sqrt(3), size, hDivizor)
}

choose_estimation <- function(name) {
  if (name == "Gauss") {
    return (gauss_estimate)
  }
  if (name == "Cauchy") {
    return (cauchy_estimate)
  }
  if (name == "Laplace") {
    return (laplace_estimate)
  }
  if (name == "Unif") {
    return (unif_estimate)
  }
}

for (cur_name in c("Gauss", "Cauchy", "Laplace", "Unif")) {
  for (i in 1:3) {
    for (h in c(2, 1, 0.5)) {
      file_title = str_c("plots/", cur_name, "/n=", toString(sizes[i]), "divizor=", toString(h), ".png")
      png(file_title)
      estimation <- choose_estimation(cur_name)
      estimation(sizes[i], h)
      dev.off()
    }
  }
}

# Poisson

for (i in sizes) {
  for (cur_h in c(2, 1, 0.5)) {
    file_title = str_c("plots/Poisson/n=", toString(i), "divizor=", toString(cur_h), ".png")
    png(file_title)
    #estimatePois(i, h)
    
    interval <- seq(6, 14, 1)
    x <- rpois(i, 10)
    h <- count_h(x)
    nh <- h * cur_h
    title_name <- str_c("Poisson", "n = ", toString(i), ", h_n = h / ", toString(cur_h))
    y <- density_estimation(interval, x, nh)
    plot(c(interval, NaN), c(y, dpois(10, 10)), type = "l", col = "red", xlab = "x", ylab = "f(x)", main = title_name)
    val_lines <- c()
    for (j in 1:length(interval)) {
      val_lines[j] <- dpois(interval[j], 10)
    }
    lines(interval, val_lines)
    
    dev.off()
  }
}
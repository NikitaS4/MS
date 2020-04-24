# includes


size <- 100

dataset <- rnorm(size, 0, 1)

print(mean(dataset))
print(var(dataset))

intervals <- hist(dataset)
intervals

xmean <- mean(dataset)
xvar <- var(dataset)

print(xmean)
print(xvar)

breaks <- intervals$breaks[0:-1]

phis <- pnorm(intervals$breaks)

p_i <- c()
for (i in 1:(length(phis)-1)) {
  print(phis[i + 1] - phis[i])
  p_i[i] <- phis[i + 1] - phis[i]
}

df <- data.frame(interv = breaks, p = p_i, n = intervals$counts, npi = size * p_i, n_i_sub_np = intervals$counts - size * p_i, frac = (intervals$counts - size * p_i) ** 2 / (size * p_i))

print(sum(df$p))
print(sum(df$n))
print(sum(df$np))
print(sum(df$n_i_sub_np))
print(sum(df$frac))

# sum should be less than 18.31

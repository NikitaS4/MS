#includes 
library(stringr)
library(LaplacesDemon)
library(moments)

size <- 20
size <- 100

dataset <- rnorm(size)

print(str_c("m = ", toString(mean(dataset))))
print(str_c("s = ", toString(var(dataset))))

gamma <- 0.95
alpha <- 1 - gamma
quantile_number <- 1 - alpha / 2

stud_quant <- qst(quantile_number, nu = size - 1)

xmean <- mean(dataset)
xvar <- var(dataset)

print(str_c("Size = ", toString(size), " Mean = ", xmean, " Var = ", xvar))

# Mean

left_bord <- xmean - (xvar * stud_quant) / sqrt(size - 1)
right_bord <- xmean + (xvar * stud_quant) / sqrt(size - 1)

print(str_c("Size = ", toString(size), " , Mean:"))
print(str_c("Left = ", toString(left_bord), " || Right = ", toString(right_bord)))

# Var

chisq_quant_left <- qchisq(quantile_number, size - 1)
chisq_quant_right <- qchisq(alpha / 2, size - 1)

left_bord <- xvar * sqrt(size) / sqrt(chisq_quant_left)
right_bord <- xvar * sqrt(size) / sqrt(chisq_quant_right)

print(str_c("Var:"))
print(str_c("Left = ", toString(left_bord), " || Right = ", toString(right_bord)))


# Asymptotic method

n_quant <- qnorm(quantile_number)

# Mean

left_bord <- xmean - (xvar * n_quant) / sqrt(size)
right_bord <- xmean + (xvar * n_quant) / sqrt(size)

print(str_c("Size = ", toString(size), " , Mean:"))
print(str_c("Left = ", toString(left_bord), " || Right = ", toString(right_bord)))

# Var

kurt <- kurtosis(dataset) 

left_bord <- xvar*(1 - 0.5*n_quant*sqrt(kurt + 2)/sqrt(size))
right_bord <- xvar*(1 + 0.5*n_quant*sqrt(kurt + 2)/sqrt(size))

print(str_c("Var:"))
print(str_c("Left = ", toString(left_bord), " || Right = ", toString(right_bord)))

boxplot(dataset)

devtools::install_github("https://github.com/ivanuricardo/TensorEconometrics")
library(TensorEconometrics)
library(rTensor)
library(MultiwayRegression)
set.seed(20230501)
data("tensor_data")

# We fit a linear model. Matrix autoregressive model with 1 lag
tensor_lag <- tensor_data[1:160, , ] + array(rnorm(25600, sd = 1e-05), dim = c(160, 32, 5))
tensor_levels <- tensor_data[2:161, , ] + array(rnorm(25600, sd = 1e-05), dim = c(160, 32, 5))

# CP regression with R = 5 and no regularization
cp_regression <- rrr(tensor_lag, tensor_levels, R = 5)

# CP regression with R= 5 and regularization
cp_ridge_reg <- rrr(tensor_lag, tensor_levels, R = 5, lambda = 0.01)

sse_list <- list(NULL)
for (i in 1:10) {
  sim_regression <- rrr(tensor_lag, tensor_levels, R = i)
  sse_list[i] <- sim_regression$sse
}

mean_sse <- mean(unlist(sse_list))
std_sse <- (1/mean_sse)*unlist(sse_list)
 
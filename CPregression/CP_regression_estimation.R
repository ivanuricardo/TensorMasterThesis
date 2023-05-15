devtools::install_github("https://github.com/ivanuricardo/TensorEconometrics")
library(TensorEconometrics)
library(rTensor)
library(MultiwayRegression)
library(dplyr)
set.seed(20230501)

# Demean the data
tensor_means <- apply(tensor_data, MARGIN = c(2,3), mean)
array_means <- array(tensor_means, dim = c(32,3,161)) %>% 
  aperm(c(3,1,2))
tensor_data <- as.tensor(tensor_data - array_means)

# We fit a linear model. Matrix autoregressive model with 1 lag
tensor_predictor <- tensor_data[1:160, , ] 
tensor_response <- tensor_data[2:161, , ] 

HOOLS_est <- HOOLS(tensor_response, tensor_predictor, 1, 1)

# What I could do is see what the optimal CP rank is for the parameter
# tensor then fit the rrr function based on that
cp_rank_selection(HOOLS_est, 30)

# 5 seems like a nice rank to fit

# CP regression with R = 5 and no regularization
cp_regression <- rrr(tensor_predictor@data, tensor_response@data, R = 5)

cp_regression$U

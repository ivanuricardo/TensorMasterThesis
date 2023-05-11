# Testing HOOLS vs VAR
library(TensorEconometrics)
library(dplyr)

var_ols <- function(A){
  dimensions_A <- dim(A)
  original_A <- A[-1,]
  lagged_A <- A[1:(dimensions_A[1]-1),]
  ols_hat <- solve(t(lagged_A)%*%lagged_A) %*% (t(lagged_A)%*%original_A)
  return(t(ols_hat))
}

data("traditional_data")
data("tensor_data")

# Demean both data sets
tensor_means <- apply(tensor_data, MARGIN = c(2,3), mean)
array_means <- array(tensor_means, dim = c(32,3,161)) %>% 
  aperm(c(3,1,2))
demeaned_tensor_data <- as.tensor(tensor_data - array_means)

demeaned_traditional_data <- scale(traditional_data, center = TRUE, scale = FALSE)

# Estimate VAR
var_parameters <- var_ols(demeaned_traditional_data)

# Estimate HOOLS
predictor_tensor <- demeaned_tensor_data@data[1:160,,]
response_tensor <- demeaned_tensor_data@data[2:161,,]
HOOLS_parameters <- HOOLS(as.tensor(response_tensor), as.tensor(predictor_tensor), 1, 1)

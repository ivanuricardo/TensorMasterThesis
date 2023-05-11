# Testing HOOLS vs VAR
library(TensorEconometrics)
library(dplyr)
library(vars)

var_ols <- function(A){
  dimensions_A <- dim(A)
  response <- A[-1,]
  predictor <- A[1:(dimensions_A[1]-1),]
  ols_hat <- solve(t(predictor)%*%predictor) %*% (t(predictor)%*%response)
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

# Compare HOOLS and VAR
# The first matrix of HOOLS parameter should line up with the first row of the VAR parameter
# Note I don't use the unfold function, but rather the matrix function in order to obtain the correct
# parameters.

HOOLS_parameters@data[,,1,1] # Corresponds to the first row of the VAR parameter, [,,1,2] corresponds 
# to the second row

unfolded_HOOLS <- unfold(HOOLS_parameters, c(4,3), c(2,1))@data

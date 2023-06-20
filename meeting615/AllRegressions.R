library(TensorEconometrics)
library(dplyr)
library(MultiwayRegression)
set.seed(20230610)

# Demean the data
cp_means <- apply(tensor_data, MARGIN = c(2,3), mean)
cp_array_means <- array(cp_means, dim = c(32,3,161)) %>% 
  aperm(c(3,1,2))
cp_data <- as.tensor(tensor_data - cp_array_means)

# HOOLS estimation. Line by line ordinary least squares
cp_predictor <- cp_data[1:160, , ] 
cp_response <- cp_data[2:161, , ] 

HOOLS_est <- HOOLS(cp_response, cp_predictor, 1, 1)

####################

# CP regression estimation
cp_reg <- cp_regression(cp_response, cp_predictor, R = 6, obs_dim_X = 1, 
                        obs_dim_Y = 1, convThresh = 1e-03, max_iter = 2000)

####################

# rrr estimation
rrr_reg <- rrr(cp_predictor@data, cp_response@data, R = 8)

# Need to check if identification restrictions are applied

####################

rev_tensor <- aperm(tensor_data, c(2,3,1))

tensor_means <- apply(rev_tensor, MARGIN = c(1,2), mean)
array_means <- array(tensor_means, dim = c(32,3,161))
demeaned_tensor_data <- as.tensor(rev_tensor - array_means)

predictor_tensor <- demeaned_tensor_data@data[,,1:160]
response_tensor <- demeaned_tensor_data@data[,,2:161]

tucker_est <- tucker_regression(X=cp_predictor,
                                Y=cp_response, R=c(2,3,2,3),
                                convThresh = 1e-06)

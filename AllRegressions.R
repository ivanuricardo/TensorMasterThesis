library(TensorEconometrics)
library(dplyr)
set.seed(20230610)

# Demean the data
cp_means <- apply(tensor_data, MARGIN = c(2,3), mean)
cp_array_means <- array(cp_means, dim = c(32,3,161)) %>% 
  aperm(c(3,1,2))
cp_data <- as.tensor(tensor_data - cp_array_means)

# HOOLS estimation. Line by line ordinary least squares
cp_predictor <- tensor_data[1:160, , ] 
cp_response <- tensor_data[2:161, , ] 

HOOLS_est <- HOOLS(cp_response, cp_predictor, 1, 1)

####################

# CP regression estimation
cp_reg <- cp_regression(cp_response, cp_predictor, R = 8,
                        obs_dim_X = 1, obs_dim_Y = 1)
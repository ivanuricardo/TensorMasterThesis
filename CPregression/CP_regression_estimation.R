devtools::install_github("https://github.com/ivanuricardo/TensorEconometrics")
library(TensorEconometrics)
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

# 6 seems like a nice rank to fit

# CP regression with R = 6 and no regularization
rrr_regression <- rrr(tensor_predictor@data, tensor_response@data, R = 20,
                      seed = 20230501)
saveRDS(rrr_regression, "rrr_output.rds")

cp_reg <- cp_regression(tensor_response, tensor_predictor, R = 20,
                        obs_dim_X = 1, obs_dim_Y = 1, seed = 20230501)
saveRDS(cp_reg, "cp_reg_output.rds")

HOOLS_est@data - rrr_regression$B

rrr_regression$B - cp_reg$B

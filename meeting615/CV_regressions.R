# Cross Validation
library(TensorEconometrics)
library(MultiwayRegression)
library(dplyr)
library(foreach)
library(doParallel)
set.seed(20230614)
data(tensor_data)

RMSE <- function(x, n) sqrt(sum(x*x)/n)

# Demean both data sets
tensor_means <- apply(tensor_data, MARGIN = c(2,3), mean)
array_means <- array(tensor_means, dim = c(32,3,161)) %>% 
  aperm(c(3,1,2))
demeaned_tensor_data <- tensor_data - array_means

## First a baseline
iters <- round(0.3*length(tensor_data[,1,1]))
R_matrix <- matrix(nrow = iters, ncol = 50)
for (i in 1:7) {
  fnorm_err <- c()
  for (j in 1:iters) {
    # Split into testing and training
    train_tensor <- as.tensor(demeaned_tensor_data[j:(112+j),,])
    test_tensor <- as.tensor(demeaned_tensor_data[(113+j),,])
    
    # Obtain predictors and responses from training data
    predictor_train <- train_tensor[1:112,,]
    response_train <- train_tensor[2:113,,]
    
    # Estimate CP on predictors and responses
    hools_est <- HOOLS(response_train, predictor_train, 1, 1)
    
    # Estimate one step ahead and compare to test
    estimate <- ttt(train_tensor[113,,], hools_est, alongA = c(1,2),
                    alongB = c(1,2))
    fnorm_err <- append(fnorm_err, RMSE(estimate@data-test_tensor@data, 113))
  }
  R_matrix[,i] <- fnorm_err
}
saveRDS(R_matrix, "HOOLS_rw.rds")

#############################################

# CP Regression

# Set the number of cores
num_cores <- 12
for (i in 1:2) {
  # Register the parallel backend
  cl <- makeCluster(num_cores)
  registerDoParallel(cl)
  results <- c()
  
  # Parallelize the inner loop
  foreach(j = 1:iters, .packages = c("TensorEconometrics")) %dopar% {
    fnorm_err <- c()
    
    # Split into testing and training
    train_tensor <- as.tensor(demeaned_tensor_data[j:(112+j), , ])
    test_tensor <- as.tensor(demeaned_tensor_data[(113+j), , ])
    
    # Obtain predictors and responses from training data
    predictor_train <- train_tensor[1:112, , ]
    response_train <- train_tensor[2:113, , ]
    
    # Estimate CP on predictors and responses
    cp_est <- cp_regression(response_train, predictor_train, R = (i+1), 1, 1)
    
    # Estimate one step ahead and compare to test
    estimate <- ttt(train_tensor[113, , ], cp_est$B, alongA = c(1,2), alongB = c(1,2))
    fnorm_err <- append(fnorm_err, RMSE(estimate@data - test_tensor@data, 113))
    
    # Return the result
    fnorm_err
  } -> results
  
  # Close the parallel backend
  stopCluster(cl)
  registerDoSEQ()  # Reset to sequential processing
  
  # Combine the results into the matrix
  R_matrix[,i] <- unlist(results)
  print(i)
}

saveRDS(R_matrix, file = "CP_rw.rds")

###################################################################

## Now for rrr regression as in Locke (2017)
num_cores <- 12
for (i in 1:7) {
  # Register the parallel backend
  cl <- makeCluster(num_cores)
  registerDoParallel(cl)
  results <- c()
  
  # Parallelize the inner loop
  foreach(j = 1:iters, .packages = c("TensorEconometrics", "MultiwayRegression")) %dopar% {
    fnorm_err <- c()
    
    # Split into testing and training
    train_tensor <- as.tensor(demeaned_tensor_data[j:(112+j), , ])
    test_tensor <- as.tensor(demeaned_tensor_data[(113+j), , ])
    
    # Obtain predictors and responses from training data
    predictor_train <- train_tensor[1:112, , ]
    response_train <- train_tensor[2:113, , ]
    
    # Estimate CP on predictors and responses
    rrr_est <- rrr(predictor_train@data, response_train@data, R = (i+1))
    
    # Estimate one step ahead and compare to test
    estimate <- ttt(train_tensor[113, , ], as.tensor(rrr_est$B), alongA = c(1,2), alongB = c(1,2))
    fnorm_err <- append(fnorm_err, RMSE(estimate@data - test_tensor@data, 113))
    
    # Return the result
    fnorm_err
  } -> results
  
  # Close the parallel backend
  stopCluster(cl)
  registerDoSEQ()  # Reset to sequential processing
  
  # Combine the results into the matrix
  R_matrix[,i] <- unlist(results)
  print(i)
}

for (i in 1:7) {
  fnorm_err <- c()
  for (j in 1:iters) {
    train_tensor <- as.tensor(demeaned_tensor_data[j:(112+j),,])
    test_tensor <- as.tensor(demeaned_tensor_data[(113+j),,])
    
    # Obtain predictors and responses from training data
    predictor_train <- train_tensor[1:112,,]
    response_train <- train_tensor[2:113,,]
    
    # Estimate CP on predictors and responses
    rrr_est <- rrr(predictor_train@data, response_train@data, R = (i+1))
    
    # Estimate one step ahead and compare to test
    estimate <- ttt(train_tensor[113,,], as.tensor(rrr_est$B), alongA = c(1,2),
                    alongB = c(1,2))
    fnorm_err <- append(fnorm_err, RMSE(estimate-test_tensor, 113))
  }
  R_matrix[,i] <- fnorm_err
}

saveRDS(R_matrix, "rrr_rw.rds")

##################################################################

## Now Tucker Regression
for (i in 1:7) {
  fnorm_err <- c()
  for (j in 1:iters) {
    train_tensor <- as.tensor(demeaned_tensor_data[j:(112+j),,])
    test_tensor <- as.tensor(demeaned_tensor_data[(113+j),,])
    
    # Obtain predictors and responses from training data
    predictor_train <- train_tensor[1:112,,]
    response_train <- train_tensor[2:113,,]
    # Estimate CP on predictors and responses
    tucker_est <- tucker_regression(predictor_train, response_train,
                                    R = c((i+1), 3, (i+1), 3), max_iter = 1200,
                                    init_val = 0)
    
    # Estimate one step ahead and compare to test
    estimate <- ttt(train_tensor[113,,], tucker_est$B, alongA = c(1,2),
                    alongB = c(1,2))
    fnorm_err <- append(fnorm_err, RMSE(estimate@data-test_tensor@data, 113))
    print(j)
  }
  R_matrix[,i] <- fnorm_err
}

saveRDS(R_matrix, "tucker_rw.rds")

#######################################################################

# Tucker Regression with different values of R
num_cores <- 14
count_idx <- 0
for (k in 1:7) {
  for (i in 1:7) {
    cl <- makeCluster(num_cores)
    registerDoParallel(cl)
    results <- c()
    count_idx <- count_idx+1
    fnorm_err <- c()
    foreach(j = 1:iters, .packages = c("TensorEconometrics")) %dopar% {
      fnorm_err <- c()
    
      train_tensor <- as.tensor(demeaned_tensor_data[j:(112+j),,])
      test_tensor <- as.tensor(demeaned_tensor_data[(113+j),,])
      
      # Obtain predictors and responses from training data
      predictor_train <- train_tensor[1:112,,]
      response_train <- train_tensor[2:113,,]
      
      # Estimate CP on predictors and responses
      tucker_est <- tucker_regression(predictor_train, response_train,
                                      R = c((k), 3, (i), 3), max_iter = 1000,
                                      )
      
      # Estimate one step ahead and compare to test
      estimate <- ttt(train_tensor[113,,], tucker_est$B, alongA = c(1,2),
                      alongB = c(1,2))
      fnorm_err <- append(fnorm_err, RMSE(estimate@data-test_tensor@data, 1))
      fnorm_err
    } -> results
    # Close the parallel backend
    stopCluster(cl)
    registerDoSEQ()  # Reset to sequential processing
    
    # Combine the results into the matrix
    R_matrix[,count_idx] <- unlist(results)
    print(count_idx)
  }
}

saveRDS(R_matrix, "TuckerAltIdx.rds")

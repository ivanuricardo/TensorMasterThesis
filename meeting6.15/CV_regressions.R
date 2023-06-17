# Cross Validation
library(TensorEconometrics)
library(MultiwayRegression)
library(dplyr)
set.seed(20230614)
data(tensor_data)

# Demean both data sets
tensor_means <- apply(tensor_data, MARGIN = c(2,3), mean)
array_means <- array(tensor_means, dim = c(32,3,161)) %>% 
  aperm(c(3,1,2))
demeaned_tensor_data <- tensor_data - array_means

## First a baseline
iters <- round(0.3*length(tensor_data[,1,1]))
R_matrix <- matrix(nrow = iters, ncol = 25)
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
    fnorm_err <- append(fnorm_err, fnorm(estimate-test_tensor))
    print(j)
  }
  R_matrix[,i] <- fnorm_err
}
saveRDS(R_matrix, "HOOLS_rw.rds")

# RMSFE for HOOLS (no choice of R) is 0.4412856

# How many iterations?
iters <- round(0.3*length(demeaned_tensor_data[,1,1]))

R_matrix <- matrix(nrow = iters, ncol = 7)
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
    cp_est <- cp_regression(response_train, predictor_train, R = (i+1), 1, 1)
    
    # Estimate one step ahead and compare to test
    estimate <- ttt(train_tensor[113,,], cp_est$B, alongA = c(1,2),
                    alongB = c(1,2))
    fnorm_err <- append(fnorm_err, fnorm(estimate-test_tensor))
    print(j)
  }
  R_matrix[,i] <- fnorm_err
}

saveRDS(R_matrix, file = "CP_rw.rds")

line_colors <- rainbow(7)
matplot(R_matrix, type = "l", col = line_colors, lty = 1)
legend("topright", legend = 1:7, col = line_colors, lty = 1, bty = "n")

colMeans(R_matrix)
# R = 2 has the lowest RMSE at 0.17, with R=3 being second lowest at 0.189. Then 
# R = 6 at 0.20555

## Now for rrr regression as in Locke (2017)
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
    fnorm_err <- append(fnorm_err, fnorm(estimate-test_tensor))
    print(j)
  }
  R_matrix[,i] <- fnorm_err
}

saveRDS(R_matrix, "rrr_rw.rds")
# Very similar to mine. the first error is the smallest at 0.18501 followed
# by the second error at 0.21158. The third is the third largest at 0.2124407.

## Now Tucker Regression
tensor_means <- apply(tensor_data, MARGIN = c(2,3), mean)
array_means <- array(tensor_means, dim = c(32,3,161)) %>% 
  aperm(c(3,1,2))
demeaned_tensor_data <- tensor_data - array_means

## First a baseline
iters <- round(0.3*length(tensor_data[,1,1]))
R_matrix <- matrix(nrow = iters, ncol = 7)
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
                                    R = c((i+1), 3, (i+1), 3), max_iter = 1000)
    
    # Estimate one step ahead and compare to test
    estimate <- ttt(train_tensor[113,,], tucker_est$B, alongA = c(1,2),
                    alongB = c(1,2))
    fnorm_err <- append(fnorm_err, fnorm(estimate-test_tensor))
    print(j)
  }
  R_matrix[,i] <- fnorm_err
}

saveRDS(R_matrix, "tucker_rw.rds")

# Tucker Regression with different values of R
tensor_means <- apply(tensor_data, MARGIN = c(2,3), mean)
array_means <- array(tensor_means, dim = c(32,3,161)) %>% 
  aperm(c(3,1,2))
demeaned_tensor_data <- tensor_data - array_means

## First a baseline
iters <- round(0.3*length(tensor_data[,1,1]))
R_matrix <- matrix(nrow = iters, ncol = 7)
for (k in 1:5) {
  for (i in 1:5) {
    fnorm_err <- c()
    for (j in 1:iters) {
      train_tensor <- as.tensor(demeaned_tensor_data[j:(112+j),,])
      test_tensor <- as.tensor(demeaned_tensor_data[(113+j),,])
      
      # Obtain predictors and responses from training data
      predictor_train <- train_tensor[1:112,,]
      response_train <- train_tensor[2:113,,]
      
      # Estimate CP on predictors and responses
      tucker_est <- tucker_regression(predictor_train, response_train,
                                      R = c((k+1), 3, (i+1), 3), max_iter = 1000)
      
      # Estimate one step ahead and compare to test
      estimate <- ttt(train_tensor[113,,], tucker_est$B, alongA = c(1,2),
                      alongB = c(1,2))
      fnorm_err <- append(fnorm_err, fnorm(estimate-test_tensor))
      print(j)
    }
  R_matrix[,i] <- fnorm_err
  }
}

saveRDS(R_matrix, "TuckerAltIdx.rds")
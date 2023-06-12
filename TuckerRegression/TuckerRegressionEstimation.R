library(TensorEconometrics)
library(dplyr)
set.seed(20230610)
data("tensor_data")

rev_tensor <- aperm(tensor_data, c(2,3,1))

tensor_means <- apply(rev_tensor, MARGIN = c(1,2), mean)
array_means <- array(tensor_means, dim = c(32,3,161))
demeaned_tensor_data <- as.tensor(rev_tensor - array_means)

predictor_tensor <- demeaned_tensor_data@data[,,1:160]
response_tensor <- demeaned_tensor_data@data[,,2:161]

tucker_est <- tucker_regression(X=as.tensor(predictor_tensor),
                                Y=as.tensor(response_tensor), R=c(10,3,10,3),
                                convThresh = 1e-06)

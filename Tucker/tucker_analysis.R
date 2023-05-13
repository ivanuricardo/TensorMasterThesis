library(TensorEconometrics)
library(tensorTS)
library(dplyr)

set.seed(20230507)

data("tensor_data")
data("traditional_data")

# Demean the data
tensor_means <- apply(tensor_data, MARGIN = c(2,3), mean)
array_means <- array(tensor_means, dim = c(32,3,161)) %>% 
  aperm(c(3,1,2))
tensor_data <- as.tensor(tensor_data - array_means)

rearrange_idx <- c(1, 22, 8, 5, 18, 32, 6, 7, 15, 16, 24, 12, 25, 13, 17, 23,
                   29, 2, 21, 11, 30, 3, 9, 20, 27, 19,4, 28, 31, 14, 10, 26)
rearranged_tensor <- tensor_data[,rearrange_idx,]

# Determine optimal tucker ranks
tucker_rank_selection(tensor_data, 0.55)
# Interesting to note - only when the ridge lambda is changed from 0.002 to 0.001 
# is when we see a change in the tucker ranks. It goes from 1 time dimension to 95
# time dimensions

log(tensor_data@modes[1])/(10 * tensor_data@modes[1])

# Wang uses a different formula, namely
sqrt(32*3*log(161)/(10*161))

# We get the same results

# Most important note: there are three main countries and one main economic factor


tucker_rank_selection(rearranged_tensor)

# Decompose tensor into tucker portions
tucker_tensor <- tucker(tensor_data, ranks = c(6, 3, 2))
norm_tucker <- tucker(as.tensor(norm_tensor), ranks = c(109, 2, 4))
rearranged_tucker <- tucker(as.tensor(rearranged_tensor), ranks = c(109, 1, 4))

tucker_time <- ttm(tucker_tensor$Z, tucker_tensor$U[[1]], m = 1)
ts.plot(tucker_time@data[,3,2])

norm_time <- ttm(norm_tucker$Z, norm_tucker$U[[1]], m = 1)
ts.plot(norm_time@data[,2,1])

rearranged_time <- ttm(rearranged_tucker$Z, rearranged_tucker$U[[1]], m = 1)
ts.plot(rearranged_time@data[,1,4])



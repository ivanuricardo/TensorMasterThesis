library(TensorEconometrics)
library(tensorTS)
library(dplyr)

set.seed(20230507)

data("tensor_data")
data("traditional_data")

# Prepare data for different cases
tensor_data <- as.tensor(tensor_data)
norm_tensor <- traditional_data %>% 
  scale(center = FALSE) %>% 
  as.matrix(byrow = TRUE) %>% 
  array(dim = c(161, 5, 32)) %>% 
  aperm(c(1, 3, 2))
rearrange_idx <- c(1, 22, 8, 5, 18, 32, 6, 7, 15, 16, 24, 12, 25, 13, 17, 23,
                   29, 2, 21, 11, 30, 3, 9, 20, 27, 19,4, 28, 31, 14, 10, 26)
rearranged_tensor <- norm_tensor[,rearrange_idx,]

# Determine optimal tucker ranks
tucker_rank_selection(tensor_data)
tucker_rank_selection(as.tensor(norm_tensor))
tucker_rank_selection(as.tensor(rearranged_tensor))

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



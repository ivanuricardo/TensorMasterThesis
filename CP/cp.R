library(TensorEconometrics)
library(tensorTS)
library(dplyr)

set.seed(20230502)
# Load data
data("tensor_data")

# Demean the data
tensor_means <- apply(tensor_data, MARGIN = c(2,3), mean)
array_means <- array(tensor_means, dim = c(32,3,161)) %>% 
  aperm(c(3,1,2))
tensor_data <- as.tensor(tensor_data - array_means)

econ_names <- c("y", "Dp", "r", "ys", "Dps")
country_names <- c("Argentina", "Australia", "Austria", "Belgium", "Brazil", 
                "Canada", "China", "Chile", "Finland", "France", "Germany",
                "India", "Indonesia", "Italy", "Japan", "Korea", "Malaysia",
                "Mexico", "Netherlands", "Norway", "New Zealand", "Peru",
                "Philippines", "South Africa", "Singapore", "Spain", "Sweden",
                "Switzerland", "Thailand", "Turkey", "United Kingdom",
                "United States")

# Visualize time series
mplot(tensor_data[, 1:5, ])

# Determine optimal CP rank
rank_selection <- cp_rank_selection(tensor_data, 20)

# Perform CP decomposition with 6 components
cp_est <- cp(tensor_data, num_components = 6)

# Extract factor matrices
A <- cp_est$U[[1]]
B <- cp_est$U[[2]]
C <- cp_est$U[[3]]

# Is this unique?
cp_uniqueness(cp_est)

# Find index of maximum value in first factor of A
series <- 1
ts.plot(A[,series])
peak_time <- which.max(abs(A[, series]))

# Determine which economic variable the first - sixth principal component of A
# contributes the most to
econ_significance <- C %*% diag(cp_est$lambdas)
econ_var <- apply(abs(econ_significance), 2, which.max)

# Interpret economic variables
econ_names[econ_var] 

# Plot of data to see similarities
ts.plot(traditional_data[, 2])

# Determine which country the first, second, and sixth principal components of 
# B contribute the most to
country_significance <- B %*% diag(cp_est$lambdas)
country_var <- apply(abs(country_significance), 2, which.max)

# Interpret country variables
country_names[country_var]

# The PCs influence the Argentina (1) variables the most, interest rates and
# inflation. Also affect Brazil (5) and Peru (22). Refer to MAR plots for
# visualization of this

################################################################

# We can do the same analysis to see whether order matters in the tensor.
# Rearrange the fibers such that continents are clustered together
rearrange_idx <- c(1, 22, 8, 5, 18, 32, 6, 7, 15, 16, 24, 12, 25, 13, 17, 23,
                   29, 2, 21, 11, 30, 3, 9, 20, 27, 19,4, 28, 31, 14, 10, 26)
rearranged_tensor <- tensor_data@data[,rearrange_idx,]

rearranged_rank <- cp_rank_selection(as.tensor(rearranged_tensor), 25)

# Perform CP decomposition with 15 components
rearranged_cp <- cp(as.tensor(rearranged_tensor), num_components = 15)

# Extract factor matrices
rearranged_A <- rearranged_cp$U[[1]]
rearranged_B <- rearranged_cp$U[[2]]
rearranged_C <- rearranged_cp$U[[3]]

# Find index of maximum value in first factor of A
peak_time <- which.max(rearranged_A[, 1]) 

# Find index of maximum value in first factor of A
series <- 5
ts.plot(rearranged_A[,series])
peak_time <- which.max(abs(rearranged_A[, series]))

# Determine which economic variable the first - sixth principal component of A
# contributes the most to
rearranged_econ_sig <- rearranged_C %*% diag(rearranged_cp$lambdas)
rearranged_econ_var <- apply(abs(rearranged_econ_sig), 2, which.max)
econ_names[rearranged_econ_var]

# Determine which country the first, second, and sixth principal components of B
# contribute the most to
rearranged_country_sig <- rearranged_B %*% diag(rearranged_cp$lambdas)
rearranged_country_var <- apply(abs(rearranged_country_sig), 2, which.max)
country_names[rearranged_country_var]

ts.plot(rearranged_A[, 1])

# Lesson: order matters!

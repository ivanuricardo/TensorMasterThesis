library(TensorEconometrics)
library(tensorTS)

set.seed(20230502)

# Load data
data("tensor_data")
data("traditional_data")

# Convert tensor data to tensor object
tensor_data <- as.tensor(tensor_data)

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

# Find index of maximum value in first factor of A
which.max(A[, 1]) # Peak occurs around time point 39

# Determine which economic variable the first - sixth principal component of A
# contributes the most to
econ_significance <- C %*% diag(cp_est$lambdas)
econ_var <- apply(abs(econ_significance), 2, which.max)

# Interpret economic variables
names(traditional_data)[econ_var] 

# Plot of data to see similarities
ts.plot(traditional_data[, 2])

# Determine which country the first, second, and sixth principal components of B
# contribute the most to
country_significance <- B %*% diag(cp_est$lambdas)
country_var <- apply(abs(country_significance), 2, which.max)

# Interpret country variables
names(traditional_data)[country_var]

# The PCs influence the Argentina (1) variables the most, interest rates and
# inflation. Also affect Brazil (5) and Peru (22). Refer to MAR plots for
# visualization of this

####################################################################

library(dplyr)
# Try the same with normalization
norm_traditional <- traditional_data %>% 
  scale(center = FALSE) %>% 
  as.matrix(byrow = TRUE) %>% 
  array(dim = c(161, 5, 32)) %>% 
  aperm(c(1, 3, 2))

mplot(norm_traditional[, 1:5, ])
norm_rank <- cp_rank_selection(as.tensor(norm_traditional), 20)

# Perform CP decomposition with 13 components
cp_est <- cp(as.tensor(norm_traditional), num_components = 13)

# Extract factor matrices
A <- cp_est$U[[1]]
B <- cp_est$U[[2]]
C <- cp_est$U[[3]]

# Find index of maximum value in first factor of A
which.max(A[, 1]) # Peak occurs around time point 117, now closer to recession

# Determine which economic variable the first - sixth principal component of A
# contributes the most to
econ_significance <- C %*% diag(cp_est$lambdas)
econ_var <- apply(abs(econ_significance), 2, which.max)

# Interpret economic variables
names(traditional_data)[econ_var] 

# Determine which country the first, second, and sixth principal components of B
# contribute the most to
country_significance <- B %*% diag(cp_est$lambdas)
country_var <- apply(abs(country_significance), 2, which.max)

names(traditional_data)[country_var]

################################################################

# We can do the same analysis to see whether order matters in the tensor.
# Rearrange the fibers such that continents are clustered together

# form groups using graphical lasso
library(qgraph)
corr_countries <- cor_auto((traditional_data[,110:160]+diag(0.000001, nrow = 161)))
glasso_countries <- EBICglasso(corr_countries, nrow(traditional_data), 0,
                               threshold = TRUE)
BICgraph <- qgraph(glasso_countries, layout = "spring", title = "bic",
                   details = TRUE)

rearrange_idx <- c(1, 22, 8, 5, 18, 32, 6, 7, 15, 16, 24, 12, 25, 13, 17, 23,
                   29, 2, 21, 11, 30, 3, 9, 20, 27, 19,4, 28, 31, 14, 10, 26)
rearranged_tensor <- norm_traditional[,rearrange_idx,]

norm_rank <- cp_rank_selection(as.tensor(rearranged_tensor), 20)

# Perform CP decomposition with 13 components
cp_est <- cp(as.tensor(norm_traditional), num_components = 15)

# Extract factor matrices
A <- cp_est$U[[1]]
B <- cp_est$U[[2]]
C <- cp_est$U[[3]]

# Find index of maximum value in first factor of A
which.max(A[, 1]) # Peak occurs around time point 74

# Determine which economic variable the first - sixth principal component of A
# contributes the most to
econ_significance <- C %*% diag(cp_est$lambdas)
econ_var <- apply(abs(econ_significance), 2, which.max)

# Interpret economic variables
names(traditional_data)[econ_var] 

# Determine which country the first, second, and sixth principal components of B
# contribute the most to
country_significance <- B %*% diag(cp_est$lambdas)
country_var <- apply(abs(country_significance), 2, which.max)

names(traditional_data)[country_var]

ts.plot(A[,1])

# Lesson: order matters!





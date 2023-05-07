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

# Try the same with normalization
norm_traditional <- traditional_data %>% 
  scale(center = FALSE) %>% 
  as.matrix(byrow = TRUE) %>% 
  array(dim = c(161, 5, 32)) %>% 
  aperm(c(1, 3, 2))

mplot(norm_traditional[, 1:5, ])
norm_rank <- cp_rank_selection(as.tensor(norm_traditional), 20)

# Perform CP decomposition with 6 components
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

library(TensorEconometrics)
library(tensorTS)
library(dplyr)

# Load data
data("tensor_data")
data("traditional_data")

# normalization potential
norm_tensor <- traditional_data %>% 
  scale(center = FALSE) %>% 
  as.matrix(byrow = TRUE) %>% 
  array(dim = c(161, 5, 32)) %>% 
  aperm(c(1, 3, 2))

# Plot data for first 5 countries. Countries on the rows and econ on the columns
mplot(norm_tensor[, 1:5, ])

# Plot matrix ACF for first 3 countries and 5 economic indicators
mplot.acf(norm_tensor[, 1:3, ])

# Estimate MAR model
matAR1 <- tenAR.est(norm_tensor, R = 1, P = 1, method = "MLE")
# p = 1, r = 1 -0.03206048
# p = 1, r = 2 -0.09496392

# List of parameter matrices
matAR1$A[[1]][[1]]

A <- matAR1$A[[1]][[1]][[1]]
B <- matAR1$A[[1]][[1]][[2]]

# A gives the row-wise interactions - interactions between countries
# Gives essentially a var with different countries holding econ indicator const.

# B gives the column-wise interactions: interactions between the economic indicators
# VAR with different econ indicators
B

library(TensorEconometrics)
library(tensorTS)
library(dplyr)

set.seed(20230507)

data("tensor_data")
data("traditional_data")

country_names <- c("Argentina", "Australia", "Austria", "Belgium", "Brazil", 
                "Canada", "China", "Chile", "Finland", "France", "Germany",
                "India", "Indonesia", "Italy", "Japan", "Korea", "Malaysia",
                "Mexico", "Netherlands", "Norway", "New Zealand", "Peru",
                "Philippines", "South Africa", "Singapore", "Spain", "Sweden",
                "Switzerland", "Thailand", "Turkey", "United Kingdom",
                "United States")

# Demean the data
tensor_means <- apply(tensor_data, MARGIN = c(2,3), mean)
array_means <- array(tensor_means, dim = c(32,3,161)) %>% 
  aperm(c(3,1,2))
tensor_data <- as.tensor(tensor_data - array_means)

rearrange_idx <- c(1, 22, 8, 5, 18, 32, 6, 7, 15, 16, 24, 12, 25, 13, 17, 23,
                   29, 2, 21, 11, 30, 3, 9, 20, 27, 19,4, 28, 31, 14, 10, 26)
rearranged_tensor <- tensor_data[,rearrange_idx,]

# Determine optimal tucker ranks
tucker_rank_selection(tensor_data, 0.001)
# Interesting to note - only when the ridge lambda is changed from 0.002 to 0.001 
# is when we see a change in the tucker ranks. It goes from 1 time dimension to 95
# time dimensions

# Xia Xu Zhu estimate
log(tensor_data@modes[1])/(10 * tensor_data@modes[1])

# Wang uses a different formula, namely
sqrt(32*3*log(161)/(10*161))

# We get the same results

# Most important note: there are three main countries and one main economic factor

tucker_rank_selection(rearranged_tensor)

# Decompose tensor into tucker portions
tucker_tensor <- tucker(tensor_data, ranks = c(10, 5, 3))
rearranged_tucker <- tucker(rearranged_tensor, ranks = c(6, 3, 3))

tucker_time <- ttm(tucker_tensor$Z, tucker_tensor$U[[1]], m = 1)
ts.plot(tucker_time@data[,3,2])

rearranged_time <- ttm(rearranged_tucker$Z, rearranged_tucker$U[[1]], m = 1)
ts.plot(rearranged_time@data[,3,2])

################################################

# We are interested in the factor structure of the country mode. 
# First, unfold the original data matrix along the country mode
unfolded_tensor <- unfold(tensor_data, 2, c(1,3))

# Multiply G by the modes not corresponding to countries
G1 <- ttm(tucker_tensor$Z, tucker_tensor$U[[1]], m = 1)
G2 <- ttm(G1, tucker_tensor$U[[3]], m = 3)

# Unfold the resulting tensor along the second dimension
unfolded_G <- unfold(G2, 2, c(1,3))

# The two dimensional factor model with 5 factors is then obtained by regressing 
# the unfolded_G by the unfolded tensor
# This is the same as having 32 responses and 5 predictors, the predictors being
# the factors
country_factor_model <- lm(t(unfolded_tensor@data) ~ t(unfolded_G@data))
country_sum <- summary(country_factor_model) 

# This is 32 equations, each with 5 factors explaining them.
# Can we attribute the factors to the countries?
country_r2 <- lapply(country_sum, function (x) x$r.squared)
country_idx <- which(country_r2 > 0.7)
country_names[country_idx]

# We get that the 5 factors affect Argentina, Brazil, Mexico, Peru, and Turkey 
# the most


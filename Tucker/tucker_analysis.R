library(TensorEconometrics)
library(tensorTS)
library(dplyr)
library(purrr)
library(ggplot2)
library(tidyr)

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

# Determine optimal tucker ranks
tucker_rank_selection(tensor_data, 0.002)
# Interesting to note - only when the ridge lambda is changed from 0.002 to 0.001 
# is when we see a change in the tucker ranks. It goes from 1 time dimension to 95
# time dimensions

# Xia Xu Zhu estimate
log(tensor_data@modes[1])/(10 * tensor_data@modes[1])

# Wang uses a different formula, namely
sqrt(32*3*log(161)/(10*161))

# We get the same results

# Most important note: there are three main countries and one main economic factor

############################################
# Can do some additional diagnostics on the ranks by plotting the fnorm with different
# tucker ranks

# Call the time dimension K1, country dimension K2, and econ dimension K3
# For K1, we save fnorm while keeping constant (K1, 1, 1), (K1, 3, 1), (K1, 8, 2)
# and (K1, 15, 3)

kc <- list(
  expand.grid(5, 1:32, 1),
  expand.grid(10, 1:32, 2),
  expand.grid(15, 1:32, 2),
  expand.grid(20, 1:32, 3)
)

country_dfs <- lapply(kc, function(k) apply(k, MARGIN = 1,
                                         function(x) tucker(tensor_data, x)$fnorm_resid))

# Combine all data frames into one
country_df <- do.call(cbind, country_dfs) %>% 
  data.frame(country = 1:32) %>% 
  gather(key = "series", value = "value", -country) %>% 
  mutate(
    series = case_when(
      series == "X1" ~ "(5, K2, 1)",
      series == "X2" ~ "(10, K2, 2)",
      series == "X3" ~ "(15, K2, 2)",
      series == "X4" ~ "(20, K2, 3)"
    )
  )

# Create ggplot
ggplot(data = country_df, aes(x = country, y = value, color = series)) + 
  geom_line() + 
  labs(x = "Country Factors", y = "Value", color = "Series")

# I choose 5 country factors

# Define parameter grids
kt <- list(
  expand.grid(1:50, 5, 1),
  expand.grid(1:50, 10, 2),
  expand.grid(1:50, 15, 3),
  expand.grid(1:50, 25, 3)
)

# Apply function for each parameter grid and store in a list
time_dfs <- lapply(kt, function(k) apply(k, MARGIN = 1,
                                         function(x) tucker(tensor_data, x)$fnorm_resid))

# Combine all data frames into one
time_df <- do.call(cbind, time_dfs) %>% 
  data.frame(time = 1:50) %>% 
  gather(key = "series", value = "value", -time) %>% 
  mutate(
    series = case_when(
      series == "X1" ~ "(K1, 5, 1)",
      series == "X2" ~ "(K1, 10, 2)",
      series == "X3" ~ "(K1, 15, 3)",
      series == "X4" ~ "(K1, 25, 3)"
    )
  )

# Create ggplot
ggplot(data = time_df, aes(x = time, y = value, color = series)) + 
  geom_line() + 
  labs(x = "Time Factors", y = "Value", color = "Series")

# Define parameter grids
ke <- list(
  expand.grid(5, 5, 1:3),
  expand.grid(10, 5, 1:3),
  expand.grid(15, 10, 1:3),
  expand.grid(20, 15, 1:3)
)

# Apply function for each parameter grid and store in a list
econ_dfs <- lapply(ke, function(k) apply(k, MARGIN = 1,
                                         function(x) tucker(tensor_data, x)$fnorm_resid))

# Combine all data frames into one
econ_df <- do.call(cbind, econ_dfs) %>% 
  data.frame(econ = 1:3) %>% 
  gather(key = "series", value = "value", -econ) %>% 
  mutate(
    series = case_when(
      series == "X1" ~ "(10, 5, K3)",
      series == "X2" ~ "(10, 10, K3)",
      series == "X3" ~ "(20, 15, K3)",
      series == "X4" ~ "(20, 25, K3)"
    )
  )

# Create ggplot
ggplot(data = econ_df, aes(x = econ, y = value, color = series)) + 
  geom_line() + 
  labs(x = "Econ Factors", y = "Value", color = "Series")

# For completeness, choose 3 econ factors

###################################################

# Decompose tensor into tucker portions
tucker_tensor <- tucker(tensor_data, ranks = c(161, 5, 3))

tucker_time <- ttm(tucker_tensor$Z, tucker_tensor$U[[1]], m = 1)
ts.plot(tucker_time@data[,1,3])

################################################

# We are interested in the factor structure of the country mode. 
# First, unfold the original data matrix along the country mode
unfolded_tensor <- unfold(tensor_data, 1, c(2,3))

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



library(TensorEconometrics)
library(tidyverse)
set.seed(20230706)

data("tensor_data")
countries <- c("ARG", "AUS", "AUT", "BEL", "BRA", "CAN", "CHN", "CHL", "FIN", 
               "FRA", "DEU", "IND", "IDN", "ITA", "JPN", "KOR", "MYS", "MEX", 
               "NLD", "NOR", "NZL", "PER", "PHL", "ZAF", "SGP", "ESP", "SWE",
               "CHE", "THA", "TUR", "GBR", "USA")
econind <- c("y", "Dp", "r")

# Demean both data sets
tensor_means <- apply(tensor_data, MARGIN = c(2,3), mean)
array_means <- array(tensor_means, dim = c(32,3,161)) %>% 
  aperm(c(3,1,2))
demeaned_tensor_data <- as.tensor(tensor_data - array_means)

# Lag split
predictor_tensor <- demeaned_tensor_data@data[1:160,,]
response_tensor <- demeaned_tensor_data@data[2:161,,]
HOOLS_parameters <- HOOLS(as.tensor(response_tensor), as.tensor(predictor_tensor), 1, 1)

# Tucker Rank Selection
tucker_rank_selection(HOOLS_parameters, 160)

# Check for a variety of observations?
for (i in 1:100) {
  print(tucker_rank_selection(HOOLS_parameters, i))
}

# Or for a variety of chosen c?
c_ops <- 0.01*1:99
for (i in c_ops) {
  print(tucker_rank_selection(HOOLS_parameters, c = i, num_obs = 160))
}

# Estimate Tucker Regression using c(7,2,5,2)
tuckerReg <- tucker_regression(as.tensor(response_tensor), as.tensor(predictor_tensor),
                               R = c(7,2,5,2), max_iter = 1000)

# Data frames
f1 <- data.frame(tuckerReg$factors[[2]])
f2 <- data.frame(tuckerReg$factors[[3]])
f3 <- data.frame(tuckerReg$factors[[4]])
f4 <- data.frame(tuckerReg$factors[[5]])

colnames(f1) <- c("F1", "F2", "F3", "F4", "F5", "F6", "F7")
colnames(f3) <- c("F1", "F2", "F3", "F4", "F5")
colnames(f2) <- c("F1", "F2")
colnames(f4) <- c("F1", "F2")
rownames(f1) <- countries
rownames(f3) <- countries
rownames(f2) <- econind
rownames(f4) <- econind

# Define the dataframes
df1 <- f1 %>% 
  rownames_to_column(var = "Country") %>% 
  pivot_longer(cols = -Country, names_to = "Factor", values_to = "Value")

df2 <- f2 %>% 
  rownames_to_column(var = "Econ") %>% 
  pivot_longer(cols = -Econ, names_to = "Factor", values_to = "Value")

df3 <- f3 %>% 
  rownames_to_column(var = "Country") %>% 
  pivot_longer(cols = -Country, names_to = "Factor", values_to = "Value")

df4 <- f4 %>% 
  rownames_to_column(var = "Econ") %>% 
  pivot_longer(cols = -Econ, names_to = "Factor", values_to = "Value")

color_palette <- c("red", "white", "blue")
# Create heatmap for f1
ggplot(df1, aes(Factor, Country, fill = Value)) +
  geom_tile() +
  scale_fill_gradientn(colors = color_palette) +
  labs(title = "Response Variable: Countries", x = "Factors", y = "Countries")

# Create heatmap for f2
ggplot(df2, aes(Factor, Econ, fill = Value)) +
  geom_tile() +
  scale_fill_gradientn(colors = color_palette) +
  labs(title = "Response Variable: Economic Indicators", x = "Factors", y = "Economic Indicators")

# Create heatmap for f3
ggplot(df3, aes(Factor, Country, fill = Value)) +
  geom_tile() +
  scale_fill_gradientn(colors = color_palette) +
  labs(title = "Predictor Variable: Countries", x = "Factors", y = "Countries")

# Create heatmap for f4
ggplot(df4, aes(Factor, Econ, fill = Value)) +
  geom_tile() +
  scale_fill_gradientn(colors = color_palette) +
  labs(title = "Predictor Variable: Economic Indicators", x = "Factors", y = "Economic Indicators")


library(TensorEconometrics)
library(tidyverse)
set.seed(20230706)

data("tensor_data")
countries <- c("AUS", "AUT", "BEL", "CAN", "CHN", "CHL", "FIN", 
               "FRA", "DEU", "IND", "IDN", "ITA", "JPN", "KOR", "MYS", 
               "NLD", "NOR", "NZL", "PHL", "ZAF", "SGP", "ESP", "SWE",
               "CHE", "THA", "GBR", "USA")
econind <- c("y", "Dp", "r")

# Demean both data sets
tensor_means <- apply(tensor_data, MARGIN = c(2,3), mean)
array_means <- array(tensor_means, dim = c(32,3,161)) %>% 
  aperm(c(3,1,2))
demeaned_tensor_data <- as.tensor(tensor_data - array_means)

# Remove Argentina, Brazil, Mexico, Peru, and Turkey
tdata <- demeaned_tensor_data@data[, -c(1, 5, 18, 22, 30), ]

# Lag split
predictor_tensor <- tdata[1:160,,]
response_tensor <- tdata[2:161,,]
HOOLS_parameters <- HOOLS(as.tensor(response_tensor), as.tensor(predictor_tensor), 1, 1)

# Tucker Rank Selection
tucker_rank_selection(HOOLS_parameters, 160)

# Estimate Tucker Regression using c(7,3,5,3)
tuckerReg <- tucker_regression(as.tensor(response_tensor), as.tensor(predictor_tensor),
                               R = c(7,3,5,3), max_iter = 1000)

# Data frames
f1 <- data.frame(tuckerReg$factors[[2]])
f2 <- data.frame(tuckerReg$factors[[3]])
f3 <- data.frame(tuckerReg$factors[[4]])
f4 <- data.frame(tuckerReg$factors[[5]])

colnames(f1) <- paste0("F", seq(1, 7))
colnames(f3) <- paste0("F", seq(1, 5))
colnames(f2) <- paste0("F", seq(1, 3))
colnames(f4) <- paste0("F", seq(1, 3))
rownames(f1) <- countries
rownames(f3) <- countries
rownames(f2) <- econind
rownames(f4) <- econind

# Prepare for plotting
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

color_palette <- c("#006ddb", "white", "#920000")

# Create heatmap for f1
ggplot(df1, aes(Factor, Country, fill = Value)) +
  geom_tile() +
  scale_fill_gradientn(colors = color_palette) +
  labs(title = "Predictor Variables: Country", x = "Factors", y = "Countries")

# Create heatmap for f2
ggplot(df2, aes(Factor, Econ, fill = Value)) +
  geom_tile() +
  scale_fill_gradientn(colors = color_palette) +
  labs(title = "Predictor Variables: Economic Indicator", x = "Factors", y = "Economic Indicators")

# Create heatmap for f3
ggplot(df3, aes(Factor, Country, fill = Value)) +
  geom_tile() +
  scale_fill_gradientn(colors = color_palette) +
  labs(title = "Response Variables: Country", x = "Factors", y = "Countries")

# Create heatmap for f4
ggplot(df4, aes(Factor, Econ, fill = Value)) +
  geom_tile() +
  scale_fill_gradientn(colors = color_palette) +
  labs(title = "Response Variables: Economic Indicator", x = "Factors", y = "Economic Indicators")


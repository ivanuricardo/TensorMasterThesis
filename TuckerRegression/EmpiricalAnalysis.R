library(TensorEconometrics)
library(tidyverse)
set.seed(20230706)

data("tensor_data")
countries <- c("AUS", "AUT", "BEL", "CAN", "CHN", "CHL", "FIN", 
               "FRA", "DEU", "IND", "IDN", "ITA", "JPN", "KOR", "MYS", 
               "NLD", "NOR", "NZL", "PHL", "ZAF", "SGP", "ESP", "SWE",
               "CHE", "THA", "GBR", "USA")
econind <- c("y", "Dp", "r")

loglike=function(data,linearpre){
  sigma_est=mean((data-linearpre)^2)
  L=dnorm(data,linearpre,sqrt(sigma_est),log=TRUE)
  sum(L)##log-likelihood
}

# Demean both data sets
tensor_means <- apply(tensor_data, MARGIN = c(2,3), mean)
array_means <- array(tensor_means, dim = c(32,3,161)) %>% 
  aperm(c(3,1,2))
demeaned_tensor_data <- as.tensor(tensor_data - array_means)

# Remove Argentina, Brazil, Mexico, Peru, and Turkey
tdata <- demeaned_tensor_data@data[, -c(1, 5, 18, 22, 30), ]

# Lag split
predictor_tensor <- as.tensor(tdata[1:160,,])
response_tensor <- as.tensor(tdata[2:161,,])
HOOLS_parameters <- HOOLS(response_tensor, predictor_tensor, 1, 1)

# Tucker Rank Selection
cc <- log(160)/(10*160)
tucker_rank_selection(HOOLS_parameters, c = cc, 169)

it <- 1:999*0.01
for (i in it) {
  tmp <- trs(HOOLS_parameters, c = i, 1)
  print(tmp)
}
trs(HOOLS_parameters, c = 0.001, 160)

# Bayesian Information Criterion
R <- c(1,1,2,2)
t1 <- tucker_regression(response_tensor, predictor_tensor,
                        R = R, max_iter = 1000)
num_params <- prod(R) + sum(R * t1$B@modes) - sum(R^2)
e1 <- response_tensor - ttt(predictor_tensor,
                                         t1$B, alongA = 2:3, alongB = 1:2)
mse <- sum(e1@data * e1@data)/160

BIC <- prod(R) + sum(HOOLS_parameters@modes * R) * log(prod(HOOLS_parameters@modes))
BICoff <- -2 * loglike(response_tensor@data, e1@data) + BIC

# Estimate Tucker Regression using c(7,3,5,3)
tuckerReg <- tucker_regression(response_tensor, predictor_tensor,
                               R = c(1,1,2,2), max_iter = 1000)

# Data frames
f1 <- data.frame(tuckerReg$factors[[2]])
f2 <- data.frame(tuckerReg$factors[[3]])
f3 <- data.frame(tuckerReg$factors[[4]])
f4 <- data.frame(tuckerReg$factors[[5]])

colnames(f1) <- paste0("F", 1)
colnames(f3) <- paste0("F", seq(1, 2))
colnames(f2) <- paste0("F", 1)
colnames(f4) <- paste0("F", seq(1, 2))
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
  geom_text(aes(label = round(Value,4))) +
  scale_fill_gradient2(low = "#006ddb", mid = "white", high = "#920000", midpoint = 0) +
  labs(title = "Predictor Variables: Country", x = "Factors", y = "Countries")

# Create heatmap for f2
ggplot(df2, aes(Factor, Econ, fill = Value)) +
  geom_tile() +
  geom_text(aes(label = round(Value,4))) +
  scale_fill_gradient2(low = "#006ddb", mid = "white", high = "#920000", midpoint = 0) +
  labs(title = "Predictor Variables:\nEconomic Indicator", x = "Factors", y = "Economic Indicators")

# Create heatmap for f3
ggplot(df3, aes(Factor, Country, fill = Value)) +
  geom_tile() +
  geom_text(aes(label = round(Value,4))) +
  scale_fill_gradient2(low = "#006ddb", mid = "white", high = "#920000", midpoint = 0) +
  labs(title = "Response Variables: Country", x = "Factors", y = "Countries")

# Create heatmap for f4
ggplot(df4, aes(Factor, Econ, fill = Value)) +
  geom_tile() +
  geom_text(aes(label = round(Value,4))) +
  scale_fill_gradient2(low = "#006ddb", mid = "white", high = "#920000", midpoint = 0) +
  labs(title = "Response Variables:\nEconomic Indicator", x = "Factors", y = "Economic Indicators")


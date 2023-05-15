library(TensorEconometrics)
library(tensorTS)
library(dplyr)
library(ggplot2)
library(gridExtra)

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
which.max(abs(A[, series]))

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

##############
cp_decomp <- cp(tensor_data, num_components = 1)

U1_data <- data.frame(time = 1:length(cp_decomp$U[[1]][,1]), 
                   values = cp_decomp$U[[1]][,1])

ggplot(U1_data, aes(x = time, y = values)) +
  geom_line() +
  geom_vline(xintercept = 27, linetype = "longdash", color = "blue") +
  geom_vline(xintercept = 48, linetype = "longdash", color = "blue") + 
  geom_vline(xintercept = 61, linetype = "longdash", color = "blue") + 
  annotate("text", x = 20, y = max(U1_data$values), label = "1986Q2",color = "blue") + 
  annotate("text", x = 54, y = max(U1_data$values), label = "1991Q2",color = "blue") + 
  annotate("text", x = 67, y = max(U1_data$values), label = "1994Q1",color = "blue") + 
  ggtitle("Time Factor")

U2_data <- data.frame(time = 1:length(cp_decomp$U[[2]][,1]), 
                   values = cp_decomp$U[[2]][,1])

ggplot(U2_data, aes(x = time, y = values)) +
  geom_line() +
  xlab("Countries") +
  ggtitle("Country Factor") +
  geom_vline(xintercept = 5, linetype = "longdash", color = "blue") + 
  annotate("text", x = 6, y = max(U2_data$values), label = "Brazil", color = "blue") + 
  geom_vline(xintercept = 22, linetype = "longdash", color = "blue") + 
  annotate("text", x = 23, y = max(U2_data$values), label = "Peru", color = "blue")
  
U3_data <- data.frame(value = cp_decomp$U[[3]][,1],
                      label = c("y", "Dp", "r"))

ggplot(U3_data, aes(x = label, y = value, fill = label)) +
  geom_bar(stat = "identity", alpha = 0.6) +
  xlab("Economic Indicator") +
  ylab("Value") +
  ggtitle("Histogram of U3_data") +
  scale_fill_manual(values = c("blue", "purple", "red"), guide = FALSE)
  
# We can see that there are groups between the countries, most notably between
# Brazil, Peru, and Argentina
# These mostly correspond to changes in inflation and interest rate
# This all happens majorly between the time period of 1986Q2 and 1991Q2
  
########################################

# Higher rank approximation via CP decomposition
higher_cp <- cp(tensor_data, num_components = 5, max_iter = 150)

# We check the percentage of the F-norm and it is approximately 75.7%
# We obtain a visualization of all components below

for (j in 1:5) {
  cpU <- data.frame(timeval = 1:nrow(higher_cp$U[[1]]), value = higher_cp$U[[1]][, j])
  p <- ggplot(data = cpU, aes(x = timeval, y = value)) + 
    geom_line() +
    ggtitle(paste0("Time", ", Factor ", j))
  assign(paste0("plot_1", j), p)
}

for (j in 1:5) {
  cpU <- data.frame(timeval = 1:nrow(higher_cp$U[[2]]), value = higher_cp$U[[2]][, j])
  p <- ggplot(data = cpU, aes(x = timeval, y = value)) + 
    geom_line() +
    ggtitle(paste0("Country", ", Factor ", j))
  assign(paste0("plot_2", j), p)
}

for (j in 1:5) {
  cpU <- data.frame(timeval = c("y", "Dp", "r"), value = higher_cp$U[[3]][, j])
  p <- ggplot(data = cpU, aes(x = timeval, y = value, 
                              fill = timeval)) + 
    geom_bar(stat = "identity", alpha = 0.6) + 
    xlab("Economic Indicator") + 
    ylab("Value") + 
    ggtitle(paste0("Economic", ", Factor ", j)) +
    scale_fill_manual(values = c("blue", "purple", "red"), guide = FALSE)
  assign(paste0("plot_3", j), p)
}
# arrange plots in a grid of 5 columns and 3 rows

grid.arrange(plot_11, plot_12, plot_13, plot_14, plot_15,
             plot_21, plot_22, plot_23, plot_24, plot_25,
             plot_31, plot_32, plot_33, plot_34, plot_35, ncol = 5)


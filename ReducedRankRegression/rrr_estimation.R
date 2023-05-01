devtools::install_github("https://github.com/ivanuricardo/TensorEconometrics")
library(TensorEconometrics)
library(rrr)

# Load data
data("traditional_data")

# Response variable is collection of German Economic Variables.
germ_idx <- 51:55
germany_econ <- as.matrix(traditional_data[,germ_idx])
row_econ <- as.matrix(traditional_data[,-germ_idx])

# Preliminary analysis, correlation between predictors and responses
GGally::ggcorr(row_econ[, 1:40])
GGally::ggcorr(row_econ[, 41:80])
GGally::ggcorr(germany_econ)

# Here, we see high correlation between each of predictors and responses

# Estimate the rank for reduction, k is included to make inversion easier
# taken from "multivariate prediction using softly shrunk rrr"

rank_trace(row_econ, germany_econ, k = 1e-04)

# X axis shows difference bn rank-t coefficient and full rank coeff matrix
# Y axis we plot reduction in residual covariance between rank-t residuals and full-rank residuals
# Thus, we see an optimal rank chosen is t = 5

# Fit multivariate regression with rrr()
rrr(row_econ, germany_econ, rank = 3, k = 1e-4)

# Calculate residuals
residuals(row_econ, germany_econ, rank = 3, k = 1e-04)

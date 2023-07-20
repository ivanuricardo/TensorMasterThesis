library(tidyverse)
library(TensorEconometrics)
library(MixMatrix)
library(reshape2)
library(corrplot)
set.seed(20230706)

data("tensor_data")
countries <- c("AUS", "AUT", "BEL", "CAN", "CHN", "CHL", "FIN", 
               "FRA", "DEU", "IND", "IDN", "ITA", "JPN", "KOR", "MYS", 
               "NLD", "NOR", "NZL", "PHL", "ZAF", "SGP", "ESP", "SWE",
               "CHE", "THA", "GBR", "USA")
econind <- c("y", "Dp", "r")

get_lower_tri<-function(x){
  x[upper.tri(x)] <- NA
  return(x)
}

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

# Tucker Regression
tuckerReg <- tucker_regression(as.tensor(response_tensor), as.tensor(predictor_tensor),
                               R = c(1,1,2,2), max_iter = 1000)

# Extract Residuals
Et <- as.tensor(response_tensor) - ttt(as.tensor(predictor_tensor), 
                                       tuckerReg$B, alongA = c(2,3), alongB = c(1,2))
permEt <- aperm(Et@data, c(2,3,1))

MatrixEst <- MLmatrixnorm(permEt)

Uest <- MatrixEst$U
Vest <- MatrixEst$V

colnames(Uest) <- countries
colnames(Vest) <- econind
rownames(Uest) <- countries
rownames(Vest) <- econind

lowerV <- get_lower_tri(Vest)
meltedV <- melt(lowerV, na.rm = TRUE)

lowerU <- get_lower_tri(Uest)
meltedU <- melt(lowerU, na.rm = TRUE)

color_palette <- c("white", "red")

ggplot(data = meltedU, aes(x = Var1, y = Var2, fill = value)) + 
  geom_tile() + 
  scale_fill_gradientn(colors = color_palette) +
  labs(title = "Estimated Covariance: Countries", x = "Countries", y = "Countries")

ggplot(data = meltedV, aes(x = Var1, y = Var2, fill = value)) + 
  geom_tile() + 
  scale_fill_gradientn(colors = color_palette) +
  labs(title = "Estimated Covariance: Econ", x = "Indicator", y = "Indicator")

corrplot(cov2cor(Uest), method = "square", tl.col = "black",
         col = colorRampPalette(c("#006ddb","white","#920000"))(200)) 
corrplot(cov2cor(Vest), method = "square", tl.col = "black",
         col = colorRampPalette(c("#006ddb","white","#920000"))(200))

RSS <- innerProd(Et, Et)
TSS <- innerProd(as.tensor(predictor_tensor), as.tensor(predictor_tensor))
1-(RSS/TSS)

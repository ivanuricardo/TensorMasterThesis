loglike=function(data,linearpre){
  sigma_est=mean((data-linearpre)^2)
  L=dnorm(data,linearpre,sqrt(sigma_est),log=TRUE)
  sum(L)##log-likelihood
}

BIC_est <- function(predictor, response, grid_R) {
  rank <- lapply(1:dim(grid_R)[1], function(x) grid_R[x,]) ## turn rank to a list
  
  estBIC <- c()
  for (i in 1:nrow(grid_R)) {
    tuck <- tucker_regression(response, predictor, rank[[i]])
    ei <- response - ttt(predictor, tuck$B, alongA = 2:3, alongB = 1:2)
    lglk <- prod(rank[[i]]) + sum((tuck$B@modes - rank[[i]]) * rank[[i]]) * 
      log(prod(tuck$B@modes))
    BIC <- -2 * loglike(response@data, ei@data) + lglk
    estBIC <- append(estBIC, BIC)
  }
}

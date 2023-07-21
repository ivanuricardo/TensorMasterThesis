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


trs <- function (tnsr, c = 0, num_obs) 
{
  est_ranks <- NULL
  if (c == 0) {
    c <- sqrt(tnsr@modes[1] * tnsr@modes[2] * log(num_obs)/(10 * 
                                                              num_obs))
  }
  for (mode in 1:tnsr@num_modes) {
    flattened_tnsr <- unfold(tnsr, mode, setdiff((1:tnsr@num_modes), 
                                                 mode))@data
    svd_mode <- svd(flattened_tnsr)$d
    r_mode <- which.max((svd_mode[1:(tnsr@modes[mode] - 1)] + 
                           c)/(svd_mode[2:(tnsr@modes[mode])] + c))
    est_ranks[mode] <- r_mode
  }
  return(est_ranks = est_ranks)
}

tnsr <- rand_tensor(c(6,7,8,9))
tucktnsr <- tucker(tnsr, ranks = c(3,4,5,6))
trs(tucktnsr$est, 160)
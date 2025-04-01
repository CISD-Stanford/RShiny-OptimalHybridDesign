library(stats)
library(rootSolve)
library(MASS)
source("optimalEquivalenceMarginFinder.randomizationRatio")
source("probFinder.randomizationRatio")
source("costFunc.randomizationRatio")

operatingCharacteristics <- function(randomizationRatio, 
                                     N,
                                     RWDRatio,
                                     eqMargin,
                                     effectSize, 
                                     standardDeviation, 
                                     typeIErrorRate, 
                                     typeIErrorRateEQ) {

  w <- RWDRatio / (1 + RWDRatio - randomizationRatio)
  x1_var <- standardDeviation^2 / (N * randomizationRatio * (1 - randomizationRatio))
  x2_var <- standardDeviation^2 * (1 + RWDRatio - randomizationRatio) / (N * RWDRatio * (1 - randomizationRatio))
  x3_var <- standardDeviation^2 * (1 + RWDRatio) / (N * randomizationRatio * (1 + RWDRatio - randomizationRatio))
  cov <- standardDeviation^2 / (N * (1 - randomizationRatio))
  eqMargin <- min(marginTypeIerror, marginPower)
  
  theta <- eqMargin - qnorm(1 - typeIErrorRateEQ / 2) * sqrt(x2_var)
  if (theta < 0) theta <- 0
  
  cutoffValueOutter <- qnorm(1 - typeIErrorRate / 2) * sqrt(x1_var)
  typeIerrorOutter <- probFinder(cutoffValue = cutoffValueOutter,
                                 x1_mean = 0,
                                 x2_mean = 0,
                                 x3_mean = 0,
                                 x1_var = x1_var,
                                 x2_var = x2_var,
                                 x3_var = x3_var,
                                 cov = cov,
                                 theta = theta, 
                                 area = "outside")
  if (typeIerrorOutter >= typeIErrorRate) {
    cutoffValueInner <- Inf
  } else {
    rootFunctionBorr <- function(z) {
      probFinderprobFinder(cutoffValue = z,
                           x1_mean = 0,
                           x2_mean = 0,
                           x3_mean = 0,
                           x1_var = x1_var,
                           x2_var = x2_var,
                           x3_var = x3_var,
                           cov = cov,
                           theta = theta, 
                           area = "inside") - (typeIErrorRate - typeIerrorOutter)
    }
    
    cutoffValueInner <- uniroot(rootFunctionBorr, interval = c(0, 10), tol = 1e-12)$root
  }
  
  betaArray = c()
  typeIErrorArray = c()
  powerArray = c()
  
  for (bias in seq(-1, 1, by = 0.01)) {
    
    betaArray <- c(betaArray, pnorm(theta, mean = bias, sd = sqrt(x2_var)) - pnorm(-theta, mean = bias, sd = sqrt(x2_var)))
    typeIErrorArray <- c(typeIErrorArray, probFinder(cutoffValue = cutoffValueOutter,
                                                     x1_mean = 0,
                                                     x2_mean = bias,
                                                     x3_mean = - w * bias,
                                                     x1_var = x1_var,
                                                     x2_var = x2_var,
                                                     x3_var = x3_var,
                                                     cov = cov,
                                                     theta = theta, 
                                                     area = "outside") + probFinderprobFinder(cutoffValue = cutoffValue_borr,
                                                                                              x1_mean = 0,
                                                                                              x2_mean = bias,
                                                                                              x3_mean = - w * bias,
                                                                                              x1_var = x1_var,
                                                                                              x2_var = x2_var,
                                                                                              x3_var = x3_var,
                                                                                              cov = cov,
                                                                                              theta = theta, 
                                                                                              area = "inside"))
    powerArray <- c(powerArray, probFinder(cutoffValue = cutoffValueOutter,
                                           x1_mean = effectSize,
                                           x2_mean = bias,
                                           x3_mean = effectSize - w * bias,
                                           x1_var = x1_var,
                                           x2_var = x2_var,
                                           x3_var = x3_var,
                                           cov = cov,
                                           theta = theta, 
                                           area = "outside") + probFinderprobFinder(cutoffValue = cutoffValue_borr,
                                                                                    x1_mean = effectSize,
                                                                                    x2_mean = bias,
                                                                                    x3_mean = effectSize - w * bias,
                                                                                    x1_var = x1_var,
                                                                                    x2_var = x2_var,
                                                                                    x3_var = x3_var,
                                                                                    cov = cov,
                                                                                    theta = theta, 
                                                                                    area = "inside"))
  }
  
  

  return(list(betaArray = betaArray,
              typeIErrorArray = typeIErrorArray,
              powerArray = powerArray))
}
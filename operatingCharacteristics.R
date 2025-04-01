library(stats)
library(rootSolve)
library(MASS)
source("probFinder.R")
source("costFunc.R")

operatingCharacteristics <- function(randomizationRatio, 
                                     N,
                                     eqMargin,
                                     RWDRatio,
                                     effectSize, 
                                     standardDeviation, 
                                     typeIErrorRate, 
                                     typeIErrorRateEQ,
                                     twoSided = TRUE) {

  w <- RWDRatio / (1 + RWDRatio - randomizationRatio)
  x1_var <- standardDeviation^2 / (N * randomizationRatio * (1 - randomizationRatio))
  x2_var <- standardDeviation^2 * (1 + RWDRatio - randomizationRatio) / (N * RWDRatio * (1 - randomizationRatio))
  x3_var <- standardDeviation^2 * (1 + RWDRatio) / (N * randomizationRatio * (1 + RWDRatio - randomizationRatio))
  cov <- standardDeviation^2 / (N * (1 - randomizationRatio))
  
  theta <- eqMargin - qnorm(1 - typeIErrorRateEQ / 2) * sqrt(x2_var)
  if (theta < 0) theta <- 0
  
  cutoffValueOutter <- cutoffValueOutter <- ifelse(twoSided, qnorm(1 - typeIErrorRate / 2) * sqrt(x1_var), qnorm(1 - typeIErrorRate ) * sqrt(x1_var))
  typeIerrorOutter <- probFinder(cutoffValue = cutoffValueOutter,
                                 x1_mean = 0,
                                 x2_mean = 0,
                                 x3_mean = 0,
                                 x1_var = x1_var,
                                 x2_var = x2_var,
                                 x3_var = x3_var,
                                 cov = cov,
                                 theta = theta, 
                                 area = "outside",
                                 twoSided = twoSided)
  if (typeIerrorOutter >= typeIErrorRate) {
    cutoffValueInner <- Inf
  } else {
    rootFunctionBorr <- function(z) {
      probFinder(cutoffValue = z,
                 x1_mean = 0,
                 x2_mean = 0,
                 x3_mean = 0,
                 x1_var = x1_var,
                 x2_var = x2_var,
                 x3_var = x3_var,
                 cov = cov,
                 theta = theta, 
                 area = "inside",
                 twoSided = twoSided) - (typeIErrorRate - typeIerrorOutter)
    }
    
    cutoffValueInner <- uniroot(rootFunctionBorr, interval = c(0, 10), tol = 1e-12)$root
  }
  
  betaArray = c()
  biasArray = c()
  typeIErrorArray = c()
  powerArray = c()
  
  for (bias in seq(-1, 1, by = 0.01)) {
    
    betaArray <- c(betaArray, pnorm(theta, mean = bias, sd = sqrt(x2_var)) - pnorm(-theta, mean = bias, sd = sqrt(x2_var)))
    
    integrand <- function(z) {
      z * dnorm(z, mean = bias, sd = sqrt(x2_var))
    }
    
    biasArray = c(biasArray, w*integrate(integrand, lower = -theta, upper = theta)$value) 
    typeIErrorArray <- c(typeIErrorArray, probFinder(cutoffValue = cutoffValueOutter,
                                                     x1_mean = 0,
                                                     x2_mean = bias,
                                                     x3_mean = - w * bias,
                                                     x1_var = x1_var,
                                                     x2_var = x2_var,
                                                     x3_var = x3_var,
                                                     cov = cov,
                                                     theta = theta, 
                                                     area = "outside",
                                                     twoSided = twoSided) + probFinder(cutoffValue = cutoffValueInner,
                                                                                    x1_mean = 0,
                                                                                    x2_mean = bias,
                                                                                    x3_mean = - w * bias,
                                                                                    x1_var = x1_var,
                                                                                    x2_var = x2_var,
                                                                                    x3_var = x3_var,
                                                                                    cov = cov,
                                                                                    theta = theta, 
                                                                                    area = "inside",
                                                                                    twoSided = twoSided))
    powerArray <- c(powerArray, probFinder(cutoffValue = cutoffValueOutter,
                                           x1_mean = effectSize,
                                           x2_mean = bias,
                                           x3_mean = effectSize - w * bias,
                                           x1_var = x1_var,
                                           x2_var = x2_var,
                                           x3_var = x3_var,
                                           cov = cov,
                                           theta = theta, 
                                           area = "outside",
                                           twoSided = twoSided) + probFinder(cutoffValue = cutoffValueInner,
                                                                          x1_mean = effectSize,
                                                                          x2_mean = bias,
                                                                          x3_mean = effectSize - w * bias,
                                                                          x1_var = x1_var,
                                                                          x2_var = x2_var,
                                                                          x3_var = x3_var,
                                                                          cov = cov,
                                                                          theta = theta, 
                                                                          area = "inside",
                                                                          twoSided = twoSided))
  }
  
  

  return(list(betaArray = betaArray,
              biasArray = biasArray,
              typeIErrorArray = typeIErrorArray,
              powerArray = powerArray))
}
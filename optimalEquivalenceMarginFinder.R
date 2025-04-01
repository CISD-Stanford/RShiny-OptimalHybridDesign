source("probFinder.R")

optimalEquivalenceMarginFinder <- function(margin, randomizationRatio, sampleSize, RWDRatio, effectSize, standardDeviation, typeIErrorRate, typeIErrorRateEQ, twoSided = TRUE) {
  
  w <- RWDRatio / (1 + RWDRatio - randomizationRatio)
  x1_var <- standardDeviation^2 / (sampleSize * randomizationRatio * (1 - randomizationRatio))
  x2_var <- standardDeviation^2 * (1 + RWDRatio - randomizationRatio) / (sampleSize * RWDRatio * (1 - randomizationRatio))
  x3_var <- standardDeviation^2 * (1 + RWDRatio) / (sampleSize * randomizationRatio * (1 + RWDRatio - randomizationRatio))
  cov <- standardDeviation^2 / (sampleSize * (1 - randomizationRatio))
  theta <- margin - qnorm(1 - typeIErrorRateEQ / 2) * sqrt(x2_var)
  
  if (theta < 0) {
    theta <- 0
    if (twoSided) {
      return(list(maxTypeIerror = typeIErrorRate, 
                  minPower = pnorm(-qnorm(1 - typeIErrorRate / 2) * sqrt(x1_var), mean = effectSize, sd = sqrt(x1_var)) + (1 - pnorm(qnorm(1 - typeIErrorRate / 2) * sqrt(x1_var), mean = effectSize, sd = sqrt(x1_var)))))
    } else {
      return(list(maxTypeIerror = typeIErrorRate, 
                  minPower = (1 - pnorm(qnorm(1 - typeIErrorRate) * sqrt(x1_var), mean = effectSize, sd = sqrt(x1_var)))))
    }
  }
  
  cutoffValueOutter <- ifelse(twoSided, qnorm(1 - typeIErrorRate / 2) * sqrt(x1_var), qnorm(1 - typeIErrorRate ) * sqrt(x1_var))
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
  
  solTypeIerror <- optimize(function(x) {
    probFinder(cutoffValue = cutoffValueOutter, 
               x1_mean = 0, 
               x2_mean = x, 
               x3_mean = - w * x, 
               x1_var = x1_var, 
               x2_var = x2_var, 
               x3_var = x3_var, 
               cov = cov, 
               theta = theta, 
               area = "outside",
               twoSided = twoSided) + 
      probFinder(cutoffValue = cutoffValueInner, 
                 x1_mean = 0, 
                 x2_mean = x, 
                 x3_mean = -w * x, 
                 x1_var = x1_var, 
                 x2_var = x2_var, 
                 x3_var = x3_var, 
                 cov = cov, 
                 theta = theta, 
                 area = "inside",
                 twoSided = twoSided)
  }, interval = c(-1, 1), maximum = TRUE)
  
  maxTypeIerror <- as.numeric(solTypeIerror$objective)
  deltaTypeIerror <- solTypeIerror$maximum
  
  solPower <- optimize(function(x) {
    probFinder(cutoffValue = cutoffValueOutter, 
               x1_mean = effectSize, 
               x2_mean = x, 
               x3_mean = effectSize - w * x, 
               x1_var = x1_var, 
               x2_var = x2_var, 
               x3_var = x3_var,
               cov = cov,
               theta = theta,
               area = "outside",
               twoSided = twoSided) +
      probFinder(cutoffValue = cutoffValueInner, 
                 x1_mean = effectSize, 
                 x2_mean = x, 
                 x3_mean = effectSize - w * x, 
                 x1_var = x1_var, 
                 x2_var = x2_var,
                 x3_var = x3_var,
                 cov = cov, 
                 theta = theta,
                 area = "inside",
                 twoSided = twoSided)
  }, interval = c(-1, 1), maximum = FALSE)
  
  minPower <- as.numeric(solPower$objective)
  deltaPower <- solPower$minimum
  
  return(list(maxTypeIerror = maxTypeIerror, minPower = minPower, deltaTypeIerror = deltaTypeIerror, deltaPower = deltaPower))
}

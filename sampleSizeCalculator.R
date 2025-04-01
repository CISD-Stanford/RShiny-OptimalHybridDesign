library(MASS)

sampleSizeCalculator <- function (standardDeviation, randomizationRatio, effectSize, power, typeIErrorRate, twoSided = TRUE) {
  
  if (twoSided) {
    requiredSampleSize <- function(n) {
      var <- standardDeviation^2 / (n * randomizationRatio * (1 - randomizationRatio))
      p <- pnorm(-qnorm(1 - typeIErrorRate / 2) * sqrt(var), mean = effectSize, sd = sqrt(var)) + (1 - pnorm(qnorm(1 - typeIErrorRate / 2) * sqrt(var), mean = effectSize, sd = sqrt(var)))
      return(p - power)
    }
  } else {
    requiredSampleSize <- function(n) {
      var <- standardDeviation^2 / (n * randomizationRatio * (1 - randomizationRatio))
      p <- (1 - pnorm(qnorm(1 - typeIErrorRate) * sqrt(var), mean = effectSize, sd = sqrt(var)))
      return(p - power)
    }
  }
  
  sol <- uniroot(requiredSampleSize, lower = 1, upper = 100000, tol = 1e-12, maxiter = 10000)
  N = sol$root
  
  rFrac <- fractions(randomizationRatio)
  denom <- as.integer(strsplit(as.character(rFrac), "/")[[1]][2])
  
  N <- ceiling(N/denom)*denom
  
  return(N)
}
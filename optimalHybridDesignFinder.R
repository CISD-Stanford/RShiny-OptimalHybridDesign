library(stats)
library(rootSolve)
library(MASS)
library(shiny)
source("optimalEquivalenceMarginFinder.R")
source("probFinder.R")
source("costFunc.R")

optimalHybridDesignFinder <- function(randomizationRatios, 
                                      RWDRatio, 
                                      effectSize, 
                                      standardDeviation, 
                                      typeIErrorRate, 
                                      typeIErrorRateEQ, 
                                      weightPower,
                                      weightTrtAllocation,
                                      weightSampleSize,
                                      weightPowerLoss,
                                      lowerBoundaryPowerLoss, 
                                      weightTypeIErrorInflation,
                                      upperBoundaryTypeIErrorInflation, 
                                      maxSampleSize, 
                                      referencePower,
                                      session,
                                      twoSided = TRUE) {
  
  designs = matrix(nrow = 0, ncol = 8)
  colnames(designs) = c("randomizationRatio", "sampleSize", "trtPatients", "eqMargin", "power", "minPower", "maxTypeIErrorInflation", "cost")

  iter = c()
  for (r in randomizationRatios) {

    N = sampleSizeCalculator(standardDeviation = standardDeviation,
                             randomizationRatio = r,
                             effectSize = effectSize,
                             power = referencePower,
                             typeIErrorRate = typeIErrorRate,
                             twoSided = twoSided)
    N.min = sampleSizeCalculator(standardDeviation = standardDeviation,
                                 randomizationRatio = r,
                                 effectSize = effectSize,
                                 power = lowerBoundaryPowerLoss,
                                 typeIErrorRate = typeIErrorRate,
                                 twoSided = twoSided)
    N = min(N, maxSampleSize)
    rFrac <- fractions(r)
    denom <- as.integer(strsplit(as.character(rFrac), "/")[[1]][2])

    iter = append(iter, length(seq(N.min, N, by = denom)))
  }

  withProgress(message = "Finding Optimal Hybrid Design...", value = 0, {
    for (i in 1:length(randomizationRatios)) {
      
      r = randomizationRatios[i]
      N = sampleSizeCalculator(standardDeviation = standardDeviation,
                               randomizationRatio = r, 
                               effectSize = effectSize,
                               power = referencePower, 
                               typeIErrorRate = typeIErrorRate,
                               twoSided = twoSided)
      N.min = sampleSizeCalculator(standardDeviation = standardDeviation,
                                   randomizationRatio = r, 
                                   effectSize = effectSize,
                                   power = lowerBoundaryPowerLoss, 
                                   typeIErrorRate = typeIErrorRate,
                                   twoSided = twoSided)
      N = min(N, maxSampleSize)
      
      rFrac <- fractions(r)
      denom <- as.integer(strsplit(as.character(rFrac), "/")[[1]][2])
      sampleSizeArray = seq(N.min, N, by = denom)
      
      for (j in 1:length(sampleSizeArray)) {
        
        n = sampleSizeArray[j]
        bias <- 0
        w <- RWDRatio / (1 + RWDRatio - r)
        x1_var <- standardDeviation^2 / (n * r * (1 - r))
        x2_var <- standardDeviation^2 * (1 + RWDRatio - r) / (n * RWDRatio * (1 - r))
        x3_var <- standardDeviation^2 * (1 + RWDRatio) / (n * r * (1 + RWDRatio - r))
        cov <- standardDeviation^2 / (n * (1 - r))
        x1_mean <- effectSize
        x2_mean <- bias
        x3_mean <- x1_mean - w * x2_mean
        
        marginFinderTypeIerror <- function(m) {
          return(optimalEquivalenceMarginFinder(margin = m, randomizationRatio = r, sampleSize = n, RWDRatio = RWDRatio, effectSize = effectSize, standardDeviation = standardDeviation, typeIErrorRate = typeIErrorRate, typeIErrorRateEQ = typeIErrorRateEQ, twoSided = twoSided)$maxTypeIerror - upperBoundaryTypeIErrorInflation)
        }
        
        marginFinderPower <- function(m) {
          return(optimalEquivalenceMarginFinder(margin = m, randomizationRatio = r, sampleSize = n, RWDRatio = RWDRatio, effectSize = effectSize, standardDeviation = standardDeviation, typeIErrorRate = typeIErrorRate, typeIErrorRateEQ = typeIErrorRateEQ, twoSided = twoSided)$minPower - lowerBoundaryPowerLoss)
        }
        
        marginTypeIerror <- uniroot(marginFinderTypeIerror, interval = c(0, 10), tol = 1e-12)$root
        
        marginPower <- uniroot(marginFinderPower, interval = c(0, 10), tol = 1e-12)$root
        
        eqMargin <- min(marginTypeIerror, marginPower)
        
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
        
        beta <- pnorm(theta, mean = 0, sd = sqrt(x2_var)) - pnorm(-theta, mean = 0, sd = sqrt(x2_var))
        
        typeIerror <- as.numeric(probFinder(cutoffValue = cutoffValueOutter,
                                            x1_mean = 0,
                                            x2_mean = 0,
                                            x3_mean = 0,
                                            x1_var = x1_var,
                                            x2_var = x2_var,
                                            x3_var = x3_var,
                                            cov = cov,
                                            theta = theta, 
                                            area = "outside",
                                            twoSided = twoSided) + probFinder(cutoffValue = cutoffValueInner,
                                                                           x1_mean = 0,
                                                                           x2_mean = 0,
                                                                           x3_mean = 0,
                                                                           x1_var = x1_var,
                                                                           x2_var = x2_var,
                                                                           x3_var = x3_var,
                                                                           cov = cov,
                                                                           theta = theta, 
                                                                           area = "inside",
                                                                           twoSided = twoSided))
        
        power <- as.numeric(probFinder(cutoffValue = cutoffValueOutter,
                                       x1_mean = x1_mean,
                                       x2_mean = x2_mean,
                                       x3_mean = x3_mean,
                                       x1_var = x1_var,
                                       x2_var = x2_var,
                                       x3_var = x3_var,
                                       cov = cov,
                                       theta = theta, 
                                       area = "outside",
                                       twoSided = twoSided) + probFinder(cutoffValue = cutoffValueInner,
                                                                         x1_mean = x1_mean,
                                                                         x2_mean = x2_mean,
                                                                         x3_mean = x3_mean,
                                                                         x1_var = x1_var,
                                                                         x2_var = x2_var,
                                                                         x3_var = x3_var,
                                                                         cov = cov,
                                                                         theta = theta, 
                                                                         area = "inside",
                                                                         twoSided = twoSided))
        
        oc = optimalEquivalenceMarginFinder(margin = eqMargin, randomizationRatio = r, sampleSize = n, RWDRatio = RWDRatio, effectSize = effectSize, standardDeviation = standardDeviation, typeIErrorRate = typeIErrorRate, typeIErrorRateEQ = typeIErrorRateEQ, twoSided = twoSided)
        maxTypeIErrorInflation = oc$maxTypeIerror
        minPower = oc$minPower
        
        cost = costFunc(weightPower = weightPower,
                        power = power,
                        weightTrtAllocation = weightTrtAllocation,
                        Nt = n*r,
                        weightSampleSize = weightSampleSize,
                        N = n,
                        weightPowerLoss = weightPowerLoss,
                        minPower = minPower,
                        lowerBoundaryPowerLoss = lowerBoundaryPowerLoss,
                        weightTypeIErrorInflation = weightTypeIErrorInflation,
                        maxTypeIErrorInflation = maxTypeIErrorInflation,
                        upperBoundaryTypeIErrorInflation = upperBoundaryTypeIErrorInflation)
        
        designs <- rbind(designs, matrix(c(r, n, n*r, eqMargin, power, minPower, maxTypeIErrorInflation, cost), nrow = 1))
        incProgress(1/sum(iter), detail = paste("Step", ifelse(i == 1, 0, sum(iter[1:(i-1)])) + j, "of", sum(iter)))
      }
    }
  })
  
  designs = round(designs, 4)
  optimalHybridDesignIndex <- which.min(designs[, 8])
  optimalHybridDesign <- designs[optimalHybridDesignIndex, , drop = FALSE]
  designs = as.data.frame(designs)

  return(list(designCandidates = designs,
              randomizationRatio = optimalHybridDesign[1],
              sampleSize = optimalHybridDesign[2],
              trtPatients = optimalHybridDesign[3],
              eqMargin = optimalHybridDesign[4],
              power = optimalHybridDesign[5],
              minPower = optimalHybridDesign[6],
              maxTypeIErrorInflation = optimalHybridDesign[7],
              cost = optimalHybridDesign[8]))
}
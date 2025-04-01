costFunc <- function (weightPower, power, weightTrtAllocation, Nt, weightSampleSize, N, weightPowerLoss, minPower,lowerBoundaryPowerLoss, weightTypeIErrorInflation, maxTypeIErrorInflation, upperBoundaryTypeIErrorInflation) {
  
  cost =  -weightPower*power - 
    weightTrtAllocation*Nt + 
    weightSampleSize*N + 
    weightPowerLoss*(lowerBoundaryPowerLoss - minPower)*ifelse(minPower < (lowerBoundaryPowerLoss - 0.001), 1, 0) + 
    weightTypeIErrorInflation*(maxTypeIErrorInflation - upperBoundaryTypeIErrorInflation)*ifelse(maxTypeIErrorInflation > (upperBoundaryTypeIErrorInflation + 0.001), 1, 0)
  
  return(cost)
}
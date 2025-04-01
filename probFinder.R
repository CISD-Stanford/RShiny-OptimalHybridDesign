library(mvtnorm)

probFinder <- function(cutoffValue, x1_mean, x2_mean, x3_mean, x1_var, x2_var, x3_var, cov, theta, area, twoSided = TRUE) {
  
  if (twoSided) {
    # Non-borrow: upper left
    p_ul <- pmvnorm(upper = c(Inf, -theta), mean = c(x1_mean, x2_mean), sigma = matrix(c(x1_var, cov, cov, x2_var), nrow = 2)) - 
      pmvnorm(upper = c(cutoffValue, -theta), mean = c(x1_mean, x2_mean), sigma = matrix(c(x1_var, cov, cov, x2_var), nrow = 2))
    
    # Non-borrow: upper right
    p_ur <- 1 - pmvnorm(upper = c(Inf, theta), mean = c(x1_mean, x2_mean), sigma = matrix(c(x1_var, cov, cov, x2_var), nrow = 2)) - 
      pmvnorm(upper = c(cutoffValue, Inf), mean = c(x1_mean, x2_mean), sigma = matrix(c(x1_var, cov, cov, x2_var), nrow = 2)) + 
      pmvnorm(upper = c(cutoffValue, theta), mean = c(x1_mean, x2_mean), sigma = matrix(c(x1_var, cov, cov, x2_var), nrow = 2))
    
    # Non-borrow: bottom right
    p_br <- pmvnorm(upper = c(-cutoffValue, Inf), mean = c(x1_mean, x2_mean), sigma = matrix(c(x1_var, cov, cov, x2_var), nrow = 2)) - 
      pmvnorm(upper = c(-cutoffValue, theta), mean = c(x1_mean, x2_mean), sigma = matrix(c(x1_var, cov, cov, x2_var), nrow = 2))
    
    # Non-borrow: bottom left
    p_bl <- pmvnorm(upper = c(-cutoffValue, -theta), mean = c(x1_mean, x2_mean), sigma = matrix(c(x1_var, cov, cov, x2_var), nrow = 2))
    
    if (theta == 0) {
      p_u <- 0
      p_b <- 0
    } else {
      # Borrow: upper
      p_u <- pmvnorm(upper = c(Inf, theta), mean = c(x3_mean, x2_mean), sigma = matrix(c(x3_var, 0, 0, x2_var), nrow = 2)) - 
        pmvnorm(upper = c(cutoffValue, theta), mean = c(x3_mean, x2_mean), sigma = matrix(c(x3_var, 0, 0, x2_var), nrow = 2)) - 
        pmvnorm(upper = c(Inf, -theta), mean = c(x3_mean, x2_mean), sigma = matrix(c(x3_var, 0, 0, x2_var), nrow = 2)) + 
        pmvnorm(upper = c(cutoffValue, -theta), mean = c(x3_mean, x2_mean), sigma = matrix(c(x3_var, 0, 0, x2_var), nrow = 2))
      
      # Borrow: bottom
      p_b <- pmvnorm(upper = c(-cutoffValue, theta), mean = c(x3_mean, x2_mean), sigma = matrix(c(x3_var, 0, 0, x2_var), nrow = 2)) - 
        pmvnorm(upper = c(-cutoffValue, -theta), mean = c(x3_mean, x2_mean), sigma = matrix(c(x3_var, 0, 0, x2_var), nrow = 2))
    }
    
    if (area == "outside") {
      return(p_ul + p_ur + p_br + p_bl)
    } else if (area == "inside") {
      return(p_u + p_b)
    } else if (area == "all") {
      return(p_ul + p_ur + p_br + p_bl + p_u + p_b)
    }
  } else {
    # Non-borrow: upper left
    p_ul <- pmvnorm(upper = c(Inf, -theta), mean = c(x1_mean, x2_mean), sigma = matrix(c(x1_var, cov, cov, x2_var), nrow = 2)) - 
      pmvnorm(upper = c(cutoffValue, -theta), mean = c(x1_mean, x2_mean), sigma = matrix(c(x1_var, cov, cov, x2_var), nrow = 2))
    
    # Non-borrow: upper right
    p_ur <- 1 - pmvnorm(upper = c(Inf, theta), mean = c(x1_mean, x2_mean), sigma = matrix(c(x1_var, cov, cov, x2_var), nrow = 2)) - 
      pmvnorm(upper = c(cutoffValue, Inf), mean = c(x1_mean, x2_mean), sigma = matrix(c(x1_var, cov, cov, x2_var), nrow = 2)) + 
      pmvnorm(upper = c(cutoffValue, theta), mean = c(x1_mean, x2_mean), sigma = matrix(c(x1_var, cov, cov, x2_var), nrow = 2))
    
    if (theta == 0) {
      p_u <- 0
      p_b <- 0
    } else {
      # Borrow: upper
      p_u <- pmvnorm(upper = c(Inf, theta), mean = c(x3_mean, x2_mean), sigma = matrix(c(x3_var, 0, 0, x2_var), nrow = 2)) - 
        pmvnorm(upper = c(cutoffValue, theta), mean = c(x3_mean, x2_mean), sigma = matrix(c(x3_var, 0, 0, x2_var), nrow = 2)) - 
        pmvnorm(upper = c(Inf, -theta), mean = c(x3_mean, x2_mean), sigma = matrix(c(x3_var, 0, 0, x2_var), nrow = 2)) + 
        pmvnorm(upper = c(cutoffValue, -theta), mean = c(x3_mean, x2_mean), sigma = matrix(c(x3_var, 0, 0, x2_var), nrow = 2))
    }
    
    if (area == "outside") {
      return(p_ul + p_ur)
    } else if (area == "inside") {
      return(p_u)
    } else if (area == "all") {
      return(p_ul + p_ur + p_u)
    }
  }
}

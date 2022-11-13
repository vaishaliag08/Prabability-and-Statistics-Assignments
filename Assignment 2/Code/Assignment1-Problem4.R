library(MASS)

gaussian <- function(vec){
  
  b0 <- vec[1]
  b1 <- vec[2]
  sigma <- vec[3]
  
  loglikely <-  0
  
  for (i in 1:nrow(Insurance)){
    
    avg <- b0 + b1*Insurance$Holders[i]
    loglikely <- loglikely + dnorm(Insurance$Claims[i], 
                                   mean=avg, sd=sigma, log=TRUE)
    
  }
  return(-1*loglikely)
}

gaussian.estimate <- optim(c(0,1/5,1), gaussian)
cat(gaussian.estimate$par)

###############################################################################

loglaplace <- function(x, mu, b){
  
  numerator <- exp(-abs(x-mu)/b)
  denominator <- 2*b
  
  laplace <- numerator/denominator
  
  return(log(laplace))
  
}

laplacian <- function(vec){
  
  b0 <- vec[1]
  b1 <- vec[2]
  sigma <- vec[3]
  
  loglikely <-  0
  
  for (i in 1:nrow(Insurance)){
    
    avg <- b0 + b1*Insurance$Holders[i]
    loglikely <- loglikely + loglaplace(Insurance$Claims[i], 
                                        mu=avg, b=sigma)
    
  }
  return(-1*loglikely)
  
}

laplacian.estimate <- optim(c(0, 0, 1), laplacian)
cat(laplacian.estimate$par)

###############################################################################

lognormal <- function(vec){
  
  b0 <- vec[1]
  b1 <- vec[2]
  sigma <- vec[3]
  
  loglikely <-  0
  
  for (i in 1:nrow(Insurance)){
    
    if (Insurance$Claims[i] > 0){
      avg <- b0 + b1*Insurance$Holders[i]
      loglikely <- loglikely + dlnorm(Insurance$Claims[i], 
                                      meanlog=avg, sdlog=sigma, log=TRUE) 
    }
    
  }
  return(-1*loglikely)
}

lognormal.estimate <- optim(c(0, 0, 1), lognormal)
cat(lognormal.estimate$par)

###############################################################################

gamma.model <- function(vec){
  
  b0 <- vec[1]
  b1 <- vec[2]
  sigma <- vec[3]
  
  loglikely <-  0
  
  for (i in 1:nrow(Insurance)){
    
    if (Insurance$Claims[i] > 0){
      avg <- b0 + b1*Insurance$Holders[i]
      loglikely <- loglikely + dgamma(Insurance$Claims[i], 
                                      shape=avg, scale=sigma, log=TRUE)
    }
    
  }
  return(-1*loglikely)
}

gamma.estimate <- optim(c(0,1/5,1), gamma.model)
cat(gamma.estimate$par)


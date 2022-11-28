attach(faithful)
head(faithful)

## parameters = (alpha, sigma1, mu, sigma2, p)

model1 <- function(parameters, data){
  
  alpha <- parameters[1]
  sigma1 <- parameters[2]
  mu <- parameters[3]
  sigma2 <- parameters[4]
  p <- parameters[5]
  
  
  loglikely <- 0
  n = length(data)
  
  for (i in 1:n){
    
    first <- p*dgamma(data[i], shape=alpha, scale=sigma1)
    second <- (1-p)*dnorm(data[i], mean=mu, sd=sigma2)
    loglikely <- loglikely + log(first + second)
  }
  return(-loglikely)
}

initial1 <- c(3,0.45,80,9,0.35)

fit1 <- optim(initial1,
             model1,
             data=waiting,
             control=list(maxit=1500))

aic1 <- length(fit1$par)*2 + 2*fit1$value

cat("fit parameters:", fit1$par, "\n AIC for model 1:", aic1)

###############################################################################

model2 <- function(parameters, data){
  
  alpha1 <- exp(parameters[1])
  sigma1 <- exp(parameters[2])
  alpha2 <- exp(parameters[3])
  sigma2 <- exp(parameters[4])
  p <- exp(parameters[5])/(1 + exp(parameters[5]))
  
  
  loglikely <- 0
  n = length(data)
  
  for (i in 1:n){
    
    first <- p*dgamma(data[i], shape=alpha1, scale=sigma1)
    second <- (1-p)*dgamma(data[i], shape=alpha2, scale=sigma2)
    loglikely <- loglikely + log(first + second)
  }
  return(-loglikely)
}

initial2 <- c(4, 0, 4, 0, 0.4)

fit2 <- optim(initial2,
              model2,
              data=waiting,
              control=list(maxit=1500))

aic2 <- length(fit2$par)*2 + 2*fit2$value

cat("fit parameters:", fit2$par, "\n AIC for model 1:", aic2)

###############################################################################

model3 <- function(parameters, data){
  
  mu1 <- parameters[1]
  sigma1 <- exp(parameters[2])
  mu2 <- parameters[3]
  sigma2 <- exp(parameters[4])
  p <- exp(parameters[5])/(1 + exp(parameters[5]))
  
  
  loglikely <- 0
  n = length(data)
  
  for (i in 1:n){
    
    first <- p*dlnorm(data[i], meanlog=mu1, sdlog=sigma1)
    second <- (1-p)*dlnorm(data[i], meanlog=mu2, sdlog=sigma2)
    loglikely <- loglikely + log(first + second)
  }
  return(-loglikely)
}

initial3 <- c(2.76,-2.25,4.4,-2.6,0.35)

fit3 <- optim(initial3,
              model3,
              data=waiting,
              control=list(maxit=1500))

aic3 <- length(fit3$par)*2 + 2*fit2$value

cat("fit parameters:", fit3$par, "\n AIC for model 1:", aic3)

cat("all AIC:", aic1, aic2, aic3)

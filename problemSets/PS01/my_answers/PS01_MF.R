##########
### Q1 ###
##########

# create data set
set.seed(123)
data <- (rcauchy(1000, location = 0, scale = 1))

# Write an R function that implements the KS test where the reference 
# distribution is normal.

# create empirical distribution of observed data
ECDF <- ecdf(data)
empiricalCDF <- ECDF(data)
# generate test statistic
D <- max(abs(empiricalCDF - pnorm(data)))

# Write an R function that implements the KS test where the reference 
# distribution is normal.

KS_pValue <- function(eCDF, D){
  sum(eCDF >= D) / nrow(eCDF)
}
                        
KS_pValue(empiricalCDF, D)
# we reject the null hypothesis that the sample was drawn from the Normal distribution

# built in function for comparison purposes
ks.test(empiricalCDF, pnorm(data))



##########
### Q2 ###
##########
# Create Data
set.seed(123)
data2 <- data.frame(x = runif(200, 1, 10)) 
data2$y <- 0 + 2.75*data2$x + rnorm(200, 0, 1.5)

# Estimate an OLS regression in R that uses the Newton-Raphson algorithm 
# (specifically BFGS, which is a quasi-Newton method), 

linear.lik <- function(theta, y, X){
  n <- nrow(X)
  k <- ncol(X)
  beta <- theta[1:k]
  sigma2 <- theta[k+1]^2
  e <- y - X%*%beta
  logl <- -.5*n*log(n*pi)-.5*n*log(sigma2) - ((t(e) %*% e)/ (2*sigma2))
  return(-logl)
}

linear.MLE <- optim(fn=linear.lik, par = c(1,1,1), hessian=TRUE, y=data2$y,
                    X=cbind(1, data2$x), method = "BFGS")
linear.MLE$par

# equivalent results to using lm
lm(data2$y ~ data2$x)

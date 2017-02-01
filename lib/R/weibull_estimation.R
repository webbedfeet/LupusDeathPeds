## Determining the appropriate weibull parameters
weibull_estimation <- function(summData){
  yr <- summData$year
  p <- summData$surv_perc
  Lag <- unique(summData$Lag)
  
  if(all(p > 1)) p <- p/100 # convert to probabilities
  if(length(yr)==1){
    yr <- c(0.1,yr); p <- c(0.999,p)
  }
  p = pmax(0.001, pmin(0.999,p))
  if(Lag==0){ # Use Weibull plot method
    #ind <- (p>0 & p<1);yr <- yr[ind];p <- p[ind]
    m <- lm(log(-log(p))~log(yr))
    k <- m$coef[2]
    lambda <- exp(-m$coef[1]/k)
  } else { # Use optimization
    obj <- function(params){sum((log(1-p) - log(ptrunc(as.numeric(yr+Lag),'weibull',a = as.numeric(Lag),shape=params[1],scale=params[2])))^2)}
    ests <- optim(c(1.5,1.5),obj, control=list( maxit=2000))
    k <- ests$par[1]
    lambda <- ests$par[2]
  }
  return(data.frame(shape=k,scale=lambda))
}

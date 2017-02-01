summ2IPD <- function(summData, params, n.risk, n.events,maxfollowup, lags=0){
  set.seed(1050)
  k <- params$shape
  lambda <- params$scale
  yr <- summData$Time
  surv <- summData$Prob
  if(length(lags)>1){lags = lags$lags}
  times = c(yr, maxfollowup) # Ensured in creation that data starts at (0,1)
  surv = c(surv, NA)
  jmps <- round(exp(diff(log(surv))),2) # Proportional jump between times (jummst like KM)
  #   ind1 <- which(jmps==1 & surv[-length(surv)]==1) # No change
  #   ind2 <- which(jmps==1 & surv[-length(surv)]!=1)
  #   if(length(ind1)>0){
  #     times  <- times[-(ind1)]
  #     surv <- surv[-(ind1)]
  #   }
  #   if(length(ind2)>0){
  #     times <- times[-(ind2+1)]
  #     surv <- surv[-(ind2+1)]
  #   }
  #   jmps <- round(exp(diff(log(surv))),2)
  n <- as.numeric(n.risk)
  times.d <- times.cens <- numeric(0)
  if(all(surv[-length(surv)]==1)){
    times.d = numeric(0)
    times.c = 0+ppoints(n)*maxfollowup
    return(list(d.times=times.d, cens.times=times.c))
  }
  for(i in 1:length(jmps)){
    last.interval <- is.na(surv[i+1])
    if(!last.interval){
      if(jmps[i]==1) next # Assumes no censoring either
      n.d <- sum(cumprod(1-1/rev(1:n))>jmps[i])
      if(n.d==0) n.d <- 1
      n.d <- min(n.d, n.events-length(times.d)-(length(jmps)-i-1)) # Account for events in other jumps. Forces compensation by censoring
      x <- rtrunc(n.d, 'weibull',scale=lambda, shape=k, 
                  a=as.numeric(times[i]+lags), b=as.numeric(times[i+1]+lags))-as.numeric(lags)
      n.c <- find.cens(x,n,jmps[i],times[i], times[i+1])
      n.c <- min(n.c, n - n.d)
      y <- times[i]+ppoints(n.c)*(times[i+1]-times[i])
      times.d <- c(times.d,x)
      times.cens <- c(times.cens,y)
      n <- n-length(c(x,y))
    } else {
      if(times[i]==times[i+1]) times[i+1] <- times[i]+10
      n.d <- n.events - length(times.d)
      sn.c <- n - n.d
      x = rtrunc(n.d, 'weibull',scale=lambda, shape=k, a=times[i], b=times[i+1])
      y = times[i]+ppoints(sn.c)*(times[i+1]-times[i])
      times.d <- c(times.d,x)
      times.cens <- c(times.cens,y)
    }
  }
  return(list(d.times=times.d, cens.times=times.cens))
}

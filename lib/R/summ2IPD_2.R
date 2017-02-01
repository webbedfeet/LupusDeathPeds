summ2IPD_2 <- function(summData, params, n.risk, n.events, maxfollowup){
  set.seed(1050)
  k = params$shape
  lambda = params$scale
  yr = summData$Time
  surv = summData$Prob
  times = c(yr, maxfollowup)
  surv = c(surv, NA)
  jmps = round(exp(diff(log(surv))),3)
  
  n <- as.numeric(n.risk)
  # t.c <- rtrunc(n.risk-n.events, 'gamma', rate=(n.risk-n.events)/maxfollowup,
  #               shape=1,
  #               a=0, b = maxfollowup)
  # times.d <- numeric(0)
  times.d <- numeric(0)
  if(is.na(n.events)){
    ## No censoring until last followup time
    if(maxfollowup==max(yr)) maxfollowup <- maxfollowup+1
    maxjmp <- prod(jmps, na.rm=T)
    n.events <- round(n*(1-maxjmp)) # k = n(1-p2/p1) ## Best estimate from reported summaries
    n.c <- n - n.events
    t.c <- max(yr)+ppoints(n.c)*(maxfollowup-max(yr)) # all censoreds at the end
  } else {
    t.c <- ppoints(n.risk-n.events)*maxfollowup # Pre-assign censored variables
  }
  for(i in 1:length(jmps)){
    last.interval <- is.na(surv[i+1])
    if(!last.interval){
      n.c <- sum(t.c < times[i+1])-sum(t.c < times[i])
      n.d <- round(min(max((1-jmps[i])*(n - n.c),1), n.events-(length(jmps)-i)))
      t.d <- rtrunc(n.d, 'weibull', scale=lambda, shape=k,
                    a = as.numeric(times[i]), b = as.numeric(times[i+1]))
      times.d <- c(times.d, t.d)
      n <- n - n.c - n.d
    } else {
      if(times[i] ==times[i+1]) times[i+1] <- times[i]+5
      n.d <- max(0, n.events - length(times.d))
      t.d = rtrunc(n.d, 'weibull', scale=lambda, shape=k, 
                 a=times[i], b = times[i+1])
      times.d <- c(times.d, t.d)
    }
  }
  ## Censor all observations past the last reported summary time so that we 
  ## reduce potential bias when pooling. This doesn't affect the reproduction of 
  ## the reported survival pattern
  t_max <- max(summData$Time)
  t.c <- c(t.c, times.d[times.d > t_max])
  times.d <- times.d[times.d <= t_max]
  
  return(list(d.times=times.d, cens.times = t.c))
}
#' Function to estimate the number of deaths at a jump of a Kaplan-Meier curve, 
#' and the number of censorings between the current and previous jumps
#' 
#' @param n1 number at risk after previous jump
#' @param p Survival probability after previous jump
#' @param jump size of current jump
#' @return n2 number at risk after current jump
#' @return d number of events at the current jump
#' @return cens number of censored in previous interval
compute <- function(n1,p, jump){
  for(d in 1:n1){
    n2=d*p/jump
    if(round(n2)>n1){
      d <- max(1,d-1)
      break
    }
  } # number of deaths
  n2 <- min(round(d*p/jump),n1)
  if(abs(d*p/n2-jump)>abs(d*p/(n2+1)-jump)) n2 <- min(n1,n2+1)
  cens <- n1-n2 # number of censored
  return(data.frame(n2=n2, d=d, cens=cens))
}

compute.profile <- function(dat, n.risk){
  if(dat[2,1]==dat[1,1]){
    u = diff(range(dat[,1]))/500
    dat[2,1] = u
    dat = rbind(dat[1,], c(u,dat[1,2]), dat[-1,])
  }
  n.risk <- as.numeric(n.risk)
  prob <- round(dat$Prob,2)
  jumps <- -diff(prob) # p0-p1
  ndat <- nrow(dat)
  if(sum(jumps>0)==0){
    d.times <- NULL
    cens.times <- max(dat[,1])*ppoints(n.risk) # Uniform in time
  } else {
    times <- dat[which(jumps>0),1]
    jumps <- jumps[which(jumps>0)]
    # Start at time=0, prob=1
    res <- vector('list',length(jumps)+1)
    n1 <- as.numeric(n.risk)
    p <- 1
    i <- 1
    while(i <= length(jumps)){
      condn <- jumps[i]/p < 1/n1 # Think 1-jumps[i]/p > 1-1/n1
      #print(c(jumps[i]/p, 1/n1, condn))
      if(condn){ # Need to clean to ensure jumps really represent a death given new at-risk numbers 
        d <- KMclean(dat[(2*i-1):nrow(dat),], n1, 
                     start.time = dat[(2*i-1),1],
                     start.p = p)
        dat <- rbind(dat[1:(2*i-2),],d)
        dat  <- dat[!duplicated(dat),]
        j <- abs(diff(dat[,2]))
        tt <- dat[j>0,1]
        j <- j[j>0]
        times <- tt
        jumps <- j
      }
      out <- compute(n1,p,jumps[i])
      n1 <- out$n2 - out$d
      p <- p - jumps[i]
      res[[i]] <- out
      if(i == length(jumps)){
        break
      } else {
        i <- i+1
      }
    }
    result <- plyr::ldply(res)
    #     result <- data.frame(do.call(rbind,res))
    d.times <- rep(times[1:nrow(result)],result$d)
    times <- c(0,times)
    cens.times <- NULL
    for(i in 1:length(jumps)){
      cens.times <- c(cens.times,
                      times[i]+ppoints(result$cens[i])*diff(times[i:(i+1)]))
    }
    # last interval
    cens.times <- c(cens.times,rep(max(dat[,1]),n1))
  }
  return(list(d.times=d.times[d.times>0], cens.times=cens.times))
}

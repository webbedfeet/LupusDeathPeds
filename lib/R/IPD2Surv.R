
IPD2Surv <- function(ipd){
  require(survival)
  cens <- rep(c(1,0),sapply(ipd,length))
  times <- unlist(ipd) %>% unname()
  return(Surv(times, cens))
}
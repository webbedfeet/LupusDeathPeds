weibEst <- function(d){
  d <- d %>% dplyr::filter(Time > 0, Time <=20) # Removes Time=0 to keep log computable
  m <- lm(log(-log(Prob))~log(Time), data=d)
  out = c(shape = m$coef[2], scale = exp(-m$coef[1]/m$coef[2]))
  names(out) <- c('shape','scale')
  return(out)
}

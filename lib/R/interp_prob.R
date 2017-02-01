interp_prob <- function(d, target = c(0,5,10,15)){
  tgt = setdiff(target[target <= max(d$Time)], d$Time) # Only interpolate for non-available times within range
  probs = sapply(tgt, function(x) min(d$Prob[d$Time < x])) # Last value carried forward
  d <- rbind(d, tibble(Time = tgt, Prob=probs)) %>% arrange(Time)
  return(d)
}
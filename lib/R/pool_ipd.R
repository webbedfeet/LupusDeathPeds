pool_ipd <- function(lst){
  d.times <- do.call(c, lapply(lst, function(l) l$d.times))
  cens.times <- do.call(c,lapply(lst, function(l) l$cens.times))
  return(list(d.times=d.times, cens.times=cens.times))
}  
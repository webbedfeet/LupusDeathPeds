fillin <- function(x){
  if(!all(is.na(x)) & length(unique(x[!is.na(x)]))==1){
    x = rep(unique(x[!is.na(x)]),length(x))
  }
  return(x)
}

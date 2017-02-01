get_IPD <- function(KMdata, n.risk.data){
  if(!is.data.frame(KMdata)){
    l <- list()
    for(i in 1:length(KMdata)){
      l[[i]] <- compute.profile(KMdata[[i]], n.risk.data[i])
      
    }
    names(l) <- names(KMdata)
    out <- l
  } else{
    out <- compute.profile(KMdata,n.risk.data)
  }
  return(out)
}


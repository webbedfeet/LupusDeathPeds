#' Collapsing the summary MCMC data into a single data.frame
#'
#' This function inputs a hierarchical list of summary data (by strata) and
#' creates a single \code{data.frame} with columns specifying 
#' each stratum.
#'
#' @param out The output from \code{\link{mcmcout}}, the results from parsing analyses from a 
#'   single directory, representing a single analysis
#' @param windowctr The center year of each time window in the analysis
#' @examples
#' outMixed <- mcmcout('FullMixed')
#' resultsMixed <- collapseResults(outMixed)
collapseResults <- function(out, windowctr = Windows[,3]){
  require(plyr)
  if(packageVersion('plyr') < '1.8.1') stop('Need plyr version 1.8.1 or greater') # for ldply
  for(i in 1:3){
    for(j in 1:2){
      out[[i]][[j]] <- as.data.frame(out[[i]][[j]])
      out[[i]][[j]]$yr <- windowctr
      names(out[[i]][[j]]) <- c('LB','Med','UB','yr')
    }
    names(out[[i]])=c('Developed','Developing')
  }
  bl=ldply(
    lapply(out, ldply, 
           .id='Dev'),
    .id='Year')
  levels(bl$Year) <- c('5 years','10 years','15 years')
  return(bl)
}

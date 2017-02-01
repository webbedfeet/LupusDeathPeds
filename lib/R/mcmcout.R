#' Extracting summaries from MCMC output
#'
#' This function looks through a directory of MCMC output for the Moving Average analysis,
#' parses the .rda files for each window, and compiles posterior median and 95% credible intervals 
#' for 5 year, 10 year and 15 year survival, stratified by developed and developing countries.
#' 
#' @param resdir The directory where the .rda files reside
#' @param  basedir=file.path('data', 'mcmc') The base path to the mcmc results files
#' @return a list of lists, first by year and then by development status
#' @export
#' @example
#' outMixed <- mcmcout('FullMixed')
mcmcout <- function(resdir, basedir=file.path('data','mcmc')){
  require(coda)
  outcts <- vector('list',3)
  for(i in 1:3) outcts[[i]] <- vector('list',2)
  for(i in 1:3){
    for(j in 1:2){
      outcts[[i]][[j]] <- matrix(NA, ncol=3, nrow=61)
    }
  }
  names(outcts) <- c('pr5','pr10','pr15')
  
  for(i in 1:61){
    print(i)
    datfile = file.path(basedir,resdir,paste('Rfile',i,'.rda',sep=''))
    if(!file.exists(datfile)) next
    load(datfile)
    s <- summary(codaSamples)
    quants <- s$quantiles
    if(length(grep('\\[',row.names(quants)))==0){
      for(u in paste('pr',c(5,10,15),sep='')){
        outcts[[u]][[1]][i,] <- quants[u,c(1,3,5)]}
    } else {
      x <- row.names(quants)
      x <- grep('pr',x,value=T)
      nms = do.call(rbind,strsplit(gsub('\\]','',x),'\\['))
      for(k in 1:nrow(nms)){
        if(quants[x[k],3]==0)next
        outcts[[nms[k,1]]][[as.numeric(nms[k,2])]][i,] <- quants[x[k],c(1,3,5)]
      }
    }
  }
  return(outcts)
}

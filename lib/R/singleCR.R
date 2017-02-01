#' @title Extract a credible interval from a single analytic window
#' @author Abhijit Dasgupta <abhijit.dasgupta@nih.gov>
#' @description
#' This function extracts a CR from the simulated posterior distribution that is the outcome of 
#' an MCMC run.
#'
#' @param yr The center year of the window you want to query
#' @param  project='FullMixed' The study we want to query. This will be matched to directory names
#' @param  parameter='all' Specification of parameters we want to query. Default is all output parameters (not used)
#' @param  level=0.95 Probablity level (not used)
#' @param  digits Number of digits to report
#' @return A data.frame object with stratum information, posterior median and credible intervals
#' 
singleCR <- function(yr, project='FullMixed', parameter='all',level=0.95, digits=3) {
  # Packages
  require(coda)
  require(stringr)
  # Sanity checks
  if(yr < 1966 | yr>2010){stop('Year is out of range')}
  if(!(project %in% dir('data/mcmc'))){stop('Project does not exist')}
  if(level < 0 | level > 1){stop('level not in (0,1)')}
  rdafile <- file.path('data','mcmc',project,paste('Rfile',yr-1965,'.rda',sep=''))
  if(!file.exists(rdafile))stop(paste('No data is available for the year',yr))
  # Create separate environment for loading
  env <- new.env()
  # Extract data
  load(rdafile, envir=env)
  x <- summary(env$codaSamples, quantiles=c(0.5, (1-level)/2, 1-((1-level)/2)))$quantiles
  x[which(x[,1]==0),] <- NA
  out <- data.frame(x[grep('pr',row.names(x), value=T),])
  if(length(grep('\\[[1-2]\\]',row.names(out)))==0) row.names(out) <- paste(row.names(out),'[1]',sep='')
  x <- do.call(rbind,str_extract_all(row.names(out),'[0-9]+'))
  out <- cbind( Developed = ifelse(x[,2]=='1','Developed','Developing'),Years = x[,1], out)
  out <- out[order(out$Developed, as.numeric(out$Years)),]
  colnames(out)[-(1:2)] <- c('Median',paste('LCB (',level,')',sep=''),
                             paste('UCB (',level,')',sep=''))
  out[,c(3,4,5)] <- signif(100*(1-out[,c(3,5,4)]),digits)
  return(out)
}

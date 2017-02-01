#' @title Credible intervals from data covering a temporal interval
#' @author Abhijit Dasgupta <abhijit.dasgupta@nih.gov>
#' @description
#' This function inputs a temporal interval, identifies all studies that contribute
#' to that interval, pools the data and uses JAGS to obtain the posterior sampled
#' distribution of the survival parameters. It then extracts the posterior median and posterior
#' credible intervals for each parameter
#' 
#' It is assumed that there are separate priors for Developed and Developing countries,
#' but the prior is the same across the years in the interval
#' @param startyr Start year of interval
#' @param  endyr End year of interval
#' @param  project='FullMixed' Currently implemented for {FullMixed, Class3, Class4, Class5}
#' @param  level=0.95 Coverage probability of the interval
pooledCR <- function(startyr, endyr, project='FullMixed', level=0.95, digits=3){
  # Packages
  require(rjags)
  require(stringr)
  
  # Sanity checks
  if(startyr < 1966 | endyr > 2010) stop('Years out of range')
  if(startyr > endyr) stop('Need the start year to be before the end year')
  if(!(project %in% c('FullMixed','Class3','Class4','Class5'))){
    stop(paste('Cannot obtain data for ',project))
  }
  if(level<0|level>1) stop('level is not in (0,1)')
  
  # Acquire temporal metadata
  env <- new.env()
  load('data/rda/bookkeeping.rda',envir=env)
  
  # Identify studies
  yrs <- seq(startyr, endyr, by=1)
  x <- sapply(lapply(env$coverage, intersect, yrs), any)
  studies <- names(x)[x]
  
  # Acquire study metadata
  load(file.path('data','rda','WardTableFinal.rda'), envir=env)
  
  # Acquire study data
  if(length(grep('Class',project))>0){
    proj <- tolower(project)
    load(file.path('data','rda',paste(proj,'mcmc.rda',sep='')), envir=env)
    ipd <- env$study.ipd[names(env$study.ipd) %in% as.character(studies)]
    classdata <- get(paste(proj,'data',sep=''), envir=env)
    info <- merge(classdata, select(env$basedata,study, Arm, lags), 
                  by=c('study','Arm'))
    classfollow <- get(paste(proj,'follow',sep=''), envir=env)
    follow <- ldply(classfollow); names(follow)[1] <- 'study'
    follow <- merge(follow, select(env$basedata, study,Arm, number, 
                                   Developed, Decades, lags),
                    by=c('study','Arm'))
    follow <- dplyr::filter(follow, study %in% as.numeric(studies))
  } else {
    load(file.path('data','rda','mixedmcmc.rda'), envir=env)
    ipd <- c(env$keep.overall, env$keep.summ)
    ipd <- ipd[names(ipd) %in% as.character(studies)]
    info <- dplyr::filter(env$basedata, study %in% as.numeric(studies), 
                          Arm=='FullStudy', Group_class=='Mixed') %>%
      select(Author, study, Group_class, Developed, Decades, lags)
    follow <- dplyr::filter(env$followup.fullstudy, study %in% as.numeric(studies))
  }
  d <- datForJags(ipd, follow, info)
  codaSamples <- runjags(d)
  quant <- summary(codaSamples, quantiles=c(0.5, 0.5-level/2, 0.5+level/2))$quantile
  quant <- data.frame(quant[grep('pr',row.names(quant)),])
  if(length(grep('\\[',row.names(quant)))==0){
    row.names(quant) <- paste(row.names(quant),'[1]',sep='')
  }
  teststring <- 'pr([0-9]+)\\[([1-2])\\]'
  quant <- cbind('Developed'=ifelse(gsub(teststring,'\\2',row.names(quant))=='1',
                                    'Developed','Developing'),
                 Year = as.numeric(gsub(teststring,'\\1',row.names(quant))), quant)
  quant <- quant[order(quant$Developed, quant$Year),]
  colnames(quant)[-(1:2)] <- c('Median',
                               paste('LCB (', level,')', sep=''),
                               paste('UCB (', level,')', sep=''))
  quant[,c(3,4,5)] <- signif(100*(1-quant[,c(3,5,4)]),digits)
  return(quant)
}


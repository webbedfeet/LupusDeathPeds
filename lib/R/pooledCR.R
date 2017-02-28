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
#' @param  level=0.95 Coverage probability of the interval
pooledCR <- function(startyr, endyr,  level=0.95, digits=3){
  # Packages
  require(rjags)
  require(stringr)

  # Sanity checks
  if(startyr < 1968 | endyr > 2016) stop('Years out of range')
  if(startyr > endyr) stop('Need the start year to be before the end year')
  if(level<0|level>1) stop('level is not in (0,1)')

  # Acquire temporal metadata
  load('data/rda/window_membership.rda')

  # Identify studies
  yrs <- seq(startyr, endyr, by=1)
  window_id <- which(apply(Windows, 1, function(x) length(intersect(yrs, x))>0))
  ind <- membership[,paste0('Window',window_id)] %>% apply(1, function(x) sum(x)>0)
  studies <- membership$pubID[ind]

  # Acquire study metadata
  load('data/rda/final_study_info.rda')


  # Acquire study data
  load('data/rda/KM2IPD.rda')
  load('data/rda/summaries2IPD.rda')
  load('data/rda/followup_data.rda')

  ipds <- c(KM2IPD[intersect(names(KM2IPD), studies)],
            summaries2IPD[intersect(names(summaries2IPD), studies)])
  fup <- fup_data %>% dplyr::filter(pubID %in% studies)

  jagsdata <- datForJags(ipds,follow=fup, info=study_info)


  codaSamples <- runjags(jagsdata)
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


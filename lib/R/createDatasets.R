#' @title Create datasets in a folder that are required for the Moving Average estimation
#' @author Abhijit Dasgupta <abhijit.dasgupta@nih.gov>
#'
#' @description
#' This function creates JAGS-compatible datasets and adds required files to a new folder for a particular
#' Moving Average analysis based on annual windows. It adds IPD data from the studies which are members
#' of each window into a JAGS-compatible file.
#'
#' @param members A tidy table for which study contributes to which window
#' @param  outdir Directory to store the files
#' @param  info Study information (including studi ID, class, developed status and left truncation info)
#' @param  minkm=5 Minimum number of Kaplan-Meier studies to be included per window
#' @param ipd=study.ipd A list of study-specific IPD, derived from KM curves
#' @param  followup=followup.fullstudy data.frame with followup information (max followup, events, number at risk, etc) (currently not implemented)
createDatasets <- function(members, ipd, outdir, info=study_info, minkm=5, followup=fup_data){
  outdir <- file.path('data','mcmc',outdir)
  if(file.exists(outdir)) unlink(outdir, recursive=T)
  dir.create(outdir, showWarnings=FALSE, recursive=T) # ensure directory exists and is empty
  members1 <- members %>% filter(pubID %in% names(ipd))
  for(i in 1:(ncol(members1)-1)){
    print(i)
    if(sum(members1[,paste0('Window',i)]) < minkm) next # need at least minkm KM curves present in window
    ipd1 <- ipd[filter_(members1, paste0('Window',i))$pubID]
    followup1 <- followup %>% dplyr::filter(pubID %in% filter_(members, paste0('Window',i))$pubID)
    jagsdata <- datForJags(ipd1,follow=followup1,info=info)
    if(!is.null(jagsdata)){
      assign(paste('data',i,sep=''), jagsdata)
      dump(paste('data',i,sep=''), file=file.path(outdir,paste('fullmodel',i,'.txt',sep='')))
    }
  }
  file.copy('fullmodelcts.bug',outdir)
  file.copy('fullmodelcts2.bug', outdir)
  file.copy('Rfile.R', outdir)
  file.copy('scripts/template.py',outdir)
}

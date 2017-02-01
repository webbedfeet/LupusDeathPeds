#' Creating data for import into JAGS
#'
#' This function inputs data from the survival studies, including IPD data and
#' followup data, and converts it into the R dump() format for import into
#' JAGS
#'
#' @param ipd This is a list of IPD data, one per study. Each element is
#'            itself a list comprising 2 elements, t.d (death) and t.cens (censored) times
#'
#' @param  follow A data.frame that contains information from studies that
#'                contains only followup time and number of events, in addition
#'                to some study characteristics (not currently implemented)
#' @param  info A data.frame that contains information about each study (study_info for this study)
#' @return A list in R dump() format using all the input data that is amenable for
#'         processing by JAGS
#'
datForJags <- function(ipd, follow=fup_data, info=study_info){
  # out <- follow[,c('number','maxfollowup','Events','Developed',
  #                  'lags')]
  dat.jags <- as.data.frame(gen_jagsdata(ipd,info))
  names(dat.jags) <- c('td','tcens','trunc','isCensored1','geog1')
  dat.jags$geog1 <- as.factor(dat.jags$geog1)
  N1 <- nrow(dat.jags)
  dat.jags <- as.list(dat.jags)
  dat.jags[['N1']] <- N1
  if(any(dat.jags[['td']]==0, na.rm=T)){
    dat.jags[['td']] <- dat.jags[['td']]+0.001
  }
  if(any(dat.jags[['tcens']]==0, na.rm=T)){
    dat.jags[['tcens']] <- dat.jags[['tcens']]+0.001
  }
  if(nrow(follow)==0) follow <- NULL

# Follow-up data
  if(!is.null(follow)){
  out <- follow %>% select(max.f.up, n.death, number, Developed, Lag) %>%
    rename(maxfollowup = max.f.up, geog2=Developed, n = number, Events=n.death) %>%
    mutate(isCensored2 = as.integer(rep(1,n()))) %>%
    mutate(Y = rep(NA, n())) %>%
    mutate(geog2 = as.factor(geog2))
  dat_follow <- as.list(out)
  dat_follow$N2 <- nrow(out)
  dat <- c(dat.jags, dat_follow)
  dat$J <- length(unique(c(dat$geog1, dat$geog2)))
  } else {
    dat <- dat.jags
    dat$N2 <- 0
    dat$J <- length(unique(dat$geog1))
  }
  # dat <- as.list(out)
  # dat$N2 <- nrow(out)
  # dat.jags <- dat.jags[,-6]
  #   dat.jags$yr1 <- as.factor(dat.jags$yr1)

  # dat <- c(dat, as.list(dat.jags))
  # dat$J <- length(unique(c(dat$geog1,dat$geog2)))
  #   dat$K <- length(unique(c(dat$yr1,dat$yr2)))
  for(i in 1:length(dat)){
    if(is.factor(dat[[i]])) dat[[i]] <- as.numeric(dat[[i]])
  }
  return(dat)
}

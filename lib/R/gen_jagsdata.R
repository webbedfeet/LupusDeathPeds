#' Converting a list of IPD into JAGS-compatible data
#'
#' @param ipd The list of IPDs
#' @param info Other data, matched on some index with the names of the IPD list, that will be included
#'            in the JAGS dataset. Typically this is a data.frame of clinical characteristics of interest
#'            including possibly average left-truncation information stored in the variable `Lag`
#' @return A list of variables to be transformed into JAGS-compatible form
gen_jagsdata <- function(ipd, info=study_info){
  for(i in 1:length(ipd)){
    ipd[[i]]$pubID <- names(ipd)[i]
  }
  info <- dplyr::filter(info, armID==pubID)
  tst <- nrow(info)!=length(info$pubID)
  if(tst) stop('Need single info row per study')

  data.jags <- dplyr::bind_rows(lapply(ipd, function(d) jags_transform(d, info)))
  return(as.list(data.jags))
}

jags_transform <- function(ipd, info){
  # for a single IPD
  dat = ipd[c('d.times','cens.times')]
  pid = ipd$pubID
  study.info <- dplyr::filter(info, pubID == pid)
  t.d <- c(dat$d.times, rep(NA,length(dat$cens.times)))
  trunc <- rep(ifelse(is.na(study.info$Lag), 0, study.info$Lag), length(t.d))
  t.cens = c(rep(1000, length(dat$d.times)), dat$cens.times) # Need 1000 to be bigger than any times in ipd
  t.d <- t.d + trunc
  t.cens <- t.cens+trunc
  is.censored <- as.integer(is.na(t.d))
  geog = rep(study.info$Developed, length(t.d))
  bl <- data.frame(td=t.d, tcens=t.cens, trunc=trunc, isCensored=is.censored, geog=geog)
  return(bl)
}

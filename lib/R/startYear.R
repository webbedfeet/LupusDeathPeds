#' Compute analytic start year
#'
#' Compute the analytic start year for each study as a proportion of the study duration
#' @param dat=basedata data.frame containing start and end information for each study
#' @param  prop=0.5 Proportion into study when analytic start is (0.75=3/4 into study)
#' @return A vector giving the analytic start years of each study
startYear <- function(dat=study_info, prop=0.5){
  timedat <- select(dat, start.enrollment, end.fup, end.enrollment)
  yr_of_study <- round(with(timedat, (start.enrollment+(pmin(end.fup, end.enrollment, na.rm=T)-start.enrollment)*prop))) #counting from start
  ind <- which(is.na(yr_of_study))
  yr_of_study[ind] <- ifelse(is.na(timedat$start.enrollment[ind]),NA, round(timedat$start.enrollment[ind]+(dat$pubdate[ind]-1-timedat$start.enrollment[ind])*prop)) #counting from start.enrollment
  ind <- which(is.na(yr_of_study))
  yr_of_study[ind] <- round(pmin(dat$end.fup[ind], dat$pubdate[ind]-1, na.rm=T) - pmax(dat$f.up.months[ind], as.numeric(dat$max.f.up[ind]), na.rm=T)/12*(1-prop)) #counting from end
  ind <- is.na(yr_of_study)
  # yr_of_study[ind] <- rep(yr_of_study[dat$study==118 & !ind], sum(ind))
  return(yr_of_study)
}

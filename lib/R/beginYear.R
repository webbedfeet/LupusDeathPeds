#' Compute year the study begins
#' 
#' Compute the start year of each study, in case it is missing
beginYear <- function(dat=study_info){
  starts <- dat$start.enrollment
  ind <- which(is.na(starts))
  starts[ind] <- ifelse(!is.na(startYear(dat[ind,])) & !is.na(endYear(dat[ind,])),
                        endYear(dat[ind,])-2*(endYear(dat[ind,])-startYear(dat[ind,])), starts[ind])
  ind <- which(is.na(starts))
  starts[ind] <- with(dat[ind,], round(pubdate - pmax(as.numeric(max.f.up), f.up.months,na.rm=T)/12-1))
  ind <- which(is.na(starts)) # just arms of Wu (2014)
  starts[ind] <- starts[min(ind)-1]
  return(starts)
}

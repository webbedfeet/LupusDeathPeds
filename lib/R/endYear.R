#' Compute the analytic end year 
#'
#' The analytic end year of the study is the end of the period that will get credit in Moving Average analysis
#' It defaults to the end of the study, defined as the last of the end of followup, end of enrollment or one year
#' before publication. 
#' @param dat=basedata data.frame containing start and end information from each study
#' @param maxduration=NULL The maximum number of years a study can get credit
#' @param ... Additional parameters sent to startYear
#' @return A vector giving the analytic end years of each study
#' @export
#' @keywords
#' @seealso
#' @alias
#' @examples
endYear <- function(dat=study_info, maxduration=NULL,...){
  end <- pmin(pmax(dat$end.fup, dat$end.enrollment, na.rm=T), dat$pubdate-1, na.rm=T)
  end <- ifelse(is.na(dat$end.fup) & !is.na(dat$max.f.up) & !is.na(startYear(dat)),
                pmin(pmax(end,round(startYear(dat)+dat$max.f.up/12)),dat$pubdate-1, na.rm=T),end)
  if(!is.null(maxduration)){
    start <- startYear(dat, ...)
    end <- pmin(end, start+maxduration)
  }
  return(end)
}

#' Extracting data for staircase plot
#' 
#' This function takes the baseline study information and the study arm and extracts information needed
#' for creating a \code{\link{stairplot}}.
#' @param dat=basedata The \code{data.frame} holding study information
#' @param Arm='FullStudy' The study arm we want to plot
#' @return A \code{data.frame} object that can be input into \code{stairplot}
#' @section Details:
#' The input \code{data.frame} needs the following fields:
#' \itemize{
#' \item{'Author'}{Author}
#' \item{"pubdate"}{Publication date}
#' \item{"start_of_study"}{The start time of the study (i.e. beginning of enrollment)}
#' \item{"end_of_study"}{The end time of the study} (i.e. the earlier of max(end of enrollment, end of followup) and pubdate -1)
#' \item{"yr_of_study"}{The analytic start time }
#' \item{"yr_of_study_end"}{The analytic end time}
#' \item{"Developed"}{Developed or developing country}
#' \item{"number"}{Size of the study (number of participants)}
#' \item{"Design"}{Study design (Observational or trial)}
#' }
stairdata <- function(dat=basedata){
  dat <- dat %>%
    select(pubID, armID,Author, pubdate, start_of_study, yr_of_study, end_of_study, yr_of_study_end, Developed, Design, number) %>%
    arrange(start_of_study) %>% 
    distinct()
  return(dat)
}

#' Plotting "staircase" plots that show the temporal extent of each study
#' 
#' This function takes a summary data.frame with start and end dates of the study as well as start and
#' end dates of the analytic window contributed by the study, and plots it, sorted by start date
#' 
#' @author Abhijit Dasgupta <abhijit.dasgupta@nih.gov>
#' @param d A \code{data.frame} object created by \code{\link{stairdata}}
#' @param title='' The plot title
#' @param lims = NULL The x axis limits as a 2-vector, giving the year span for the plot
#' @return A ggplot object
stairplot <- function(d, title='',lims=NULL){
  require(ggplot2)
  ggplot(d, aes(x = seq_along(yr_of_study), y=yr_of_study, 
                ymin=start_of_study, ymax=end_of_study))+
    geom_point(aes(size=sqrt(number), color=Design))+
    geom_linerange()+
    geom_linerange(aes(x=seq_along(yr_of_study), ymin=yr_of_study, ymax=yr_of_study_end), colour='green')+
    labs(x='' )+guides(size=FALSE)+
    geom_text(aes(x=seq_along(yr_of_study),y=start_of_study-0.5, label=pubID, size=4, hjust=1))+
    #coord_flip()+
    theme(legend.position='bottom', axis.text.y=element_blank(), axis.ticks.y=element_blank())+
    scale_y_continuous('Year', breaks=seq(1940,2016,by=10), limits=lims)+
    coord_flip()+
    ggtitle(title)
}

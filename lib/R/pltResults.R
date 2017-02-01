#' Plotting the results of continuous analysis, with panels by Developed status
#'
#' This function takes the results of \code{\link{collapseResults}} and plots them, with colors
#' representing incidence rates after 5, 10 and 15 years, dotted lines representing a 95% posterior
#' interval, and panels for Developed and Developing countries
#'
#' @param bl The results from collapseResults
#' @import ggplot2
#' @examples
#' outMixed <- mcmcout('FullMixed')
#' resultsMixed <- collapseResults(outMixed)
#' plt <- pltResults(resultsMixed) + ggtitle('Mixed studies')
pltResults <- function(bl){
  require(ggplot2)
  levels(bl$Dev) <- paste(levels(bl$Dev),'countries')
  levels(bl$Year) <- gsub('s','',levels(bl$Year))
  p=ggplot(bl,aes(x=yr,y=Med, group=Year,color=Year))+geom_line(size=1)+
    geom_line(aes(x=yr, y=LB),linetype=2)+geom_line(aes(x=yr, y=UB),linetype=2)+
    facet_wrap(~Dev, ncol=1)+ylim(0,1)+scale_color_hue(l=40)+
    labs(x='Year',y='Probability of survival',color='')+
    theme_bw()+theme(legend.key=element_blank(), legend.position=c(0.2,0.2),
                     legend.background=element_rect(fill="transparent"))
  return(p)
}

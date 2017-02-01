## Function to clean digitized curves with summary survival data
## 

#' Summ_clean
#' 
#' This function takes digitized curves of summary survival data and 
#' ensures certain properties are met. It is assumed that summary survival
#' data is reported only at whole units
#'
#' @param dat A tibble with names Time and Prob, derived from digitized curves
#' @param n The number at risk at the beginning of the study
#'
#' @return A tibble with names Time and Prob, after cleaning
#' @export
#'
#' @examples
Summ_clean <- function(dat){
  dat <- mutate(dat, Time = round(Time)) %>% # round Time
    arrange(Time) %>%  # sort in increasing order of Time
    mutate(Prob = ifelse(Prob > 1, 1, Prob)) %>% 
    nest(-Time) %>% mutate(data = map_dbl(data, ~min(.$Prob))) %>% 
    dplyr::rename(Prob=data) %>% 
    mutate(Prob = ifelse(Time==0, 1, Prob)) %>% 
    mutate(dff = c(-0.01, diff(Prob))) %>% 
    mutate(Prob = ifelse(dff >0, Prob-dff, Prob)) %>% # ensures non-increasing
    filter(dff<0) %>% 
    rbind(tibble(Time=0,Prob=1, dff=.$Prob[1]-1)) %>% arrange(Time) %>% 
    select(-dff) %>% 
    distinct()
 return(dat) 
}
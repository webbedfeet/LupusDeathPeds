tidy.summary.survfit <- function(s){
  tibble('time' = s$time, 'n.risk' = s$n.risk,
         'n.event' = s$n.event,
         'Survival' = s$surv)
}
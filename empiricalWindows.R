## Getting raw estimates from digitized curves
source('lib/reload.R'); reload()
load('data/rda/final_study_info.rda')
load('data/rda/KM_digitized.rda')
load('data/rda/KM2IPD.rda')
load('data/rda/summaries2IPD.rda')
load('data/rda/window_membership.rda')

ipds <- c(KM2IPD, summaries2IPD)
members <- membership %>% filter(pubID %in% names(ipds))

draw_curves <- function(n, members, ipds, by_id=T, by_dev = !by_id){ # n is window number in memberships
  ids <- members$pubID[members[[paste0('Window',n)]]]
  lapply(ipds[ids], function(ipd){
    tidy(summary(survfit(IPD2Surv(ipd)~1),times = seq(0,20,by=0.5)))
  }) %>% plyr::ldply(.id='pubID') %>%
    left_join(study_info %>% select(pubID, Developed)) -> bl
  if(by_id){
    ggplot(bl, aes(time, Survival, group=pubID, color=pubID))+geom_line()+ylim(0,1)
  } else{
    ggplot(bl, aes(time, Survival, group=pubID, color=Developed))+geom_line()+ylim(0,1)
  }
}


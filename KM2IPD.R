source('lib/reload.R')
reload()
source('preamble.R')
load('data/rda/KM_digitized.rda') #loads KM_full & KM_digitized_clean

KM2stratIPD <- list()
for(u in names(KM_digitized_clean)){
  KM2stratIPD[[u]] <- get_IPD(KM_digitized_clean[[u]],
                         KM_full$N[KM_full$filename==u])
}

## Pooling studies as needed
KM2IPD <- list()
for(u in KM_full$ids){
  lst <- KM2stratIPD[KM_full$filename[KM_full$ids==u]]
  KM2IPD[[u]] <- pool_ipd(lst)
  KM2IPD[[u]] <- lapply(KM2IPD[[u]], unname)
}

## Add McCombs data to this
dat <- read_excel(file.path(popdir,'McCombs data.xlsx'))
ipd <- list('d.times' = dat$duration[dat$died==1]/12, 'cens.times' = dat$duration[dat$died==0]/12)
KM2IPD[['Mccombs_1959']] <- ipd
save(KM2IPD, file='data/rda/KM2IPD.rda', compress=T)

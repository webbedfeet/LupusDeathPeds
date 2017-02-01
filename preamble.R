# Things to pre-load
#

AD_dirs_mac <- c('data' = '/Volumes/NO NAME/LupusMetaDeath/Abhijit SLE Mortality Library',
                  'popdata' = '/Volumes/NO NAME/LupusMetaDeath',
                  'home' = '/Users/abhijit/NIAMS/Ward/LupusDeath')
AD_local_mac <- c('data' = '/Users/abhijit/Dropbox/Work/Ward/Studies/LupusMetaDeath/Abhijit SLE Mortality Library',
                  'popdata' = '/Users/abhijit/Dropbox/Work/Ward/Studies/LupusMetaDeath',
                  'home' = '/Users/abhijit/NIAMS/Ward/LupusDeath')
AD_local_win <- AD_local_mac %>% str_replace('/Users','C:/Users') %>% str_replace('abhijit','dasgupab') %>%
  str_replace('NIAMS/','')
names(AD_local_win) <- names(AD_local_mac)

if(Sys.info()['sysname'] == 'Windows'){
  datadir <- AD_local_win['data']
  popdir <- AD_local_win['popdata']
} else {
  popdir <- AD_local_mac['popdata']
  datadir <- AD_local_mac['data']
}

FH_dirs <- c('data' = 'Papers/Abhijit SLE Mortality Library/',
             'popdata' = 'Papers')
AD_peds_dirs <- c('data' = '/Users/abhijit/Dropbox/Work/Ward/Studies/LupusMetaDeath/pediatric')
options(dplyr.width = Inf, stringsAsFactors=F)

`%!in%` <- Negate(`%in%`)


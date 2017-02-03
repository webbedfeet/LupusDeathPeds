# Extracting information from Excel file
source('lib/reload.R')
reload()
source('preamble.R')

#require(rjags)

#datadir <- AD_local_mac['popdata']
#datadir <- FH_dirs_ped['popdata']
#study_info<- read_excel(file.path(datadir,'peds_mw LL_mw_update.xlsx'))[1:62, 1:46]
study_info<- read_excel(file.path(datadir,'peds_mw LL_mw_update.xlsx'))[1:62, 2:46]
study_info$Author[study_info$Author=='Ramirez Gomez'] <- 'Ramirez-Gomez'
study_info$Author[study_info$Author=='das Chagas Medeiros'] <- 'das-Chagas-Medeiros'
study_info$Author[study_info$Author=='estes d'] <- 'estes'
study_info$Author[study_info$Author=='Reveille JD,'] <- 'Reveille'
study_info$Author[study_info$Author=='Wu G,'] <- 'Wu'



## Fix names of variables
names(study_info) <- str_trim(names(study_info)) #get rid of white spaces
study_info <- study_info %>%  #%>% is like with(dataset,)
  dplyr::rename(surv15yr = `surv15 y`,  #::specifies rename is in package dplyr
                Time0 = `Time 0`)
names(study_info) <- make.names(names(study_info)) %>%
  str_replace('\\.+$','') %>%
  str_replace_all( '\\.+','.')

## Cleaning data
study_info <- study_info %>%
  mutate_if(is.character, str_trim) %>% # get rid of trailing spaces
  mutate(Author = Author %>% str_trim() %>% str_to_title() %>%
           str_extract('^[\\w-]+'),
         Arm = ifelse(!is.na(as.numeric(Arm)), letters[as.numeric(Arm)], Arm)) %>%  # convert 1,2, to a,b,
  mutate(Arm = ifelse(Arm %in% letters, Arm, ''),
         armID = paste(Author, pubdate, Arm, sep='_') %>%
           str_replace('_NA','') %>% str_replace(' ','_'), # create unique ID per arm
         pubID = paste(Author, pubdate, sep='_'), # create publication ID
         dis.dur.yrs = as.numeric(str_replace(dis.dur.yrs, '<', '')),
         Time0 = ifelse(inception==1,'diagnosis', Time0) %>% tolower()) %>%  # Account for inception cohorts
  mutate(pubID = ifelse(Arm %in% letters, paste(pubID, Arm, sep='_'), pubID),
         armID = str_replace(armID, '_$','')) %>%  # Add a,b to pubID, since different cohorts
  nest(-pubID) %>%
  mutate(data = map(data, ~mutate(., dis.dur.yrs = fillin(dis.dur.yrs)))) %>%
  unnest() %>%
  mutate(
    Lag = ifelse(Time0=='diagnosis', 0, dis.dur.yrs)) %>%  # create Lag variable
  mutate(Lag = ifelse(is.na(Lag), median(Lag[Time0=='studyentry'], na.rm=T),Lag))
#study_info$KM.fig[study_info$Author=='Zitnan'] <- NA # Fig is not KM curve

# #delete second and third row
# study_info<-study_info[,c(-2,-3)]

# #manually fix Ananieva start.enrollment  end.enrollment???????????????????????????????????????????
# study_info$start.enrollment[study_info$Author=='Ananieva' & study_info$number==52]<-1971#??????????????????????
# study_info$end.enrollment[study_info$Author=='Ananieva'& study_info$number==52]<-1980#??????????????????????
# study_info[study_info$Author=='Ananieva',]#???????????????????????????????????????????????

save(study_info, file='data/rda/study_info.rda', compress=T)

# summary_survivalPed <- study_info %>%
#   select(armID, pubID, number, Lag, starts_with('surv')) %>%
#   gather(year, surv_perc, starts_with('surv')) %>%
#   mutate(year = as.numeric(str_replace(year, 'surv([0-9]+)yr', '\\1'))) %>%
#   dplyr::filter(!is.na(surv_perc)) %>%
#   arrange(armID)
#
# pubs_with_KMPed <- study_info %>%
#   #dplyr::filter(KM.fig!='0', !is.na(KM.fig)) %>%
#   dplyr::filter(!is.na(KM.fig), !is.na(KM.fig)) %>%
#   select(pubID) %>%
#   distinct()  #unique values
#
# summary_survivalPed <- summary_survivalPed %>%
#   dplyr::filter(!(pubID %in% pubs_with_KMPed$pubID)) %>% # only keep studies without KM
#   left_join(
#     study_info %>% select(armID, deaths, f.up.months)
#   ) %>%
#   mutate(n.deaths = round(deaths * number / 100))
#
# save(summary_survivalPed, file='data/rda/summary_survivalPed.rda', compress=T)
#
#
#
#
#
# load('data/rda/summary_survivalPed.rda')
# load('data/rda/study_info.rda')
#
# with_survivalPed <- union(summary_survivalPed$pubID,#10
#                        pubs_with_KMPed$pubID)#21
# without_survivalPed <- study_info %>%
#   dplyr::filter(pubID %in% setdiff(pubID, with_survivalPed)) %>%
#   select(armID, pubID, number, deaths, f.up.months:max.f.up)
#
# save(without_survivalPed, file='data/rda/without_survivalPed.rda', compress=T)
#
# #write csv
# write.csv(study_info,file='study_info.csv')
# write.csv(summary_survivalPed,file='summary_survivalPed.csv')
# write.csv(without_survivalPed,file='without_survivalPed.csv')
#

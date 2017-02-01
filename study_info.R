# Extracting information from Excel file

source('lib/reload.R')
reload()
source('preamble.R')

study_info <- read_excel(file.path(popdir,'double-checked_mw3.xlsx'))[1:187, 1:43]
study_info$Author[study_info$Author=='Al Arfaj'] <- 'Al-Arfaj'
study_info$Author[study_info$Author=='Massadro'] <- 'Massardo'

## Fix names of variables
names(study_info) <- str_trim(names(study_info))
study_info <- study_info %>%
  dplyr::rename(surv15yr = `surv15 y`,
         Time0 = `Time 0`) %>%
  select(-study)
names(study_info) <- make.names(names(study_info)) %>%
  str_replace('\\.+$','') %>%
  str_replace_all( '\\.+','.')

## Cleaning data
study_info <- study_info %>%
  mutate_if(is.character, str_trim) %>% # get rid of trailing spaces
  mutate(Author = Author %>% str_trim() %>% str_to_title() %>%
           str_extract('^[\\w-]+'),
         Arm = ifelse(is.na(Arm),'',Arm),
         armID = paste(Author, pubdate, Arm, sep='_') %>%
           str_replace('_$','') %>% str_replace(' ','_'), # create unique ID per arm
         pubID = paste(Author, pubdate, sep='_')) %>%  # create publication ID
         mutate(pubID = ifelse(str_detect(Arm,'^[abc]$'), # separate out time-separated arms
                        paste0(pubID,Arm), pubID)) %>%
         mutate(armID = ifelse(str_detect(Arm,'^[abc]$'),
                        pubID, armID)) %>%
         mutate(dis.dur.yrs = as.numeric(str_replace(dis.dur.yrs, '<', '')),
         Time0 = ifelse(inception==1,'diagnosis', Time0) %>% tolower()) %>%  # Account for inception cohorts
  nest(-pubID) %>%
  mutate(data = map(data, ~mutate(., dis.dur.yrs = fillin(dis.dur.yrs)))) %>%
  unnest() %>%
  mutate(
         Lag = ifelse(Time0=='diagnosis', 0, dis.dur.yrs), # create Lag variable
         Lag = ifelse(is.na(Lag), median(Lag[Time0=='studyentry'], na.rm=T),Lag))
study_info$KM.fig[study_info$Author=='Zitnan'] <- NA # Fig is not KM curve
save(study_info, file='data/rda/study_info.rda', compress=T)



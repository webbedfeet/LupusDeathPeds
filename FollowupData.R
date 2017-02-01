# Follow-up data

source('preamble.R')
source('lib/reload.R')
reload()
load('data/rda/final_study_info.rda')
load('data/rda/KM2IPD.rda')
load('data/rda/summaries2IPD.rda')

ids <- setdiff(study_info$pubID, c(names(KM2IPD), names(summaries2IPD)))

fup_data <- study_info %>% filter(pubID %in% ids) %>% filter(male.only=='N') %>%
  filter(pubID != 'Rodriguez_2000') %>% # See Notes.Rmd
  mutate(max.f.up = ifelse(is.na(max.f.up), end_of_study-start_of_study, max.f.up/12)) %>% # Convert to years
  # select(armID,pubID, max.f.up, number, deaths, f.up.months, mean.median) %>%
  mutate(n.death = round(number*deaths/100)) %>%
  filter(!is.na(n.death)) %>%
  filter(armID==pubID) %>%
  select(armID, pubID, max.f.up, number, n.death, Developed, Lag, inception)

save(fup_data, file='data/rda/followup_data.rda', compress=T)

# This file will contain updates to various data sets that are resulting
# from data cleaning exercises
# Inputs will be study_info.rda and fig_metadata.rda

source('study_info.R')
load('data/rda/study_info.rda')

study_info <- arrange(study_info, pubID) %>%
  mutate(KM.fig = ifelse(is.na(KM.fig),'0',KM.fig))

# Data cleaning -----------------------------------------------------------

study_info$max.f.up <- as.numeric(study_info$max.f.up)

## Fessel is from United States
study_info$Country[study_info$pubID=='Fessel_1974'] <- 'United States'

## Update Estes' information to start at 1961 and end at 1969
study_info[study_info$pubID=='Estes_1971',]$start.enrollment=1961
study_info[study_info$pubID=='Estes_1971',]$end.enrollment=1969

## Update Joo information
study_info[study_info$pubID=='Joo_2015',]$end.fup = 2007
study_info[study_info$pubID=='Joo_2015',]$end.enrollment = NA

## Make Siegel_1969_female the overall study record
study_info$armID[study_info$armID=='Siegel_1969_female'] <- 'Siegel_1969'

## Create an overall record for Hashimoto
x <- study_info %>% dplyr::filter(Author=='Hashimoto') %>%
  summarise_all(funs(ifelse(length(unique(.))==1, unique(.), NA)))
bl <- study_info %>% dplyr::filter(Author=='Hashimoto')
x <- x %>%
  mutate(number = sum(bl$number),
         female = weighted.mean(bl$female, bl$number),
         f.up.months = weighted.mean(bl$f.up.months, bl$number),
         max.f.up = max(bl$max.f.up, na.rm=T),
         armID = pubID,
         Arm='')
study_info <- rbind(study_info, x) %>% arrange(pubID)

## Fix the information in the Design field
study_info <- study_info %>%
  mutate(Design = str_to_title(Design),
         Design = ifelse(str_count(Design, pattern='[[:alpha:]]+')==2, Design,
                         paste(Design, 'Observational', sep=' ')))

# New variables -----------------------------------------------------------

## Identify male only studies, which will be kept separate
male_only <- study_info %>% filter(female==0) %>%
  select(pubID) %>% left_join(study_info %>% dplyr::count(pubID)) %>%
  filter(n==1)
study_info <- study_info %>%
  mutate(male.only = factor(ifelse(pubID %in% male_only$pubID, 'Y','N')))

## Identify arms that represent full studies

bl <- study_info %>% filter(armID==pubID) %>% select(armID)
study_info <- study_info %>%
  mutate(fullstudy.arm = factor(ifelse(armID %in% bl$armID, 'Y','N')))


# Compute different temporal quantities for the studies -------------------

bl <- study_info %>%
  mutate(start_of_study = beginYear(.),
         yr_of_study = startYear(.),
         end_of_study = endYear(.),
         end_of_study_10 = endYear(., maxduration=10))

study_info <- study_info %>%
  left_join(bl %>% select(armID, start_of_study, yr_of_study, end_of_study, end_of_study_10)) %>%
  nest(-pubID) %>%
  mutate(data = lapply(data, function(d){
    mutate_each(d, funs(fillin(.)), start_of_study, yr_of_study, end_of_study,
                end_of_study_10)})) %>%
  unnest()

# Identify developed status -----------------------------------------------
## Country is "Developed" if it's World Bank GNI per capita classification is
## High Income

study_info <- study_info %>%
  mutate(Country = str_to_title(Country),
         Country = ifelse(Country %in% c('Usa','Uk'), str_to_upper(Country), Country),
         Country = str_replace(Country, '[\\.,]',''))
transform_country <- c(
  "USA" = "United States",
  "Latin American Countries" = "Latin America",
  "Korea" = "Korea, Rep.",
  'UK' = "United Kingdom",
  "Dubai" = "United Arab Emirates",
  "Russia" = "Russian Federation",
  "Hong Kong" = 'Hong Kong SAR, China',
  "Iran" = "Iran, Islamic Rep."
)
study_info <- study_info %>%
  mutate(Country = ifelse(Country %in% names(transform_country),
         transform_country[Country],
         Country))
download.file('http://siteresources.worldbank.org/DATASTATISTICS/Resources/OGHIST.xls', destfile = 'data/OGHIST.xls', mode='wb')
thresh <- read_excel('data/OGHIST.xls', sheet='Thresholds', skip=5)
thresh <- data.frame(t(thresh[c(1,18),-1]), stringsAsFactors=F)
thresh <- thresh %>%
  dplyr::rename(Year=X1, Threshold = X2) %>%
  mutate(Year = as.numeric(Year),
         Threshold = Threshold %>% str_trim() %>% str_replace('> ','') %>%
           str_replace(',','') %>% as.numeric)

library(wbstats)
incomes <- as_tibble(wb(indicator = 'NY.GNP.PCAP.CD')) %>%
  select(country, date, value) %>%
  dplyr::rename(Year = date, Income=value)
incomes <-incomes %>%
  mutate(Year = as.numeric(Year)) %>%
  left_join(thresh) %>%
  mutate(Status = ifelse(Income >= Threshold, "Developed", "Developing"))

bl <- study_info %>% select(pubID,Country, yr_of_study) %>%
  left_join(incomes, by=c('Country'='country',
                          'yr_of_study'='Year')) %>%
  mutate(Status = ifelse(Country=='United States', 'Developed', Status)) %>%
  mutate(Status = ifelse(is.na(Status) & yr_of_study < 1987 & Income >= 6000, 'Developed', Status)) %>%
  mutate(Status = ifelse(Country=='Latin America', 'Developing',Status)) %>%
  mutate(Status = ifelse(Country %in% c('Europe','Poland','Antilles','Russian Federation',  'India', 'China','Chile', 'Malaysia', 'Hungary', 'International','Barbados','Puerto Rico'), 'Developing', Status)) %>%
  mutate(Status = ifelse(is.na(Status), 'Developed',Status)) %>%
  mutate(Status = ifelse(pubID %in% c('Nossent_2010','Bruce_2015','Cervera_2003'),
         'Developed',Status))

development_status <- bl
save(development_status, file='data/rda/development_status.rda')

bl2 <- bl %>% select(pubID, yr_of_study, Status) %>% distinct()
study_info <- study_info %>%
  left_join(bl2) %>%
  dplyr::rename(Developed=Status)

## Update lag based on figure annotation (Years since diagnosis)
load('data/rda/fig_metadata.rda')

study_info %>% left_join(
  fig_metadata %>% select(ids, `from SLE`, TimeInYears) %>%
    distinct(),
  by = c('pubID'='ids')
) -> study_info

study_info %>% mutate(Lag2 = Lag, # Old lag variable
                      Lag = ifelse(`from SLE`=='y' & !is.na(`from SLE`), 0, Lag)) -> study_info
save(study_info, file='data/rda/final_study_info.rda', compress=T)


# Windowing for moving average --------------------------------------------

## FUll duration
study_duration <- study_info %>% mutate(start_date=yr_of_study,
                            end_date = end_of_study) %>%
  select(pubID, start_date, end_date) %>%
  nest(-pubID) %>%
  mutate(final = map(data, ~mutate(.,
                                   start_date = min(start_date, na.rm=T),
                                   end_date = max(end_date, na.rm=T)))) %>%
  select(-data) %>%
  unnest() %>%
  distinct()

time_range <- c(min(study_duration$start_date, na.rm=T), max(study_duration$end_date))
window_length <- 5
x <- seq(time_range[1], time_range[2]-window_length+1)
Windows <- do.call(cbind,
                   lapply(1:window_length-1, '+', x)) %>%
  unname()

study_years <- plyr::dlply(study_duration, ~pubID,
                           function(d) seq(d$start_date, d$end_date))

membership <- plyr::ldply(study_years,
                     function(x) apply(Windows,1,
                                       function(y) length(intersect(x,y))>0))
names(membership) <- str_replace(names(membership),'V','Window')

save(membership, Windows,file = 'data/rda/window_membership.rda', compress=T)

# 10 Year
study_duration <- study_info %>% mutate(start_date=yr_of_study,
                                        end_date = end_of_study_10) %>%
  select(pubID, start_date, end_date) %>%
  nest(-pubID) %>%
  mutate(final = map(data, ~mutate(.,
                                   start_date = min(start_date, na.rm=T),
                                   end_date = max(end_date, na.rm=T)))) %>%
  select(-data) %>%
  unnest() %>%
  distinct()

time_range <- c(min(study_duration$start_date, na.rm=T), max(study_duration$end_date))
window_length <- 5
x <- seq(time_range[1], time_range[2]-window_length+1)
Windows <- do.call(cbind,
                   lapply(1:window_length-1, '+', x)) %>%
  unname()

study_years <- plyr::dlply(study_duration, ~pubID,
                           function(d) seq(d$start_date, d$end_date))

membership_10 <- plyr::ldply(study_years,
                          function(x) apply(Windows,1,
                                            function(y) length(intersect(x,y))>0))
names(membership_10) <- str_replace(names(membership_10),'V','Window')

save(membership_10, Windows,file = 'data/rda/window_membership_10.rda', compress=T)

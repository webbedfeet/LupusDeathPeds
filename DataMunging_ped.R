# This file will contain updates to various data sets that are resulting
# from data cleaning exercises

#source('study_info.R') #change study_info to study_infoPed
source('study_info_ped.R')
load('data/rda/study_infoPed.rda')

study_infoPed <- arrange(study_infoPed, pubID) %>% 
  mutate(KM.fig = ifelse(is.na(KM.fig),'0',KM.fig))

# Data cleaning -----------------------------------------------------------

study_infoPed$max.f.up <- as.numeric(study_infoPed$max.f.up)

## Fessel is from United States
#study_infoPed$Country[study_infoPed$pubID=='Fessel_1974'] <- 'United States'

## Update Estes' information to start at 1961 and end at 1969
study_infoPed[study_infoPed$pubID=='Estes-D_1971',]$start.enrollment=1961
study_infoPed[study_infoPed$pubID=='Estes-D_1971',]$end.enrollment=1969

## Update Wu-G information: no start.enrollment data????????????????????????
#study_infoPed[study_infoPed$pubID=='Wu-G_2014',]$start.enrollment=1975????????????????????????



## Update Joo information
#study_info[study_info$pubID=='Joo_2015',]$end.fup = 2007
#study_info[study_info$pubID=='Joo_2015',]$end.enrollment = NA

## Make Siegel_1969_female the overall study record
#study_info$armID[study_info$armID=='Siegel_1969_female'] <- 'Siegel_1969'

## Create an overall record for Hashimoto
#x <- study_info %>% dplyr::filter(Author=='Hashimoto') %>% 
#  summarise_all(funs(ifelse(length(unique(.))==1, unique(.), NA))) 
#bl <- study_info %>% dplyr::filter(Author=='Hashimoto')  
#x <- x %>% 
#  mutate(number = sum(bl$number),
#         female = weighted.mean(bl$female, bl$number),
#         f.up.months = weighted.mean(bl$f.up.months, bl$number),
#         max.f.up = max(bl$max.f.up, na.rm=T),
#         armID = pubID,
#         Arm='')
#study_info <- rbind(study_info, x) %>% arrange(pubID)









## =========================Create an overall record for Gonzales=====================EXCLUDE=======
#x <- study_infoPed %>% dplyr::filter(Author=='Gonzales') %>% 
#  summarise_all(funs(ifelse(length(unique(.))==1, unique(.), NA))) #1 entry with same info
#bl <- study_infoPed %>% dplyr::filter(Author=='Gonzales')  #2 Gonzales entry
#x <- x %>%    #pool info in 2 entries
#  mutate(number = sum(bl$number),
#         female = weighted.mean(bl$female, bl$number),
#         f.up.months = weighted.mean(bl$f.up.months, bl$number),
#         max.f.up = max(bl$max.f.up, na.rm=T),
#         armID = pubID,
#         Arm='')
#study_infoPed <- rbind(study_infoPed, x) %>% arrange(pubID)

## Create an overall record for Kawasaki
#x <- study_infoPed %>% dplyr::filter(Author=='Kawasaki') %>% 
#  summarise_all(funs(ifelse(length(unique(.))==1, unique(.), NA))) 
#bl <- study_infoPed %>% dplyr::filter(Author=='Kawasaki') 
#x <- x %>%    #pool info in 2 entries
#  mutate(number = sum(bl$number),
#         female = weighted.mean(bl$female, bl$number),
#         f.up.months = weighted.mean(bl$f.up.months, bl$number),
#         max.f.up = max(bl$max.f.up, na.rm=T),
#         armID = pubID,
#         Arm='')
#study_infoPed <- rbind(study_infoPed, x) %>% arrange(pubID)#max.f.up==-inf due to no data in original

## Create an overall record for Ananieva
#x <- study_infoPed %>% dplyr::filter(Author=='Ananieva') %>% 
#  summarise_all(funs(ifelse(length(unique(.))==1, unique(.), NA))) 
#bl <- study_infoPed %>% dplyr::filter(Author=='Ananieva')  
#x <- x %>%    #pool info in 2 entries
#  mutate(number = sum(bl$number),
#         female = weighted.mean(bl$female, bl$number),
#         f.up.months = weighted.mean(bl$f.up.months, bl$number),
#         max.f.up = max(bl$max.f.up, na.rm=T),
#         armID = pubID,
#         Arm='')
#study_infoPed <- rbind(study_infoPed, x) %>% arrange(pubID)

## Create an overall record for Isavea
#x <- study_infoPed %>% dplyr::filter(Author=='Isavea') %>% 
#  summarise_all(funs(ifelse(length(unique(.))==1, unique(.), NA))) 
#bl <- study_infoPed %>% dplyr::filter(Author=='Isavea')  
#x <- x %>%    #pool info in 2 entries
#  mutate(number = sum(bl$number),
#         female = weighted.mean(bl$female, bl$number),
#         f.up.months = weighted.mean(bl$f.up.months, bl$number),
#         max.f.up = max(bl$max.f.up, na.rm=T),
#         armID = pubID,
#         Arm='')
#study_infoPed <- rbind(study_infoPed, x) %>% arrange(pubID)#max.f.up==-inf due to no data in original
## =========================Create an overall record for Gonzales=====================EXCLUDE=======










## Fix the information in the Design field
study_infoPed <- study_infoPed %>% 
  mutate(Design = str_to_title(Design),
         Design = ifelse(str_count(Design, pattern='[[:alpha:]]+')==2, Design,
                         paste(Design, 'Observational', sep=' ')))

# New variables -----------------------------------------ped has no gender issue------------------

## Identify male only studies, which will be kept separate
#male_only <- study_info %>% filter(female==0) %>% 
#  select(pubID) %>% left_join(study_info %>% count(pubID)) %>% 
#  filter(n==1)
#study_info <- study_info %>% 
#  mutate(male.only = factor(ifelse(pubID %in% male_only$pubID, 'Y','N')))


## Identify arms that represent full studies

bl <- study_infoPed %>% filter(armID==pubID) %>% select(armID)
study_infoPed <- study_infoPed %>% 
  mutate(fullstudy.arm = factor(ifelse(armID %in% bl$armID, 'Y','N')))
#study_infoPed$fullstudy.arm


# Compute different temporal quantities for the studies -------------------

bl <- study_infoPed %>%
  mutate(start_of_study = beginYear(.),#.refers to all columns to which the functions in funs are applied
         yr_of_study = startYear(.),
         end_of_study = endYear(.),
         end_of_study_10 = endYear(., maxduration=10))
#Error in eval(substitute(expr), envir, enclos) : 
#object 'maxfollowup' not found=========>fix
#fix: change max.f.up to maxfollowup
#study_infoPed$maxfollowup<-study_infoPed$max.f.up#max.f.up is in mth
#study_infoPed$esrdfu..m.<-study_infoPed$f.up.months
#names(study_infoPed)





study_infoPed <- study_infoPed %>% 
  left_join(bl %>% select(armID, start_of_study, yr_of_study, end_of_study, end_of_study_10)) %>% 
  nest(-pubID) %>% 
  mutate(data = lapply(data, function(d){
    mutate_each(d, funs(fillin(.)), start_of_study, yr_of_study, end_of_study, 
                end_of_study_10)})) %>% 
  unnest()

# Identify developed status -----------------------------------------------
## Country is "Developed" if it's World Bank GNI per capita classification is 
## High Income
#http://data.worldbank.org/indicator/NY.GNP.PCAP.PP.CD
#s1<-study_infoPed
#study_infoPed<-s1

study_infoPed <- study_infoPed %>% 
  mutate(Country = str_to_title(Country),
         #Country = ifelse(Country %in% c('china'), str_to_upper(Country), Country),
         Country = str_replace(Country, '[\\.,]',''))

study_infoPed$Country

transform_country <- c(
  "US." = "United States",
  "Russia" = "Russian Federation",
  'Uk' = "United Kingdom",
  'Egypt' = "Egypt, Arab Rep.",
  'Trinidad' = 'Trinidad and Tobago',
  'UK.' = "United Kingdom",
  "Us" = "United States",
  "Usa" = "United States",
  "Iran" = "Iran, Islamic Rep.")
  #"Latin American Countries" = "Latin America",
  #"Korea" = "Korea, Rep.",
  #"US." = "United States"
  

study_infoPed <- study_infoPed %>% 
  mutate(Country = ifelse(Country %in% names(transform_country),
         transform_country[Country],
         Country))
download.file('http://siteresources.worldbank.org/DATASTATISTICS/Resources/OGHIST.xls', destfile = 'data/OGHIST.xls', mode='wb')
thresh <- read_excel('data/OGHIST.xls', sheet='Thresholds', skip=5)
thresh <- data.frame(t(thresh[c(1,18),-1]), stringsAsFactors=F)
thresh <- thresh %>% 
  rename(Year=X1, Threshold = X2) %>% 
  mutate(Year = as.numeric(Year),
         Threshold = Threshold %>% str_trim() %>% str_replace('> ','') %>% 
           str_replace(',','') %>% as.numeric)

library(wbstats)
incomes <- as_tibble(wb(indicator = 'NY.GNP.PCAP.CD')) %>% 
  select(country, date, value) %>% 
  rename(Year = date, Income=value)
incomes <-incomes %>% 
  mutate(Year = as.numeric(Year)) %>% 
  left_join(thresh) %>% 
  mutate(Status = ifelse(Income >= Threshold, "Developed", "Developing"))

bl <- study_infoPed %>% select(pubID,Country, yr_of_study) %>% 
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
study_infoPed <- study_infoPed %>% 
  left_join(bl2) %>% 
  rename(Developed=Status)

save(study_infoPed, file='data/rda/study_infoPed.rda', compress=T)


#manually fix Wu-G start yr_of_study end_of_study???????????????????????????????????????????
study_infoPed$start_of_study[study_infoPed$Author=='Wu-G']<-1975#??????????????????????
study_infoPed$yr_of_study[study_infoPed$Author=='Wu-G']<-1992#??????????????????????
study_infoPed$end_of_study[study_infoPed$Author=='Wu-G']<-2009#??????????????????????
study_infoPed$end_of_study_10[study_infoPed$Author=='Wu-G']<-2002#??????????????????????
study_infoPed[study_infoPed$Author=='Wu-G',]#???????????????????????????????????????????????


# Windowing for moving average --------------------------------------------

study_duration <- study_infoPed %>% mutate(start_date=yr_of_study,
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
#window_length <- 5
window_length <- 10
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

cbind(study_infoPed$Country,study_infoPed$Developed)
write.csv(study_infoPed,file='study_infoPed1.csv')






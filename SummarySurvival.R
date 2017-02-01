## Processing figures with summary survival

source('lib/reload.R')
reload()
source('preamble.R')


# Import and process graphs of summary survival ---------------------------

csvdir = datadir

## Create metadata for graph summaries
##
load('data/rda/fig_metadata.rda')
load('data/rda/final_study_info.rda')
fig_metadata <- fig_metadata %>%
  mutate(male.only = ifelse(ids %in% study_info$pubID[study_info$male.only=='Y'],'Yes','No'))

metadata <- fig_metadata %>% filter(str_detect(filename, 'summ')) %>%
  filter(!str_detect(filename, 'cummort')) %>%
  filter(male.only=='No')
Ns <- study_info$number[study_info$pubID %in% metadata$ids]
names(Ns) <- study_info$pubID[study_info$pubID %in% metadata$ids]
metadata$N <- Ns[metadata$ids]
metadata$N[metadata$needs.pooling=='Yes'] <- NA
metadata$N[is.na(metadata$N)] <- c(
  25,113,  # Ballou
  501,21,  # Hashimoto
  257, 42, # Kellum
  218,77,  # Reveille
  56, 254, # Seleznick
  93, 41, 158, # Siegel
  546, 63, # Wallace
  499, 40, # Wang
  330, 117, 25 # Wu
)
bl = study_info %>% filter(pubID %in% unique(metadata$ids)) %>%
  filter(armID==pubID) %>%
  select(pubID, armID, number, deaths, max.f.up, start_of_study, end_of_study) %>%
  mutate(max.f.up = round(max.f.up/12),
         max.f.up = ifelse(is.na(max.f.up), end_of_study - start_of_study, max.f.up),
         n.death = round(deaths*number/100))
metadata <- metadata %>% left_join(bl %>% select(pubID, n.death, max.f.up), by=c('ids' = 'pubID')) %>%
  mutate(n.death = ifelse(needs.pooling=='Yes', NA, n.death))

save(metadata, file = 'data/rda/SummSurvMetadata.rda')

## Read csv files and process
##
csvfiles <- dir(csvdir, pattern='csv')
csvfiles <- csvfiles[csvfiles %in% metadata$filename]

csvdata <- lapply(csvfiles, function(f){
  d <- read.csv(file.path(csvdir,f), header=F)
  names(d) <- c('Time','Prob')
  return(d)
})
names(csvdata) <- csvfiles

## Cleaning the digitized data
csvdata <- lapply(csvdata, Summ_clean)

id <- 'Hersh-2010-fig1-adult-summ.csv'
csvdata[[id]] <- dplyr::mutate(csvdata[[id]], Time = Time/12) # This data is in months

# Create IPD from possibly stratified graphical summaries -----------------

IPD_from_csv_stratified <- list()
for(n in names(csvdata)){
  print(n)
  summData <- csvdata[[n]]
  params <- as.list(weibEst(summData))
  meta <- metadata %>% filter(filename==n)
  IPD_from_csv_stratified[[n]] <-
    summ2IPD_2(summData, params,
               meta$N,
               meta$n.death,
               meta$max.f.up)
}

IPD_from_csv <- list()
for(u in metadata$ids){
  lst = IPD_from_csv_stratified[metadata$filename[metadata$ids == u]]
  IPD_from_csv[[u]] <- pool_ipd(lst)
  IPD_from_csv[[u]] <- lapply(IPD_from_csv[[u]],unname)
}

# Import summary data not in graphs ---------------------------------------
load('data/rda/graph_types.rda')
ids <- c(csv_km, csv_summaries) %>% str_extract('^[\\w -]+[0-9]{4}[abc]?') %>%
  str_replace('-([0-9]{4})','_\\1') %>% str_to_title()


clean_summaries <- function(d){
  d <- rbind(tibble(Time=0, Prob=100), d) %>%
    mutate(Prob = Prob/100)
  return(d)
}
dat <- study_info %>% filter(pubID %!in% ids) %>%
select(armID, pubID, starts_with('surv')) %>%
gather(Time, Prob, starts_with('surv')) %>%
mutate(Time = str_extract(Time, '[0-9]+'),
       Time = as.numeric(Time)) %>%
filter(!is.na(Prob)) %>%
arrange(armID)
dat <- dat[dat$pubID %!in% (study_info %>% filter(male.only=='Y') %>% select(pubID) %>% as_vector),] %>%
select(-armID) %>% # verified all were overalls
nest(-pubID) %>%
mutate(data = map(data, ~clean_summaries(.)))
summ_data <- as.list(dat$data); names(summ_data) <- dat$pubID

metadata_for_spreadsheet <- study_info %>%
  filter(pubID %in% names(summ_data)) %>%
  filter(armID==pubID) %>%
  select(pubID, number, deaths, max.f.up, start_of_study, end_of_study) %>%
  mutate(n.death = round(number*deaths/100),
         max.f.up = round(max.f.up/12),
         max.f.up = ifelse(is.na(max.f.up), end_of_study-start_of_study, max.f.up))

IPD_from_spreadsheet <- list()
for(u in names(summ_data)){
  print(u)
  summData <- summ_data[[u]]
  summData[1,] <- c(0.001,0.999)
  params <- as.list(weibEst(summData))
  meta <- metadata_for_spreadsheet %>% filter(pubID==u)
  IPD_from_spreadsheet[[u]] <-
    summ2IPD_2(summData, params,
               meta$number,
               meta$n.death,
               meta$max.f.up)
}

# Pool all IPD and store --------------------------------------------------

summaries2IPD <- c(IPD_from_csv, IPD_from_spreadsheet)
save(summaries2IPD, file='data/rda/summaries2IPD.rda', compress=T)

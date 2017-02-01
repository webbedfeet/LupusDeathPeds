## identifying and adjusting CSV file names to reflect studies

source('lib/reload.R')
reload()
source('preamble.R')
csvfiles = dir(datadir, pattern='csv')
# if(length(csvfiles)==0 & datadir==AD_dirs_mac['data']) stop("Attach drive")
csvfiles <- csvfiles[!str_detect(csvfiles,'need')]
csvfiles <- csvfiles[!str_detect(csvfiles,'child')]
csvfiles <- csvfiles[!str_detect(csvfiles,'cts')]
csvfiles <- csvfiles[!str_detect(csvfiles,'ignore')]

csv_summaries <- csvfiles[str_detect(csvfiles,'summ')]
csv_cumhaz <- csvfiles[str_detect(csvfiles,'cumhaz')]
csv_cummort <- csvfiles[str_detect(csvfiles, 'cummort')]

csv_km <- setdiff(csvfiles, c(csv_summaries, csv_cumhaz, csv_cummort))

save(csv_km, csv_summaries, csv_cumhaz, csv_cummort, file='data/rda/graph_types.rda')

ids_from_filenames <- csvfiles %>%
  str_replace( '-([0-9]{4})','_\\1') %>%
  str_extract('^[\\w -]+_[0-9]{4}[abc]?') %>%
  str_to_title(.)
figs_from_filenames <- csvfiles %>%
  str_extract('[Ff]ig[0-9 ]+') %>% str_trim()
fig_metadata <- tibble(filename=csvfiles, ids = ids_from_filenames,
                       figs = figs_from_filenames)

## Do we need to pool data
bl <- fig_metadata %>% dplyr::count(ids, figs) %>%
  mutate(needs.pooling=ifelse(n==1,'No','Yes'))
fig_metadata <- fig_metadata %>% left_join(select(bl, -n))

## ID true KM files
fig_metadata <- fig_metadata %>%
  mutate(is.KM = ifelse(filename %in% csv_km, 'Yes', 'No'))


## Some meta-data on the png files
## We need to see if curves started at diagnosis, and
## ensure that time is recorded in months
pngfiles = dir(datadir, pattern='png')
pngfiles %>%
  str_replace('-([0-9]{4})','_\\1') %>%
  str_extract('^[\\w -]+_[0-9]{4}[abc]?') %>%
  str_to_title() -> png_id
png <- tibble(pngfiles, png_id)
png %>%
  rbind(data.frame(pngfiles = rep('Urman-1977-fig1.png',2),
                   png_id = paste0('Urman_1977',c('a','b')))) %>%
  filter(png_id != 'Urman_1977')-> png

bl = fig_metadata %>% left_join(png, by=c('ids'='png_id'))
bl$pngfiles[bl$filename=='Ballou-1982-fig2-summ.csv'] <- 'Ballou-1982-fig2.png'
bl$pngfiles[bl$filename=='Ballou-1982-fig3-summ.csv'] <- 'Ballou-1982-fig3.png'
bl <- distinct(bl)
# write.csv(bl, file='pngfiles.csv', row.names=F) # Add meta-data to this file
# don't write this file again...metadata is already included

png_info <- read_csv('pngfiles.csv') %>%
  mutate(`from SLE` = ifelse(is.na(`from SLE`), 'n','y'),
         years = ifelse(is.na(years),'y','n')) %>%
  dplyr::rename(TimeInYears = years)
fig_metadata <- left_join(fig_metadata, png_info)

save(fig_metadata, file='data/rda/fig_metadata.rda')

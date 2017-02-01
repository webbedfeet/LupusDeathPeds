# Extracting results from moving average analysis
source('lib/reload.R')
reload()
load('data/rda/window_membership.rda')
load('data/rda/study_info.rda')

titles <- c('adult' = 'All data',
            'adult_10' = 'Restricted to 10 years',
            'inception' = 'Inception cohorts')
pdf(file = 'graphs/adultMA.pdf')
for(x in paste('rda',c('adult','adult_10','inception'), sep='/')){
plt <- x %>% mcmcout() %>%
  collapseResults() %>%
  pltResults()
print(plt+ggtitle(titles[str_replace(x, 'rda/','')]))
}
dev.off()


# Extracting results from moving average analysis
source('lib/reload.R')
reload()
load('data/rda/window_membership.rda')
load('data/rda/study_info.rda')

pdf(file = 'graphs/pedsMA.pdf')
plt <- 'rda/peds' %>% mcmcout() %>%
  collapseResults() %>%
  pltResults()
print(plt)

dev.off()


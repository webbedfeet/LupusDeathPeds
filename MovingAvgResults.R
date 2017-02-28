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

'%!in%' <- Negate('%in%')
pdf(file = 'graphs/pedsdevelopingMA.pdf')
results <- 'peds_developing' %>% mcmcout() %>%
  collapseResults() %>% filter(Dev=='Developed') %>%
  mutate(Dev = 'Developing countries') %>%
  mutate_each(funs(ifelse(yr %in% c(1988,1989),NA, .)), LB, Med, UB)
pltResults(results)
dev.off()

pdf(file = 'graphs/pedsdevelopingMA_10.pdf')
results <- 'peds_developing_10' %>% mcmcout() %>%
  collapseResults() %>% filter(Dev=='Developed') %>%
  mutate(Dev = 'Developing countries') %>%
  mutate_each(funs(ifelse(yr %in% c(1988,1989),NA, .)), LB, Med, UB)
pltResults(results)
dev.off()

# Compute 2008-2014 summary

bl <- pooledCR(2008,2016)

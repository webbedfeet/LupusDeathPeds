# Extracting results from moving average analysis
source('lib/reload.R')
reload()
load('data/rda/window_membership.rda')
load('data/rda/study_info.rda')


'%!in%' <- Negate('%in%')
pdf(file = 'graphs/pedsdevelopingMA.pdf')
plt <- 'peds_developing' %>% mcmcout() %>%
  collapseResults() %>% filter(Dev=='Developed') %>%
  mutate(Dev = 'Developing countries') %>%
  mutate_each(funs(ifelse(yr %in% c(1988,1989),NA, .)), LB, Med, UB) %>%
  pltResults()
print(plt)
dev.off()

pdf(file = 'graphs/pedsdevelopingMA_10.pdf')
plt <- 'peds_developing_10' %>% mcmcout() %>%
  collapseResults() %>% filter(Dev=='Developed') %>%
  mutate(Dev = 'Developing countries') %>%
  mutate_each(funs(ifelse(yr %in% c(1988,1989),NA, .)), LB, Med, UB) %>%
  pltResults()
print(plt)
dev.off()

pdf(file = 'graphs/pedsdevelopedMA.pdf')
plt <- 'peds_developed' %>% mcmcout() %>%
  collapseResults() %>%
  filter(Dev=='Developed') %>%
  mutate(Dev = 'Developed countries') %>%
  pltResults()
print(plt)
dev.off()

pdf(file = 'graphs/pedsdevelopedMA_10.pdf')
plt <- 'peds_developed_10' %>% mcmcout() %>%
  collapseResults() %>%
  filter(Dev=='Developed') %>%
  mutate(Dev = 'Developed countries') %>%
  pltResults()
print(plt)
dev.off()

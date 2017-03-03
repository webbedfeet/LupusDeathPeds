
# Staircase plots ---------------------------------------------------------

source('lib/reload.R')
reload()

load('data/rda/final_study_info.rda')
plt_overall <- study_info %>%
  mutate(pubID = pubID %>% str_replace('_', ' (') %>% paste0(')') %>% str_replace('_','')) %>%
  mutate(yr_of_study_end = end_of_study_10) %>%
  stairdata %>%
  stairplot() + ggtitle('Overall')

plt_developed <- study_info %>%
  mutate(pubID = pubID %>% str_replace('_', ' (') %>% paste0(')') %>% str_replace('_','')) %>%
  mutate(yr_of_study_end = end_of_study_10) %>%
  filter(Developed == 'Developed') %>%
  stairdata %>%
  stairplot() + ggtitle('Developed countries')

plt_developing <- study_info %>%
  mutate(pubID = pubID %>% str_replace('_', ' (') %>% paste0(')') %>% str_replace('_','')) %>%
  mutate(yr_of_study_end = end_of_study_10) %>%
  filter(Developed == 'Developing') %>%
  stairdata %>%
  stairplot() + ggtitle('Developing countries')

pdf('graphs/stairplot_peds.pdf')
print(plt_overall)
print(plt_developed)
print(plt_developing)
dev.off()

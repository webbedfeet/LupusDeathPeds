
# Staircase plots ---------------------------------------------------------

source('lib/reload.R')
reload()

load('data/rda/final_study_info.rda')

pdf('graphs/stairplot_peds.pdf')
plt <- study_info %>%
  mutate(yr_of_study_end = end_of_study) %>%
  stairdata %>%
  stairplot()
print(plt)
dev.off()

pdf('graphs/stairplot_peds_10.pdf')
plt <- study_info %>%
  mutate(yr_of_study_end = end_of_study_10) %>%
  stairdata() %>%
  stairplot()
print(plt)
dev.off()

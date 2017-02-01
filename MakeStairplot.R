# Creating stairplot for study

source('lib/reload.R')
reload()
load('data/rda/study_info.rda')

filter_overall <- function(d){
  if(any(str_detect(d$Arm, '[abc]$'))){
    d <- filter(d, Arm!='')
  }
  return(d)
}

# Remove male-only studies and Grigor

info <- study_info %>% filter(armID==pubID)
info <- rbind(info,
              study_info %>% filter(!(pubID %in% info$pubID))) %>% 
  nest(-Author) %>% 
  mutate(data = map(data, ~filter_overall(.))) %>% 
  unnest() %>% 
  filter(male.only=='N') %>% 
  filter(pubID != 'Grigor_1978')

plt <- info %>% 
  mutate(pubID = pubID %>% str_replace('_',' (') %>% paste0(')')) %>% 
  mutate(yr_of_study_end = end_of_study) %>% 
  stairdata() %>% 
  stairplot() + ggtitle('Overall')

plt_developed <- info %>% 
  mutate(pubID = pubID %>% str_replace('_',' (') %>% paste0(')'), 
         yr_of_study_end = end_of_study) %>% 
  filter(Developed=='Developed') %>% 
  stairdata() %>% 
  stairplot()+ggtitle('Developed countries')

plt_developing <- info %>% 
  mutate(pubID = pubID %>% str_replace('_',' (') %>% paste0(')'), 
         yr_of_study_end = end_of_study) %>% 
  filter(Developed=='Developing') %>% 
  stairdata() %>% 
  stairplot() + ggtitle('Developing countries')
pdf(file='graphs/stairplot.pdf')
print(plt)
print(plt_developed)
print(plt_developing)
dev.off()

plt2 <- info %>% 
  mutate(pubID = pubID %>% str_replace('_',' (') %>% paste0(')') ) %>% 
  mutate(yr_of_study_end = end_of_study_10) %>% 
  stairdata() %>% 
  stairplot()


pdf(file = 'graphs/stairplot_10yr.pdf')
print(plt2)
dev.off()

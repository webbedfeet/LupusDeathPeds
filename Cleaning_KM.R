# Reading and cleaning digitized KM curves

source('lib/reload.R')
reload()
source('preamble.R')
library(stringr)

load('data/rda/fig_metadata.rda')
load('data/rda/final_study_info.rda')

fig_metadata <- fig_metadata %>%
  mutate(male.only = ifelse(ids %in% study_info$pubID[study_info$male.only=='Y'],'Yes','No'))

KM_full <- fig_metadata %>% filter(male.only=='No', is.KM=='Yes')
Ns <- study_info$number; names(Ns) <- study_info$pubID
KM_full$N <- Ns[KM_full$ids]
KM_full <- KM_full %>% mutate(N = ifelse(needs.pooling=='Yes',NA, N))
KM_full$N[is.na(KM_full$N)] <- c(
  83,271,170,175,153, # Artim-Essen
  193,40, # Bujan
  62,43, # Cardoso
  511, 156, # Drenkard
  221, 117, 103, 176, # Fernandez
  115, 101, # Flower
  # 54, 101, 151, # Funauchi
  147, 35, # Jallouli
  64, 638, 52, # Johnson
  276, 67, # Kao
  795, 133, # Merola
  32, 645, # Mok
  150, 82, # Ruiz-Irastorza
  43, 116, # Schmid
  1141, 339, # Shinjo
  76, 4, # Stoll
  # 142,73 # Voss
  197, 211# Ward
)

# Read in the digitized data in a list

KM_digitized <- list()
KM_digitized_clean <- list()

for(u in KM_full$filename){
  print(u)
  KM_digitized[[u]] <- read.csv(file.path(datadir,u), header=F)
  names(KM_digitized[[u]]) <- c('Time','Prob')
  if(is.na(KM_full$N[KM_full$filename==u])) next
  KM_digitized_clean[[u]] <- KMclean(KM_digitized[[u]], KM_full$N[KM_full$filename==u])
}

save(KM_full, KM_digitized_clean, file='data/rda/KM_digitized.rda', compress=T)

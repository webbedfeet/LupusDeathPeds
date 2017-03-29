# Extracting results from moving average analysis
source('lib/reload.R')
reload()
load('data/rda/window_membership.rda')
load('data/rda/study_info.rda')

cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

'%!in%' <- Negate('%in%')
pdf(file = 'graphs/pedsdevelopingMA.pdf')
plt <- 'peds_developing' %>% mcmcout() %>%
  collapseResults() %>% filter(Dev=='Developed') %>%
  mutate(Dev = 'Low/Middle Income Countries') %>%
  mutate_each(funs(ifelse(yr %in% c(1988,1989),NA, .)), LB, Med, UB) %>%
  pltResults()+scale_color_manual(values=cbbPalette)
print(plt)
dev.off()

pdf(file = 'graphs/pedsdevelopingMA_10.pdf')
plt <- 'peds_developing_10' %>% mcmcout() %>%
  collapseResults() %>% filter(Dev=='Developed') %>%
  mutate(Dev = 'Low/Middle Income Countries') %>%
  mutate_each(funs(ifelse(yr %in% c(1988,1989),NA, .)), LB, Med, UB) %>%
  pltResults()+scale_color_manual(values=cbbPalette)
print(plt)
dev.off()

pdf(file = 'graphs/pedsdevelopedMA.pdf')
plt <- 'peds_developed' %>% mcmcout() %>%
  collapseResults() %>%
  filter(Dev=='Developed') %>%
  mutate(Dev = 'High Income Countries') %>%
  pltResults()#+scale_color_manual(values=cbbPalette)
print(plt)
dev.off()

pdf(file = 'graphs/pedsdevelopedMA_10.pdf')
plt <- 'peds_developed_10' %>% mcmcout() %>%
  collapseResults() %>%
  filter(Dev=='Developed') %>%
  mutate(Dev = 'High Income Countries') %>%
  pltResults()+scale_color_manual(values=cbbPalette)
print(plt)
dev.off()

# Compute 2008-2014 summary

bl <- pooledCR(2008,2016)
# library(ReporteRs)
# output <- docx() %>%
#   addParagraph(value="Pooled estimate, 2008-2016 (mortality rate)") %>%
#   addFlexTable(FlexTable(bl)) %>%
#   writeDoc(file = 'docs/pooledEstimate.docx')
# Not working on work desktop -- Java problem
output <- bl$output %>% mutate(out = paste0(Median,' (',`LCB (0.95)`,', ',`UCB (0.95)`,')')) %>%
  select(Developed, Year, out) %>%
  mutate(Year = paste0(Year, ' year')) %>%
  spread(Year, out)
devcount <- study_info %>% filter(armID==pubID, pubID %in% bl$studies) %>% count(Developed)
output <- cbind(output, n = devcount$n)[,c(1,4,3,2)]



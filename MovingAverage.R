## Running moving average Bayesian analysis

source('lib/reload.R')
source('preamble.R')
reload()
load('data/rda/final_study_info.rda')
load('data/rda/fig_metadata.rda')
load('data/rda/window_membership.rda')
load('data/rda/KM2IPD.rda')
load('data/rda/summaries2IPD.rda')
load('data/rda/followup_data.rda')


fullmodelcts.bugs <- "
model{
# Prior model & estimated quantities
for(j in 1:J){
#beta[j] ~ dnorm(0.0,0.0001);
 nu[j] ~ dgamma(3.0,3.0);
lambda[j] ~ dgamma(3.0,0.03);
#lambda[j] ~ dunif(20,200);
pr5[j] <- 1-pweib(5.0, nu[j],lambda[j]);
pr10[j] <- 1-pweib(10.0, nu[j],lambda[j]);
pr15[j] <- 1-pweib(15.0, nu[j],lambda[j]);
}
# Likelihood for IPD data
for(i in 1:N1){
isCensored1[i] ~ dinterval(td[i], tcens[i]);
td[i] ~ dweib(nu[geog1[i]], lambda[geog1[i]])T(trunc[i],);
}
# Likelihood for followup data
for(i in 1:N2){
p[i] <- pweib(maxfollowup[i]+Lag[i], nu[geog2[i]], lambda[geog2[i]])-pweib(Lag[i],nu[geog2[i]], lambda[geog2[i]]);
isCensored2[i] ~ dinterval(Y[i], Events[i]);
Y[i] ~ dbin(p[i], n[i]);
}
}
"
writeLines(fullmodelcts.bugs, con='fullmodelcts.bug')

# Ignore follow-up data
fullmodelcts2.bugs <- "
model{
# Prior model & estimated quantities
for(j in 1:J){
#beta[j] ~ dnorm(0.0,0.0001);
nu[j] ~ dgamma(3.0,3.0);
lambda[j] ~ dgamma(3.0,0.03);
#lambda[j] ~ dunif(50,200);
pr5[j] <- 1-pweib(5.0, nu[j],lambda[j]);
pr10[j] <- 1-pweib(10.0, nu[j],lambda[j]);
pr15[j] <- 1-pweib(15.0, nu[j],lambda[j]);
}
# Likelihood for IPD data
for(i in 1:N1){
isCensored1[i] ~ dinterval(td[i], tcens[i]);
td[i] ~ dweib(nu[geog1[i]], lambda[geog1[i]])T(trunc[i],);
}
# Likelihood for followup data
# for(i in 1:N2){
# p[i] <- pweib(maxfollowup[i], nu[geog2[i]], lambda[geog2[i]])-pweib(lags[i],nu[geog2[i]], lambda[geog2[i]]);
# isCensored2[i] ~ dinterval(Y[i], Events[i]);
# Y[i] ~ dbinom(p[i], n[i]);
# }
}
"
writeLines(fullmodelcts2.bugs, con='fullmodelcts2.bug')

## Which are male-only studies
fig_metadata <- fig_metadata %>%
  mutate(male.only = ifelse(ids %in% study_info$pubID[study_info$male.only=='Y'],'Yes','No'))

ipds <- KM2IPD[setdiff(names(KM2IPD), fig_metadata$ids[fig_metadata$male.only=='Yes'])]
ipds <- c(ipds,
          summaries2IPD[setdiff(names(summaries2IPD), study_info$pubID[study_info$male.only=='Y'])])

## Use study_info since summaries2IPD contains data from spreadsheet, not just graphical

## Remove suspicious study which is killing JAGS, Manger_2002. This does solve problem.
# ipds <- ipds[-which(names(ipds)=='Manger_2002')]
createDatasets(membership, ipds, outdir='adult', minkm=2, info=study_info, followup=fup_data)

load('data/rda/window_membership_10.rda')
createDatasets(membership_10,ipds,outdir='adult_10', minkm = 2)


## Create datasets for inception cohorts only
ids = names(ipds)
inception_ids = filter(study_info, inception==1)$pubID

ipds_inception <- ipds[intersect(ids, inception_ids)]
membership_inception <- membership %>% filter(pubID %in% inception_ids)
createDatasets(membership_inception, ipds_inception, outdir='inception', minkm=2,
               followup = fup_data %>% filter(pubID %in% inception_ids))


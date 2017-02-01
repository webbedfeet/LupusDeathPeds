#' Send data to JAGS
#'
#' Given a data set that is formated for ingestion into JAGS, e.g., the 
#' output from datForJags, this function wraps rjags and sets some defaults for 
#' parameters to be sent to rjags. The JAGS files are hardcoded here for the 
#' particular application we are considering here, but this can certainly be
#' adapted.
#' @param dat Data in the form of a list, conforming to the JAGS program being run.
#' This list is essentially in the form of the output from a dump() statement
#' @param  nadapt=1000 Number of adaptation iterations in JAGS
#' @param  niter=1000 Number of interations of JAGS
#' @param  nchains=1 Number of chains to run
#' @param  thin=1 How much to thin the chains by
#' @return A mcmc.list object giving the results of the JAGS run. This essentially
#' gives the sampled posterior distribution for each parameter of interest.
runjags <- function(dat, nadapt=1000, niter=1000, nchains=1, thin=1){
  require(rjags)
  load.module('glm')
  load.module('lecuyer')
  # source('fullmodel.mixed.txt')
  if(dat$N2 > 0){
    inits <- with(dat, 
                  list(lambda = rep(1,J),
                       nu = rep(1, J),
                       .RNG.name = "lecuyer::RngStream",
                       .RNG.seed = sample(1:10000,1),
                       td = ifelse(isCensored1==1, 100, NA), # init for censored td
                       Y = ifelse(isCensored2==1, n, NA)# init for censored Y
                       #                      ypred14 = 100,
                       #                      ypred24 = 100
                  )
    )
    
    parameters <- c('lambda','nu','pr5','pr10','pr15')
    
    mod <- jags.model("fullmodelcts.bug",
                      data = dat,
                      inits = inits,
                      n.chains = nchains, # Change to 4 after testing
                      n.adapt = nadapt)
    #update(mod, n.iter=1000) # Burn-in
    codaSamples <- coda.samples(mod, variable.names=parameters, 
                                n.iter=niter, thin=thin)
  } else {
    inits <- with(dat, 
                  list(lambda = rep(1,J),
                       nu = rep(1, J),
                       .RNG.name = "lecuyer::RngStream",
                       .RNG.seed = sample(1:10000,1),
                       td = ifelse(isCensored1==1, 100, NA) # init for censored td
                       #Y = ifelse(isCensored2==1, n, NA)# init for censored Y
                       #                      ypred14 = 100,
                       #                      ypred24 = 100
                  )
    )
    
    parameters <- c('lambda','nu','pr5','pr10','pr15')
    
    mod <- jags.model("fullmodelcts2.bug",
                      data = dat,
                      inits = inits,
                      n.chains = nchains, # Change to 4 after testing
                      n.adapt = nadapt)
    #update(mod, n.iter=1000) # Burn-in
    codaSamples <- coda.samples(mod, variable.names=parameters, 
                                n.iter=niter, thin=thin)
  }
  return(codaSamples)
}

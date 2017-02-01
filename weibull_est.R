weibull_est <- function(n){
  source(file.path('data','mcmc','adult',paste0('fullmodel',n,'.txt')))
  eval(parse(text = paste0('dat = data',n)))

  bl <- data.frame(td = dat$td, tcens = dat$tcens)
  s <- Surv(ifelse(is.na(bl$td),bl$tcens, bl$td),
            ifelse(is.na(bl$td),0,1))
  sfit=survfit(s~1)
  z = tidy(sfit)
  # return(sfit)
  m <- lm(log(-log(estimate))~log(time), data=z %>% filter(time>0, estimate < 1))
  nu = coef(m)[2]
  lambda = exp(-coef(m)[1]/coef(m)[2])
  return(data.frame(nu=nu, lambda=lambda))
}

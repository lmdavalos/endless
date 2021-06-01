##libraries
library(brms)

##remove prior data
rm(list=ls()) 

##get data
load("bayes_spa_data.RData")

##start Bayesian SEM modeling
##use only formulae from piecewise
##focus on change, not size of forest

##priors based on https://rdrr.io/cran/brms/man/set_prior.html
## and on https://www.rensvandeschoot.com/tutorials/brms-priors/
##prior to help forest and coca
##space and time splines thanks to
##initial idea https://discourse.mc-stan.org/t/space-time-models-in-stan-brms/4735/8
##set up mrf https://fromthebottomoftheheap.net/2017/10/19/first-steps-with-mrf-smooths/
##combine terms https://stats.stackexchange.com/questions/363440/how-does-markov-random-field-bs-mrf-in-mgvc-gam-handle-repeated-measures-on-th
##with correct nb this is the set up: t2(year, MPIOS, bs = c("fs", "mrf"), xt = list(year = NULL, MPIOS = list(nb = nb)))

##the smoths are sweet!

##will the right scale break the models?
##let's find out
dat4$cult <- as.integer(dat4$coca-1)
dat4$conf <- as.integer(with(dat4, conflict * pop/1000))

##forest 
fore_mod <- bf(betfor ~ t2(year, MPIOS, bs = c("fs", "mrf"), xt = list(year = NULL, MPIOS = list(nb = nb))) + log10(Forest) + log10(Grasslands) + log10(Croplands) + betgra + log10(conf + 1) + log10(inf_lag + 1) + log10(AREA))

##coca
coca_mod <- bf(cult ~ t2(year, MPIOS, bs = c("fs", "mrf"), xt = list(year = NULL, MPIOS = list(nb = nb))) + log10(Forest) + log10(Grasslands) + log10(Croplands) + betgra + betcro + log10(inf_lag + 1) + pru + betpou + betcoc + log10(asp_lag + 1) + log10(era_lag + 1) + log10(AREA))

##conflict
conf_mod <- bf(conf ~  t2(year, MPIOS, bs = c("fs", "mrf"), xt = list(year = NULL, MPIOS = list(nb = nb))) + log10(Forest) + log10(Croplands) + log10(inf_lag + 1) + log10(popr) + pru + betpou + betcoc + log10(cult + 1) + log10(asp_lag + 1) + log10(era_lag + 1))

##run the models all together
szz_mrf_brms <- brm(fore_mod +
                  coca_mod +
                  conf_mod +
                  set_rescor(FALSE), 
                  family = list("student", 
zero_inflated_negbinomial(link_zi = "logit"), 
zero_inflated_negbinomial(link_zi = "logit")), 
                  data2 = list(nb = nb),
                  #iter  = 6000, warmup = 3000, thin = 10,
                  iter  = 2000, warmup = 800, thin = 5, 
                  control = list(adapt_delta = 0.9, max_treedepth = 50),
                  data = dat4,
                  cores = 4, chains = 4)
                
##save                
save.image("szz_mrf_brms_v3.RData")

##remove all data
rm(list=ls())

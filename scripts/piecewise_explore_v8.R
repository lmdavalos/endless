##libraries
library(piecewiseSEM) 
library(nlme)
library(lme4)  
 
##remove prior data
rm(list=ls()) 

##load data
load("bayes_spa_data.RData")

##start SEM modeling
##for nesting see https://rpsychologist.com/r-guide-longitudinal-lme-lmer##

##will the right scale break the models?
##let's find out
dat4$cult <- as.integer(dat4$coca-1)
dat4$conf <- as.integer(with(dat4, conflict * pop/1000))

##start with all covars
mi1 <- psem(lme(betfor ~ log10(Forest) + log10(Grasslands) + log10(Croplands) + betgra + betcro + log10(inf_lag+1) + log10(popr) + pru + betpou + betpro + log10(cult+1) + betcoc + log10(asp_lag + 1) + log10(era_lag + 1) + log10(conf+1)  + log10(AREA) + year, random = list(~ 1| MPIOS, ~ 1| year), control = lmeControl(opt = "optim"), data = dat4),
lme(log10(cult+1) ~ year, random = list(~ 1| MPIOS, ~ 1| year), control = lmeControl(opt = "optim"), data = dat4),  
lme(log10(conf+1) ~ year, random = list(~ 1| MPIOS, ~ 1| year), control = lmeControl(opt = "optim"), data = dat4), data = dat4)
  
##not a lot of R^2
sink("piecewise_v8.txt")
print(summary(mi1, .progressBar=F, conserve = TRUE))
sink()

##now add all the vars from the d-sep tests and take out covars p >0.1
mi2 <- psem(lme(betfor ~ log10(Forest) + log10(Grasslands) + log10(Croplands) + betgra + log10(conf+1) + log10(inf_lag+1) + log10(AREA) + year, random = list(~ 1| MPIOS, ~ 1| year), control = lmeControl(opt = "optim"), data = dat4),
lme(log10(cult+1) ~ log10(Forest) + log10(Grasslands) + log10(Croplands) + betcro + log10(popr) + pru + betpou + betcoc + log10(asp_lag + 1) + log10(era_lag + 1) + log10(AREA) + year, random = list(~ 1| MPIOS, ~ 1| year), control = lmeControl(opt = "optim"), data = dat4),  
lme(log10(conf+1) ~ log10(Forest) + log10(Grasslands) + log10(Croplands) + betcro + log10(popr) + pru + betpro + log10(cult + 1) + log10(asp_lag + 1) + log10(AREA) + year, random = list(~ 1| MPIOS, ~ 1| year), control = lmeControl(opt = "optim"), data = dat4), data = dat4)
  
##much better but low explanatory power for betfor
sink("piecewise_v8.txt", append=T)
print(summary(mi2, .progressBar=F))
sink()

##and again
mi3 <- psem(lme(betfor ~ log10(Forest) + log10(Grasslands) + log10(Croplands) + betgra + log10(conf+1) + log10(inf_lag+1) + log10(AREA) + year, random = list(~ 1| MPIOS, ~ 1| year), control = lmeControl(opt = "optim"), data = dat4),
lme(log10(cult+1) ~ log10(Forest) + log10(Grasslands) + log10(Croplands) + betgra + betcro + log10(inf_lag + 1) + pru + betpou + betcoc + log10(asp_lag + 1) + log10(era_lag + 1) + log10(AREA), random = list(~ 1| MPIOS, ~ 1| year), control = lmeControl(opt = "optim"), data = dat4),  
lme(log10(conf+1) ~ log10(Forest) +log10(Croplands) + log10(popr) + pru + betcoc + log10(cult + 1) + log10(asp_lag + 1) + log10(era_lag + 1) + year, random = list(~ 1| MPIOS, ~ 1| year), control = lmeControl(opt = "optim"), data = dat4), data = dat4)
  
##getting closer
sink("piecewise_v8.txt", append=T)
print(summary(mi3, .progressBar=F, conserve = TRUE))
sink()

##and again
mi4 <- psem(lme(betfor ~ log10(Forest) + log10(Grasslands) + log10(Croplands) + betgra + log10(conf+1) + log10(inf_lag+1) + log10(AREA) + year, random = list(~ 1| MPIOS, ~ 1| year), control = lmeControl(opt = "optim"), data = dat4),
lme(log10(cult+1) ~ log10(Forest) + log10(Grasslands) + log10(Croplands) + betgra + betcro + log10(inf_lag + 1) + pru + betpou + betcoc + log10(asp_lag + 1) + log10(era_lag + 1)  + log10(AREA), random = list(~ 1| MPIOS, ~ 1| year), control = lmeControl(opt = "optim"), data = dat4),  
lme(log10(conf+1) ~ log10(Forest) + log10(Croplands) + log10(popr) + pru + betpou + betcoc + log10(cult + 1) + log10(asp_lag + 1) + log10(era_lag + 1) + year, random = list(~ 1| MPIOS, ~ 1| year), control = lmeControl(opt = "optim"), data = dat4), data = dat4)
  
##this runs but betfor is definitely not explainable with these data, lol
sink("piecewise_v8.txt", append=T)
print(summary(mi4, .progressBar=F, conserve = TRUE))
sink()

##and again
mi5 <- psem(lme(betfor ~ log10(Forest) + log10(Grasslands) + log10(Croplands) + betgra + log10(conf+1) + log10(inf_lag+1) + log10(AREA) + year, random = list(~ 1| MPIOS, ~ 1| year), control = lmeControl(opt = "optim"), data = dat4),
lme(log10(cult+1) ~ log10(Forest) + log10(Grasslands) + log10(Croplands) + betgra + betcro + log10(inf_lag + 1) + pru + betpou + betcoc + log10(asp_lag + 1) + log10(era_lag + 1) + log10(AREA), random = list(~ 1| MPIOS, ~ 1| year), control = lmeControl(opt = "optim"), data = dat4),  
lme(log10(conf+1) ~ log10(Forest) + log10(Croplands) + log10(inf_lag + 1) + log10(popr) + pru + betpou + betcoc + log10(cult + 1) + log10(asp_lag + 1) + log10(era_lag + 1) + year, random = list(~ 1| MPIOS, ~ 1| year), control = lmeControl(opt = "optim"), data = dat4), data = dat4)
  
##this runs but betfor is definitely not explainable with these data, lol
sink("piecewise_v8.txt", append=T)
print(summary(mi5, .progressBar=F, conserve = TRUE))
sink()

closeAllConnections()

##save
save.image("piece_v8.RData")
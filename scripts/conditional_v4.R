##libraries
library(reshape2)
library(ggplot2)
library(brms)
library(scales)

##remove prior data
rm(list=ls()) 

##get data
##analyses of the models with infrastructure and betcon on betfor
##snn mrf
load("snn_mrf_brms_v3.RData")

##plot conditional effects of various predictors
##time
m0<-conditional_effects(snn_mrf_brms, "year")

##save
save.image("conditional_v4.RData")

##plot conditional effects of various predictors
m1<-conditional_effects(snn_mrf_brms, "Forest")

##save
save.image("conditional_v4.RData")

m2<-conditional_effects(snn_mrf_brms, "Grasslands")

##save
save.image("conditional_v4.RData")

m3<-conditional_effects(snn_mrf_brms, "Croplands")

##save
save.image("conditional_v4.RData")

m4<-conditional_effects(snn_mrf_brms, "betgra")

##save
save.image("conditional_v4.RData")

m5<-conditional_effects(snn_mrf_brms, "conf")

##save
save.image("conditional_v4.RData")

m6<-conditional_effects(snn_mrf_brms, "inf_lag")

##save
save.image("conditional_v4.RData")

##coca and conflict have a lot in common

m7<-conditional_effects(snn_mrf_brms, "betcro")

##save
save.image("conditional_v4.RData")

m8<-conditional_effects(snn_mrf_brms, "pru")

##save
save.image("conditional_v4.RData")

m9<-conditional_effects(snn_mrf_brms, "betpou")

##save
save.image("conditional_v4.RData")

m10<-conditional_effects(snn_mrf_brms, "betcoc")

##save
save.image("conditional_v4.RData")

m11<-conditional_effects(snn_mrf_brms, "asp_lag")

##save
save.image("conditional_v4.RData")

m12<-conditional_effects(snn_mrf_brms, "era_lag")

##save
save.image("conditional_v4.RData")

m13<-conditional_effects(snn_mrf_brms, "cult")

##save
save.image("conditional_v4.RData")

m14<-conditional_effects(snn_mrf_brms, "popr")

##save
save.image("conditional_v4.RData")

m15<-conditional_effects(snn_mrf_brms, "conf")

##save
save.image("conditional_v4.RData")

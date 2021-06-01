##libraries
library(reshape2)
library(ggplot2)
library(brms)
library(scales)
library(gganimate)
library(plotKML)
library(sf)
library(transformr)
library(tidyverse)
library(gpclib)
library(plyr)

##remove prior data
rm(list=ls()) 

##get data
##analyses of the models with infrastructure and betcon on betfor
##snn mrf
load("snn_mrf_brms_v3.RData")

##get the smooths, this take <1 hr on a powerful computer
smooth<-conditional_smooths(snn_mrf_brms)

##here for animation https://gist.github.com/rafapereirabr/0d68f7ccfc3af1680c4c8353cf9ab345
##here to convert polygon to ggplot https://stackoverflow.com/questions/9466828/plotting-polygon-shapefiles-and-geom-points-with-ggplot2

##merge to get df
forvis<-merge(smooth[[1]], dat4spa[,c("MPIOS", "x", "y")], by="MPIOS", all.x=T)

##merge to get df
cocvis<-merge(smooth[[2]], dat4spa[,c("MPIOS", "x", "y")], by="MPIOS", all.x=T)

##merge to get df
convis<-merge(smooth[[3]], dat4spa[,c("MPIOS", "x", "y")], by="MPIOS", all.x=T)

##convert to plotable in ggplot2
d4s <- fortify(dat4spa, region="MPIOS")

##merge to maps
for_df <- left_join(d4s, forvis, by = c('id'='MPIOS'))
coc_df <- left_join(d4s, cocvis, by = c('id'='MPIOS'))
con_df <- left_join(d4s, convis, by = c('id'='MPIOS'))

##save
save.image("smooth_v4.RData")

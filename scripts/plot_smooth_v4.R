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
load("smooth_v4.RData")

##take out leticia & puerto nariño bc low estimate values
coc_df<-subset(coc_df, id !="91540")
coc_df<-subset(coc_df, id !="91001")

con_df<-subset(con_df, id !="91540")
con_df<-subset(con_df, id !="91001")

##match the forest plot bc otherwise it stand out
for_df<-subset(for_df, id !="91540")
for_df<-subset(for_df, id !="91001")

##make gif of the smoothing
p1<-ggplot(data = for_df) + geom_polygon(aes(x = long, y = lat, group = group, fill = estimate__), color="gray90", show.legend = FALSE) + theme_void() + scale_fill_distiller(palette="BrBG") + transition_time(year) + ease_aes('linear') + labs(title = 'forest growth: {frame_time}')

##save
anim_save("forest_change_v4.gif", animation=p1)

##make gif of the smoothing
p2<-ggplot(data = coc_df) + geom_polygon(aes(x = long, y = lat, group = group, fill = estimate__), color="gray90", show.legend = F) + theme_void() + scale_fill_distiller(palette="Greys") + transition_time(year) + ease_aes('linear') + labs(title = 'coca cultivation: {frame_time}') 

##save
anim_save("coca_v4.gif", animation=p2)

##make gif of the smoothing
p3<-ggplot(data = con_df) + geom_polygon(aes(x = long, y = lat, group = group, fill = estimate__), color="gray90", show.legend = F) + theme_void() + scale_fill_viridis_c(option="A", direction = -1) + transition_time(year) + ease_aes('linear') + labs(title = 'conflict: {frame_time}') 

##save
anim_save("conflict_v4.gif", animation=p3)

##remove stuff
#rm(list=setdiff(ls(), "smooth", "forvis", "cocvis", "convis", "d4s"))

##save
save.image("smooth_v4.RData")

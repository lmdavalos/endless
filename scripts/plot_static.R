##libraries
library(reshape2)
library(ggplot2)
library(scales)
library(tidyverse)
library(plyr)
library(ggthemes)

##remove prior data
rm(list=ls()) 

##get data
load("smooth_v4.RData")

##take out leticia & puerto nariño bc low estimate values
##leave only 5 time points
coc_df<-subset(coc_df, id !="91540")
coc_df<-subset(coc_df, id !="91001")
coc_df<-coc_df[coc_df$year %in% unique(coc_df$year)[seq(1,100,33)], ]

con_df<-subset(con_df, id !="91540")
con_df<-subset(con_df, id !="91001")
con_df<-con_df[con_df$year %in% unique(con_df$year)[seq(1,100,33)], ]

##match the forest plot bc otherwise it stand out
for_df<-subset(for_df, id !="91540")
for_df<-subset(for_df, id !="91001")
for_df<-for_df[for_df$year %in% unique(for_df$year)[seq(1,100,33)], ]

##make plot
p1<-ggplot(data = for_df) + geom_polygon(aes(x = long, y = lat, group = group, fill = estimate__, lwd = 0)) + theme_void() + scale_fill_viridis_c(option="D") + facet_grid(cols = vars(year))

##save
ggsave("forest_change_v1.pdf", h = 4, w = 13)

##make plot
p2<-ggplot(data = coc_df) + geom_polygon(aes(x = long, y = lat, group = group, fill = estimate__ , lwd = 0)) + theme_void() + scale_fill_viridis_c(option="C", direction = -1) + facet_grid(cols = vars(year))

##save
ggsave("coca_v1.pdf", h = 4, w = 13)

##make plot
p3<-ggplot(data = con_df) + geom_polygon(aes(x = long, y = lat, group = group, fill = estimate__, lwd = 0)) + theme_void() + scale_fill_viridis_c(option="A", direction = -1) + facet_grid(cols = vars(year))

##save
ggsave("conflict_v1.pdf", h = 4, w = 13)

##make a good map
##make dataset
map_df<-subset(con_df, year==2002.000)

##change id
map_df$MPIOS<-gsub("^0", "", map_df$id)

##make a map with regions
reg<- read.csv("region_edited.csv")

##merge
map_df<-merge(map_df, reg, by='MPIOS')
map_df<-map_df[order(map_df$group),]

##make plot
p4<-ggplot(data = map_df) + geom_polygon(aes(x = long, y = lat, group = group, fill = Region)) + theme_map() + scale_fill_viridis_d(option="B", direction = -1, begin = 0.25) 

##save
ggsave("basemap_v1.pdf", h = 6.5, w = 6)

##remove stuff
rm(list=setdiff(ls(), c("coc_df", "con_df", "for_df", "map_df", "p1", "p2", "p3", "p4")))

##save
save.image("smooth_v5.RData")

##libraries
library(reshape2)
library(dplyr)
library(sp)
library(spdep)
library(maptools)
library(surveillance)
library(nlme)

##remove prior data
rm(list=ls()) 

##get forest data
a01<-read.csv("a2001.csv")
a02<-read.csv("a2002.csv")
a03<-read.csv("a2003.csv")
a04<-read.csv("a2004.csv")
a05<-read.csv("a2005.csv")
a06<-read.csv("a2006.csv")
a07<-read.csv("a2007.csv")
a08<-read.csv("a2008.csv")
a09<-read.csv("a2009.csv")
a10<-read.csv("a2010.csv")
a11<-read.csv("a2011.csv")
a12<-read.csv("a2012.csv")
a13<-read.csv("a2013.csv")
a14<-read.csv("a2014.csv")
a15<-read.csv("a2015.csv")
a16<-read.csv("a2016.csv")
a17<-read.csv("a2017.csv")

##make into single dataset
forest<-rbind(a01,a02,a03,a04,a05,a06,a07,a08,a09,a10,a11,a12,a13,a14,a15,a16,a17)

##the name of the MPIOS column in forest is super weird in Clara's R. You can comment this line out if it's not weird in yours. 
#names(forest)[names(forest) == "ï..MPIOS"] <- "MPIOS"

##assign year
forest$year<-c(rep(2001, 1118), rep(2002, 1118), rep(2003, 1118), rep(2004, 1118), rep(2005, 1118), rep(2006, 1118), rep(2007, 1118), rep(2008, 1118), rep(2009, 1118), rep(2010, 1118), rep(2011, 1118), rep(2012, 1118), rep(2013, 1118), rep(2014, 1118), rep(2015, 1118), rep(2016, 1118), rep(2017, 1118) )

## get eleo's panel
dat<-read.csv("dataset_paper3_v2.csv")

## maybe this is a cleaner labeled table?
d1<-read.csv("data3paper.csv")

## merge with better labels
dat1<-merge(dat, d1, by.x="idmun", by.y="MPIOS")

##clean up 
dat1$state<-NULL
dat1$municipality<-NULL

##brms does not like underscores in its formula
colnames(dat1)<-gsub('\\_$', "", colnames(dat1))

##Antioquia and Atlantico mpios had mismatch in id fix in newid
dat1$newid<-with(dat1, ifelse(NOMBRE_DPT=="ANTIOQUIA", gsub("^5", "05", newid), newid))
dat1$newid<-with(dat1, ifelse(NOMBRE_DPT=="ATLANTICO", gsub("^8", "08", newid), newid))

##merge with forest data
##cannot do all.y bc this leads to empty rows of population which we need
dat2<-merge(dat1, forest, by.x=c('newid','year'), by.y=c('MPIOS','year'), all.x=T)

##Order for computing lag
dat2<-dat2[order(dat2$year,dat2$newid),]

##all covers need a minimum quantity added to avoid division by 0 issues
##the resolution of this one is 1 using 1 to make hurdle lognormal model downstream
dat2$coca<-dat2$coca+1

##the resolution here is 25
dat2$Forest<-dat2$Forest+25
dat2$Grasslands<-dat2$Grasslands+25
dat2$Croplands<-dat2$Croplands+25

##compute lag values
##important lags asp, er, coca
dat2<-mutate(dat2, asp_lag = lag(asp,  n = 1116))
dat2<-mutate(dat2, era_lag = lag(er, n = 1116))
dat2<-mutate(dat2, coc_lag = lag(coca, n = 1116))

##lag land covers
dat2<-mutate(dat2, for_lag = lag(Forest, n = 1116))
dat2<-mutate(dat2, gra_lag = lag(Grasslands, n = 1116))
dat2<-mutate(dat2, cro_lag = lag(Croplands, n = 1116))

##lag population
dat2<-mutate(dat2, pop_lag = lag(pop, n = 1116))
dat2<-mutate(dat2, por_lag = lag(popr, n = 1116))
dat2<-mutate(dat2, pou_lag = lag(popu, n = 1116))

##infrastructure
dat2<-mutate(dat2, inf_lag = lag(infrastructurepcc, n = 1116))

##read Regions in
r1<-read.csv("Regions.csv")

##brms hates underscores in vars & clean up regions
r1$region <- factor(r1$Region_natural)
r1$region<-gsub('Pac\xedfico', "Pacifico", r1$region)

##merge dat2 with regions
dat2r<-merge(dat2, r1, by.x=c('idmun'), by.y=c('MPIOS'), all.x=T)

##read in the mpio shapefile
mpios<-rgdal::readOGR("./mpio/mpio.shp")

##merge our database with spatial
##must use only one year otherwise chokes with Error in fix.by(by.x, x) : 'by' must specify a uniquely valid column
##there are more mpios than dat2, so all.x=F
dat2spa<-merge(mpios, subset(dat2r, year=="2002"), by.x="MPIOS", by.y="newid", all.x=F)

##after merging need a spatial reference code for downstream matching to the neighbour matrix
dat2spa$label<-seq(from=0, to=dim(dat2spa)[1]-1)

##convert to neighboring weights matrix, necessary for car
nb2.mat <- poly2adjmat(dat2spa, zero.policy = TRUE)

##get coordinates for nlme
dat2spa$x <- coordinates(dat2spa)[, 1]/1000
dat2spa$y <- coordinates(dat2spa)[, 2]/1000

##must now add a label column to the entire dataframe with many years of data
##take columns from dat2spa so they match
mbit<-dat2spa@data[,c("MPIOS", "label", "x", "y")]

##now merge so the whole data set has a label value
dat2r<-merge(mbit, dat2r, by.x="MPIOS", by.y="newid")

##this data set to use forest lags
dat3<-dat2r[!is.na(dat2r$for_lag),]

##now sum the land uses so that we can figure out which mpios to keep
d2<-aggregate(Forest ~MPIOS, dat3, sum)
d3<-aggregate(Grasslands ~MPIOS, dat3, sum)

##keep only mpios with forest at some point in time
##about 3K mpios fall out
d2<-subset(d2, Forest>25 * length(unique(dat3$year)))
d3<-subset(d3, Grasslands>25 * length(unique(dat3$year)))

##merge the two keeping only overlap
d4<-merge(d2, d3, all=F) 

##trouble is land covers generate issues with artificial 0 where no land cover was observed
##focus on areas with grass and forest, lose about 5K rows
dat3<-dat3[dat3$MPIOS %in% d4$MPIOS, ]

##compute change in forest cover as a point percent change of total area
##as with delta coca this variable is barely explained by anything...
##changed to puyravaud
dat3$betfor<-log(dat3$Forest/dat3$for_lag)
dat3$betcro<-log(dat3$Croplands/dat3$cro_lag)
dat3$betgra<-log(dat3$Grasslands/dat3$gra_lag)

##compute population changes
##total
dat3$betpop<-log(dat3$pop/dat3$pop_lag)
##rural
dat3$betpor<-log(dat3$popr/dat3$por_lag)
##urban
dat3$betpou<-log(dat3$popu/dat3$pou_lag)
##proportion rural
dat3$pru<-dat3$popr/dat3$pop
##change in rural fraction, mostly negative
dat3$betpro<-log((dat3$popr/dat3$pop)/(dat3$por_lag/dat3$pop_lag))

##compute change in coca but first add a tiny constant to avoid dividing by 0, see above
dat3$betcoc<-log(dat3$coca/dat3$coc_lag)

##compute conflict for zero inflated Poisson
##scrap Poisson, use this distribution as it works better
dat3$betcon<-log10(dat3$conflict+.001)+3

##clean up NA
##this stays as is, it's a new mpio
dat3[is.infinite(dat3$betpop),"betpop"]<-0
dat3[is.infinite(dat3$betpor),"betpor"]<-0
dat3[is.na(dat3$betpro),"betpro"]<-0

##and places withou urban pop
dat3[is.na(dat3$betpou),"betpou"]<-0
dat3[is.infinite(dat3$betpou),"betpou"]<-0

##this data set to use infrastructure lags
dat4<-dat3[!is.na(dat3$inf_lag),]

##deletion set
del<-setdiff(rownames(nb2.mat), dat4$label)

##new matrix without the missing mpios
nb.mat <- nb2.mat[!rownames(nb2.mat) %in%  del, !colnames(nb2.mat) %in% del]

##take in spending data
spen<-read.csv("20002010DNPSpending.csv")

##get rid of extraneous years
spen<-subset(spen, year>2002)

##merge so all data from  dat4 remains
dat5<-merge(dat3, spen[,c("codigo", "ViaVTotalCb08", "year")], by.x=c("idmun", "year"), by.y=c("codigo", "year"), all.x=T)

##Order for computing lag
dat5<-dat5[order(dat5$year,dat5$idmun),]

##subset to the right years
dat5<-subset(dat5, year<2012)
dat5<-subset(dat5, year>2002)

##fill in missing mpios with 0 spending 
dat5[is.na(dat5$ViaVTotalCb08),]$ViaVTotalCb08<-0

##compute lag values for vias spending
##important lags asp, er, coca
dat5<-mutate(dat5, vialag = lag(ViaVTotalCb08,  n = 1116))

##keep only years with available spending
dat5<-dat5[!is.na(dat5$vialag),]

##merge best database with spatial part 2
##there are more mpios than dat2, so all.x=F
dat4spa<-merge(mpios, subset(dat4, year=="2002"), by.x="MPIOS", by.y="MPIOS", all.x=F)

##convert to neighboring weights matrix, necessary for smooth
nb <- poly2nb(dat4spa, row.names = dat4spa$MPIOS)

##this needed
names(nb) <- attr(nb, "region.id")

save.image("bayes_spa_data.RData")

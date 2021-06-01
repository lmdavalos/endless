library(bipartite)
library(viridis)
library(Matrix)
library(plyr)

##remove prior data
rm(list=ls()) 

##read in data
dat1<-read.csv("cov_brms_v3.csv", stringsAsFactors = FALSE)

##factor
dat1$category<-factor(dat1$category)

##make matrices
##network to contribution
mat1 <- table(dat1$res, dat1$cov)

##make all connectors the same width
mat1<-ifelse(mat1>0, 2, mat1)

##make color vector for category
##makes only a few colors
ncol <- viridis(length(unique(dat1$category))+1, alpha = 1, begin = 0, end = 1, direction = -1, option = "D")

##set colors according to cat of cov
col<-ncol[as.factor(dat1[!duplicated(dat1$cov),'category'])]

##this reorders so they in the right direction
col<-col[order(as.factor(dat1[!duplicated(dat1$cov),'cov']))]

##this is a hackey way of coloring the arrows, matches are done through colnames but cannot be precise on rows, therefore most are positive and then recolor by hand lol
line_color<-ifelse(colnames(mat1)=="Cropland growth", "salmon", ifelse(colnames(mat1)=="Infrastructure", "firebrick", ifelse(colnames(mat1)=="Manual eradication", ifelse(rownames(mat1)=="Conflict", "#c7dfff", "steelblue"), "steelblue")))

##simpler bipartite network
##write to pdf
pdf("brms_net_recol_arrows.pdf", h = 7, w= 6, useDingbats=F)

##plot bottom part including middle
plotweb(mat1, y.width.low=0.1, y.width.high=0.1, method="cca", bor.col.interaction =NA, y.lim=c(-1,3), arrow="down", col.high=col, bor.col.high=col,  col.low="gray80",  bor.col.low="gray80", text.rot = 90, col.interaction =line_color)

##stop the pdf
dev.off()

##libraries
library(reshape2)
library(ggplot2)
library(brms)
library(scales)

##remove prior data
rm(list=ls()) 

# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)

  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                    ncol = cols, nrow = ceiling(numPlots/cols))
  }

 if (numPlots==1) {
    print(plots[[1]])

  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

##get data
##conditional reltaionships
load("conditional_v4.RData")

##plot forest
p1f<-plot(m1, plot = FALSE)[[1]] + theme_bw() + scale_x_continuous(labels=math_format())+labs(x="Forest cover (ha)", y=expression(italic(r[forest])))

p2f<-plot(m1, plot = FALSE)[[2]] + theme_bw() + scale_x_continuous(labels=math_format())+labs(x="Forest cover (ha)", y="Coca cultivation (ha)")

p3f<-plot(m1, plot = FALSE)[[3]] + theme_bw() + scale_x_continuous(labels=math_format())+labs(x="Forest cover (ha)", y="Conflict (victims)")

##plot year
p1y<-plot(m0, plot = FALSE)[[1]] + theme_bw() + scale_x_continuous()+labs(x="Year", y=expression(italic(r[forest])))

p2y<-plot(m0, plot = FALSE)[[2]] + theme_bw() + scale_x_continuous()+labs(x="Year", y="Coca cultivation (ha)")

p3y<-plot(m0, plot = FALSE)[[3]] + theme_bw() + scale_x_continuous()+labs(x="Year", y="Conflict (victims)")

##print
pdf("shared_three.pdf", h=9, w=6)
multiplot(p1f, p2f, p3f, p1y, p2y, p3y, cols=2)
dev.off()

##plot grasslands
p1h<-plot(m2, plot = FALSE)[[1]] + theme_bw() + scale_x_continuous(label=comma)+labs(x="Grassland cover", y=expression(italic(r[forest])))

p2h<-plot(m2, plot = FALSE)[[2]] + theme_bw() + scale_x_continuous(label=comma)+labs(x="Grassland cover", y="Coca cultivation (ha)")

##plot betgra
p1g<-plot(m4, plot = FALSE)[[1]] + theme_bw() + scale_x_continuous(label=comma)+labs(x=expression(italic(r[grassland])), y=expression(italic(r[forest])))

p2g<-plot(m4, plot = FALSE)[[2]] + theme_bw() + scale_x_continuous(label=comma)+labs(x=expression(italic(r[grassland])), y="Coca cultivation (ha)")

##plot inf
p1i<-plot(m6, plot = FALSE)[[1]] + theme_bw() + scale_x_continuous(label=comma)+labs(x="Infrastructure spending (pesos per capita)", y=expression(italic(r[forest])))

p2i<-plot(m6, plot = FALSE)[[2]] + theme_bw() + scale_x_continuous(label=comma)+labs(x="Infrastructure spending (pesos per capita)", y="Coca cultivation (ha)")

##print
pdf("shared_forest_growth_coca.pdf", h=6, w=9)
multiplot(p1h, p2h, p1g, p2g, p1i,  p2i, cols=3)
dev.off()

##coca and conflict have a lot in common

##plot croplands
p2c<-plot(m3, plot = FALSE)[[2]] + theme_bw() + scale_x_continuous(label=comma)+labs(x="Cropland cover", y="Coca cultivation (ha)")

p3c<-plot(m3, plot = FALSE)[[3]] + theme_bw() + scale_x_continuous(label=comma)+labs(x="Cropland cover", y="Conflict (victims)")

##plot pru
p2r<-plot(m8, plot = FALSE)[[2]] + theme_bw() + scale_x_continuous(label=comma)+labs(x="Rural proportion", y="Coca cultivation (ha)")

p3r<-plot(m8, plot = FALSE)[[3]] + theme_bw() + scale_x_continuous(label=comma)+labs(x="Rural proportion", y="Conflict  (victims)")

##plot betpou
p2u<-plot(m9, plot = FALSE)[[2]] + theme_bw() + scale_x_continuous(label=comma)+labs(x=expression(italic(r["urban population"])), y="Coca cultivation (ha)")

p3u<-plot(m9, plot = FALSE)[[3]] + theme_bw() + scale_x_continuous(label=comma)+labs(x=expression(italic(r["urban population"])), y="Conflict (victims)")

##plot asp
p2a<-plot(m11, plot = FALSE)[[2]] + theme_bw() + scale_x_continuous(label=comma)+labs(x="Aerial fumigation (ha)", y="Coca cultivation (ha)")

p3a<-plot(m11, plot = FALSE)[[3]] + theme_bw() + scale_x_continuous(label=comma)+labs(x="Aerial fumigation (ha)", y="Conflict  (victims)")

##plot era
p2e<-plot(m12, plot = FALSE)[[2]] + theme_bw() + scale_x_continuous(label=comma)+labs(x="Manual eradication (ha)", y="Coca cultivation (ha)")

p3e<-plot(m12, plot = FALSE)[[3]] + theme_bw() + scale_x_continuous(label=comma)+labs(x="Manual eradication  (ha)", y="Conflict  (victims)")

##print
pdf("shared_coca_conflict.pdf", h=6, w=15)
multiplot(p2c, p3c, p2r, p3r, p2u, p3u, p2a, p3a, p2e, p3e, cols=5)
dev.off()

##plot conflict
p1w<-plot(m5, plot = FALSE)[[1]] + theme_bw() + scale_x_continuous(label=comma)+labs(x="Conflict (victims)", y=expression(italic(r[forest])))

##plot coca
p3w<-plot(m13, plot = FALSE)[[3]] + theme_bw() + scale_x_continuous(label=comma)+labs(x="Coca cultivation (ha)", y="Conflict (victims)")

##print
pdf("responses.pdf", h=3, w=6)
multiplot(p1w, p3w, cols=2)
dev.off()

##save
save.image("conditional_v4.RData")
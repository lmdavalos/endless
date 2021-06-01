##libraries
library(reshape2)
library(ggplot2)
library(brms)
library(MCMCvis)
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
##student-t, negative binomial, negative binomial, markov random fields
load("snn_mrf_brms_v3.RData")

##student-t, zero inflated negative binomial, zero inflated negative binomial, markov random fields
load("szz_mrf_brms_v3.RData")

##set theme ggplot2
theme_set(theme_minimal())

##forget loo, go with ppcheck

##forget loo, go with ppcheck
p11<-pp_check(snn_mrf_brms, resp="betfor", type="loo_pit") + ggtitle("rForest, student")

p12<-pp_check(szz_mrf_brms, resp="betfor", type="loo_pit") + ggtitle("rForest, student")

p21<-pp_check(snn_mrf_brms, resp="cult", type="loo_pit") + ggtitle("Coca cultivation, negative binomial")

p22<-pp_check(szz_mrf_brms, resp="cult", type="loo_pit") + ggtitle("Coca cultivation, zero-inflated")

p31<-pp_check(snn_mrf_brms, resp="conf", type="loo_pit") + ggtitle("Conflict, negative binomial")

p32<-pp_check(szz_mrf_brms, resp="conf", type="loo_pit") + ggtitle("Conflict, zero-inflated")

##save
save.image("loo_pit_v9.RData")

##print
pdf("loo_pit_plo_v9.pdf", h=9, w=6)
multiplot(p11, p21, p31, p12, p22, p32, cols=2)
dev.off()

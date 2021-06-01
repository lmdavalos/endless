##libraries
library(reshape2)
library(ggplot2)
library(brms)
library(MCMCvis)
library(bayesplot)
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
##analyses of the models with infrastructure and betcon on betfor
##snn mrf
load("snn_mrf_brms_v3.RData")

##print out results
##exclude mpio-level clustering effects
sink("snn_mrf_brms_summary_v3.txt")
print(MCMCsummary(snn_mrf_brms, excl=c("s_betfor_t2yearMPIOS_1", "s_betfor_t2yearMPIOS_2", "s_betfor_t2yearMPIOS_3", "s_cult_t2yearMPIOS_1", "s_cult_t2yearMPIOS_2", "s_cult_t2yearMPIOS_3", "s_conf_t2yearMPIOS_1", "s_conf_t2yearMPIOS_2", "s_conf_t2yearMPIOS_3"), probs=c(0.025, 0.975) ))
sink()

##make less terrible
color_scheme_set("gray")
bayesplot_theme_set(theme_bw())

v1<-mcmc_areas(as.matrix(snn_mrf_brms), pars=c("b_betfor_log10Forest", "b_betfor_log10Grasslands", "b_betfor_log10Croplands", "b_betfor_betgra", "b_betfor_log10confP1", "b_betfor_log10inf_lagP1", "b_betfor_log10AREA"), prob_outer = .99, area_method="scaled height")

v2<-mcmc_areas(as.matrix(snn_mrf_brms), pars=c("b_cult_log10Forest", "b_cult_log10Grasslands", "b_cult_log10Croplands", "b_cult_betgra", "b_cult_betcro", "b_cult_log10inf_lagP1", "b_cult_pru", "b_cult_betpou", "b_cult_betcoc", "b_cult_log10asp_lagP1", "b_cult_log10era_lagP1", "b_cult_log10AREA"), prob_outer = .99, area_method="scaled height")

v3<-mcmc_areas(as.matrix(snn_mrf_brms), pars=c("b_conf_log10Forest", "b_conf_log10Croplands", "b_conf_log10inf_lagP1", "b_conf_log10popr", "b_conf_pru", "b_conf_betpou", "b_conf_betcoc", "b_conf_log10cultP1", "b_conf_log10asp_lagP1", "b_conf_log10era_lagP1"), prob_outer = .99, area_method="scaled height")

##print
pdf("coefs_three_v3.pdf", h=9, w=4)
multiplot(v1, v2, v3, cols=1)
dev.off()


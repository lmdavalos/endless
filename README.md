# "Endless, Grassy Frontier: Pasture Expansion and Conflict Promoted by Coca and Fumigation Explain Deforestation in Colombia"


Required R packages:
dplyr
sp
spdep
maptools
surveillance
nlme
reshape2
ggplot2
scales
piecewiseSEM
lme4
brms
MCMCvis
bayesplot
plotKML
sf
transformr
tidyverse
gpclib
plyr
bipartite
viridis
Matrix

1) Place all scripts in the same folder as the data.

2) sem_dat_v7.R tabulates the data and includes the adjacency matrix for downstream analyses, saves a RData file.

3) piecewise_explore_v8.R generates piecewise SEM,

4) sem_bet_v8nb.R and sem_bet_v8zi.R run Bayesian models, these run parallel chains and take <7 days in a cluster but 4 months on a mini-cluster.

5) conditional_v4 and smooth_v4.R process the Bayesian results and save their corresponding files.

6) plot_summaries_v9.R, plot_smooth_v4.R, plot_conditional_v5.R use already generated RData files to plot coefficients, GMRF smooths, and conditional effects.

7) loo_pit_plo_v10.R generates and plots loo-pit simulations and requires both models to be compared.

8) network1_v3.R makes a network plot based on cov_brms_v3.csv, which was compiled by hand. Three plots and arrow colors were adjusted by hand.

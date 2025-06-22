rm(list=ls())

source("../utils.R")

# load measures for justice data for Pennsylvania
load("~/bmm/data/data_PA.RData")

# obtain density estimates and confidence intervals for each measure
conv_rates = list(pri = get_results(120, 51), pub = get_results(120, 52),   coun = get_results(120, 53))
misd_ror   = list(pri = get_results(7, 51),   pub = get_results(7, 52),   coun = get_results(7, 53))
misd_bail  = list(pri = get_results(123, 51), pub = get_results(123, 52), coun = get_results(123, 53))
fel_ror    = list(pri = get_results(122, 51), pub = get_results(122, 52), coun = get_results(122, 53))
fel_bail   = list(pri = get_results(124, 51), pub = get_results(124, 52), coun = get_results(124, 53))

# display plots
# Figure 2
plot_measure(conv_rates, xlab='Conviction Rate', pos=c(1,3,4), legend_loc = "topleft")
# Figure 3, (a)-(d)
plot_measure(misd_ror, xlab='Proportions of Cases', pos=c(1.5,3.5,1), legend_loc="topleft")
plot_measure(misd_bail, xlab='Proportions of Cases',  pos=c(1.5,3.5,4), legend_loc="topright")
plot_measure(fel_ror, xlab='Proportions of Cases', pos=c(1,3,2), legend_loc="topright")
plot_measure(fel_bail, xlab='Proportions of Cases', pos=c(1,3,4), legend_loc="topleft")

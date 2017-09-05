# By Luis Paulo 
#
# First version: 2017-02
# Revision: 2017-08
#
# Correlation analysis between RxJava's Development Effort and Incidence of Smells
# RxJava is available @ https://github.com/ReactiveX/RxJava
#

#depends
library("TTR")
#loading csv
ds.path <- paste(workspace.dspath, 'rxjava.csv', sep = '/')
ds.data <- read.csv(ds.path, header=TRUE, sep=",")
#separating efforts (per type) 
dev_e <- ds.data$enhancement
bug_e <- ds.data$bug
dvb_e <- dev_e + bug_e
smlls <- ds.data$gc + ds.data$bc + ds.data$dc + ds.data$bm + ds.data$lm
#combining effort types into one single matrix of values
combined_e <- matrix(c(dev_e, bug_e, dvb_e, smlls), nrow = 150)
#creating timeseries 
timeseries_e <- ts(combined_e, frequency = 1, names = c('enh', 'bug', 'e+b', 'smells'))
#plotting timeseries
plot(timeseries_e, main = "RxJava's Dev x Smells")
#plotting tendencies
dev_e_trend <- SMA(dvb_e)
plot.ts(dev_e_trend)
smlls_trend <- SMA(smlls)
plot.ts(smlls_trend)
#breakdown effort x smell correlation
eccoba.caller(ds.data, minimal_timeframe = 5, minimal_correlation = 0.7, listener_function = real.caller)


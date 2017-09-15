# By Luis Paulo 
#
# First version: 2017-02
# Revision: 2017-08
#
# Correlation analysis between Springboot's Development Effort and Incidence of Smells
# Springboot is available @ https://github.com/spring-projects/spring-boot
#

#depends
library("TTR")
#loading csv
ds.path <- paste(workspace.dspath, 'springboot.csv', sep = '/')
ds.data <- read.csv(ds.path, header=TRUE, sep=",")
#separating efforts (per type) 
dev_e <- ds.data$enhancement
bug_e <- ds.data$bug
dvb_e <- dev_e + bug_e
smlls <- ds.data$gc + ds.data$bc + ds.data$dc + ds.data$bm + ds.data$lm
#combining effort types into one single matrix of values
combined_e <- matrix(c(dev_e, bug_e, dvb_e, smlls), nrow = 744)
#creating timeseries 
timeseries_e <- ts(combined_e, frequency = 1, names = c('enh', 'bug', 'e+b', 'smells'))
#plotting timeseries
plot(timeseries_e, main = "Springboot's Dev x Smells")
#plotting tendencies
dev_e_trend <- SMA(dvb_e)
plot.ts(dev_e_trend)
smlls_trend <- SMA(smlls)
plot.ts(smlls_trend)
#testing data for normalization
ds.log_devl <- log(dvb_e + 1)
ds.log_smls <- log(smlls + 1)
plot(density(ds.log_devl))
plot(density(ds.log_smls))
shapiro.test(ds.log_devl)
shapiro.test(ds.log_smls)
## Plot using a qqplot
qqnorm(ds.log_devl)
qqline(ds.log_devl, col = 2)
qqnorm(ds.log_smls)
qqline(ds.log_smls, col = 2)
#breakdown effort x smell correlation
eccoba.caller(ds.data, minimal_timeframe = 10, minimal_correlation = 0.75, listener_function = real.caller)


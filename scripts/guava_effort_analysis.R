# By Luis Paulo 
#
# First version: 2017-02
# Revision: 2017-08
#
# Correlation analysis between Google-Guava's Development Effort and Incidence of Smells
# Google-guava is available @ https://github.com/google/guava
#

#depends
library("TTR")
library("ggplot2")
library("tidyr")
#loading csv
ds.path <- paste(workspace.dspath, 'guava.csv', sep = '/')
ds.data <- read.csv(ds.path, header=TRUE, sep=",")
#separating efforts (per type) 
dev_e <- ds.data$enhancement
bug_e <- ds.data$bug
dvb_e <- dev_e + bug_e
smlls <- ds.data$gc + ds.data$bc + ds.data$dc + ds.data$bm + ds.data$lm
#combining effort types into one single matrix of values
combined_e <- matrix(c(dev_e, bug_e, dvb_e, smlls), nrow = 125)
#creating timeseries 
timeseries_e <- ts(combined_e, frequency = 1, names = c('enh', 'bug', 'e+b', 'smells'))
#plotting timeseries
plot(timeseries_e, main = "GGuava's Dev x Smells")
#plotting tendencies
dev_e_trend <- SMA(dvb_e)
plot.ts(dev_e_trend)
smlls_trend <- SMA(smlls)
plot.ts(smlls_trend)
#testing data for normalization
cor(dvb_e[60:80], smlls[60:80])
cor.test(dvb_e[60:80], smlls[60:80])
plot(density(ds.log_devl))
plot(density(ds.log_smls))
## Plot using a qqplot
qqnorm(ds.log_devl)
qqline(ds.log_devl, col = 2)
qqnorm(ds.log_smls)
qqline(ds.log_smls, col = 2)
#breakdown effort x smell correlation
eccoba.caller('guava', ds.data, minimal_timeframe = 10, minimal_correlation = 0.75, listener_function = coral.caller)
eccoba.caller('guava', ds.data, minimal_timeframe = 10, minimal_correlation = 0.75, listener_function = export.to.resys)
#plot refactorings recommendation
refacs.contextualized_path    <- paste(resys.csvpath, 'output/incidence_guava_contextualized.csv', sep = '/')
refacs.notcontextualized_path <- paste(resys.csvpath, 'output/incidence_guava.csv', sep = '/')
refacs.contextualized    <- read.csv(refacs.contextualized_path, header=TRUE)
refacs.notcontextualized <- read.csv(refacs.notcontextualized_path, header=TRUE)
ggplot(refacs.contextualized, aes(x=as.factor(date), y=qt, fill=refactoring)) + 
  geom_bar(stat = "identity", width=0.5, position="dodge") +
  scale_fill_brewer(palette="Paired")
ggplot(refacs.notcontextualized, aes(x=as.factor(date), y=qt, fill=refactoring)) + 
  geom_bar(stat = "identity", width=0.5, position="dodge") +
  scale_fill_brewer(palette="Paired")

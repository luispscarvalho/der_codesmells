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
#breakdown effort x smell correlation
eccoba.caller('guava', ds.data, minimal_timeframe = 10, minimal_correlation = 0.7, listener_function = coral.caller)
eccoba.caller('guava', ds.data, minimal_timeframe = 10, minimal_correlation = 0.7, listener_function = export.to.resys)
#plot refactorings recommendation
refacs.lowc_path <- paste(workspace.dspath, '/others/gguava_refacts_lowcor.csv', sep = '/')
refacs.higc_path <- paste(workspace.dspath, '/others/gguava_refacts_highcor.csv', sep = '/')
refacs.lowc <- read.csv(refacs.lowc_path, header=TRUE)
refacs.higc <- read.csv(refacs.higc_path, header=TRUE)
refacs.dflowc <- gather(refacs.lowc, refactoring, qt, EM,RTWQ,IPO,PWO,DC,RMWMO,CCE,MM,EF,ECo,HM,EI,ESubC,RDWO,EH,ESupC,MF,PF,PM,RCWP,EC)
refacs.dfhigc <- gather(refacs.higc, refactoring, qt, EM,RTWQ,IPO,PWO,DC,RMWMO,CCE,MM,EF,ECo,HM,EI,ESubC,RDWO,EH,ESupC,MF,PF,PM,RCWP,EC)
ggplot(refacs.dflowc, aes(x=as.factor(date), y=qt, fill=refactoring)) + 
  geom_bar(stat = "identity", width=0.5, position="dodge") +
  scale_fill_brewer(palette="Paired")
ggplot(refacs.dfhigc, aes(x=as.factor(date), y=qt, fill=refactoring)) + 
  geom_bar(stat = "identity", width=0.5, position="dodge") +
  scale_fill_brewer(palette="Paired")

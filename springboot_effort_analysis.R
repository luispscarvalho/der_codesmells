#
# Development timeseries analisys for Spring-Boot (https://github.com/spring-projects/spring-boot)
#
# By Luis Paulo, 2017-02-28
#
#init dependencies
#loading csv
data <- read.csv('/misc/workspace/doutorado/temp/springboot.csv', header=TRUE, sep=",")
#separating efforts (per type) 
dev_e <- data$enhancement
bug_e <- data$bug
dvb_e <- dev_e + bug_e
chr_e <- data$churn
#combining effort types into one single matrix of values
combined_e <- matrix(c(dev_e, bug_e, dvb_e, chr_e), nrow = 744)
#creating timeseries 
timeseries_e <- ts(combined_e, frequency = 1, names = c('enh', 'bug', 'e+b', 'churn'))
#plotting timeseries
plot(timeseries_e, main = "Springboot's DE x Churn")
#finding auto-correlations between deb and tech debt
library("TTR")
dev_e_trend <- SMA(dvb_e)
plot.ts(dev_e_trend)
tdr_e_trend <- SMA(tdr_e)
plot.ts(tdr_e_trend)
# any subset with strong correlation?
dvb_e_sub <- data[10:10,]$development + data[10:10,]$bug
tdr_e_sub <- data[10:10,]$td
cor(dvb_e_sub, tdr_e_sub, method = "pearson")
corr_dev_tdr <- ccf(dvb_e_sub, tdr_e_sub)
corr_dev_tdr
# breakdown effort x smell correlation
eccoba.caller(data$enhancement + data$bug, data$churn, minimal_timeframe = 40, minimal_correlation = 0.6)
# subset of correlation found. How does the data cluster?
set.seed(20)
cat_cluster <- kmeans(data[1:125, 2:3], 3, nstart =20)
cat_cluster

table(cat_cluster$cluster, data[1:125,]$cat)
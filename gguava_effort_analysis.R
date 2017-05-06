#
# Development timeseries analisys for Google-guava (https://github.com/google/guava)
#
# By Luis Paulo, 2017-02-28
#
#loading csv
data <- read.csv('/gguava.csv', header=TRUE, sep=",")
#separating efforts (per type) 
dev_e <- data$enhancement
bug_e <- data$bug
dvb_e <- dev_e + bug_e
chr_e <- data$churn
#combining effort types into one single matrix of values
combined_e <- matrix(c(dev_e, bug_e, dvb_e, chr_e), nrow = 125)
#creating timeseries 
timeseries_e <- ts(combined_e, frequency = 1, names = c('enh', 'bug', 'e+b', 'churn'))
#plotting timeseries
plot(timeseries_e, main = "GGuava's Dev x Churn")
#finding auto-correlations between deb and tech debt
library("TTR")
dev_e_trend <- SMA(dvb_e)
plot.ts(dev_e_trend)
chr_e_trend <- SMA(chr_e)
plot.ts(chr_e_trend)
#breakdown effort x smell correlation
eccoba.caller(data$enhancement + data$bug, data$churn, minimal_timeframe = 10, minimal_correlation = 0.6)
#highly correlated subset found? How do smells cluster?
ndata <- data[64:125, 4:9]
gc_churn <- ndata$gc*ndata$churn
bc_churn <- ndata$bc*ndata$churn
dc_churn <- ndata$dc*ndata$churn
bm_churn <- ndata$bm*ndata$churn
lm_churn <- ndata$lm*ndata$churn
#graphically comparing the churns (for all smells)
#combining effort types into one single matrix of values
combined_churn <- matrix(c(gc_churn, bc_churn, dc_churn, bm_churn, lm_churn), nrow = 62)
#creating timeseries 
timeseries_churn <- ts(combined_churn, frequency = 1, names = c('gc', 'bc', 'dc', 'bm', 'lm'))
#plotting timeseries
plot(timeseries_churn, main = "GGuava's Code Smells x Churn")
churn_range <- range(0, gc_churn, bc_churn, dc_churn, bm_churn, lm_churn)
plot(gc_churn, type="o", col="blue", ylim=churn_range, axes=FALSE, ann=FALSE)
box()
plot(lm_churn, type="o", col="darkgrey", ylim=churn_range, axes=FALSE, ann=FALSE)
box()
lines(bc_churn, type="o", pch=22, lty=2, col="red")
lines(dc_churn, type="o", pch=22, lty=2, col="navy")
lines(bm_churn, type="o", pch=22, lty=2, col="darkgreen")
lines(lm_churn, type="o", pch=22, lty=2, col="darkgrey")
title(main="GGuava's Code Smells x Effort (Code-churn)", col.main="black", font.main=1)
title(xlab="Time", col.lab=rgb(0,0.5,0))
title(ylab="Effort", col.lab=rgb(0,0.5,0))
legend(1, churn_range[2], c("GC","BC","DC","BM","LM"), cex=0.8, 
       col=c("blue","red","navy","darkgreen","darkgrey"), pch=21:22, lty=1:2, xjust = 0);
#agglomerations
kc <- kmeans(ndata, 3, nstart=20)
kc
table(kc$cluster, ndata$gc)
table(kc$cluster, ndata$bc)
table(kc$cluster, ndata$dc)
table(kc$cluster, ndata$bm)
table(kc$cluster, ndata$lm)

plot(ndata, col=kc$cluster)

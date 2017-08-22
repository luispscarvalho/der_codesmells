#
# Development timeseries analisys for DbFit (https://github.com/dbfit/dbfit)
#
# By Luis Paulo, 2017-02-28
#
#loading csv
data <- read.csv('/dbfit.csv', header=TRUE, sep=",")
#separating efforts (per type) 
dev_e <- data$enhancement
bug_e <- data$bug
dvb_e <- dev_e + bug_e
chr_e <- data$churn
#combining effort types into one single matrix of values
combined_e <- matrix(c(dev_e, bug_e, dvb_e, chr_e), nrow = 141)
#creating timeseries 
timeseries_e <- ts(combined_e, frequency = 1, names = c('enh', 'bug', 'e+b', 'churn'))
#plotting timeseries
plot(timeseries_e, main = "Dbfit's DE x Churn")
#finding auto-correlations between deb and tech debt
library("TTR")
dev_e_trend <- SMA(dvb_e)
plot.ts(dev_e_trend)
chr_e_trend <- SMA(chr_e)
plot.ts(chr_e_trend)
# any subset with strong correlation?
dvb_e_sub <- data[40:30,]$enhancement + data[40:30,]$bug
chr_e_sub <- data[40:30,]$chr
cor(dvb_e_sub, chr_e_sub, method = "pearson")
corr_dev_chr <- ccf(dvb_e_sub, chr_e_sub)
corr_dev_chr
# breakdown effort x smell correlation
eccoba.caller(data$enhancement + data$bug, data$churn, minimal_timeframe = 10, minimal_correlation = 0.6)
#highly correlated subset found? How do smells relate?
ndata <- data[37:141, 4:9] # high correlation interval
gc_churn <- ndata$gc*ndata$churn
bc_churn <- ndata$bc*ndata$churn
dc_churn <- ndata$dc*ndata$churn
bm_churn <- ndata$bm*ndata$churn
lm_churn <- ndata$lm*ndata$churn
#graphically comparing the churns (for all smells)
churn_range <- range(0, gc_churn, bc_churn, dc_churn, bm_churn, lm_churn)
plot(gc_churn, type="o", col="blue", ylim=churn_range, axes=FALSE, ann=FALSE)
box()
lines(bm_churn, type="o", pch=22, lty=2, col="darkgreen")
lines(lm_churn, type="o", pch=22, lty=2, col="darkgrey")
title(main="RxJava's Code Smells x Effort (Code-churn)", col.main="black", font.main=1)
title(xlab="Time", col.lab=rgb(0,0.5,0))
title(ylab="Effort", col.lab=rgb(0,0.5,0))
legend("topright", churn_range[2], c("GC","BM","LM"), cex=0.8, 
       col=c("blue","darkgreen","darkgrey"), pch=21:22, lty=1:2, xjust = 1)
plot(gc_churn, type="o", col="blue", ylim=churn_range, axes=FALSE, ann=FALSE)
box()
plot(bm_churn, type="o", col="darkgreen", ylim=churn_range, axes=FALSE, ann=FALSE)
box()
plot(lm_churn, type="o", col="darkgrey", ylim=churn_range, axes=FALSE, ann=FALSE)
box()

#agglomerations
set.seed(20)
kc <- kmeans(ndata, 2, nstart=20)
kc
table(kc$cluster, ndata$gc)
table(kc$cluster, ndata$bc)
table(kc$cluster, ndata$dc)
table(kc$cluster, ndata$bm)
table(kc$cluster, ndata$lm)

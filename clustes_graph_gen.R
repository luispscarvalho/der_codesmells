library(ggplot2)
#load the data
clusters <- read.csv("/clusters.csv", sep=",")

barplot(as.matrix(clusters), 
        ylab = "Instances",
        cex.lab = 1.5, 
        cex.main = 1.4, 
        beside=TRUE, 
        col=c("#993404", "#d95f0e", "#fe9929", "#fed98e", "#ffffd4"),
        las=2) + coord_flip()
legend("topright", c("God Class","Brain Class","Data Class","Brain Method","Long Method"), cex=0.6, 
       bty="n", fill=c("#993404", "#d95f0e", "#fe9929", "#fed98e", "#ffffd4"))



#### data ######
sam1 <- searchData$subC #Insert Data
sam2 <- searchData$subD #Insert Data

DF <- stack(data.frame(Clumpy = sam1, Diffuse = sam2))

#### paired-t-test ######

res <- t.test(values ~ ind, data=DF, paired = TRUE)

#### plots #####
library(gregmisc)

opar <- par(mfrow=c(1,1))
tapply(DF$values, list(DF$ind), ci)
plotmeans(values ~ ind, data=DF, xlab="Search Space", ylab="SubTrial",main=c("Average Anagram Sub-trial"), n.label=FALSE)

d <- sam1 - sam2

plot.ylim <- c(min(d, 0), max(d, 0))
abline(h = 0, lty = 2, col="red",lwd =2)

#### Strip Chart 
#stripchart(d, vertical=T, pch=16, ylim = plot.ylim )

points(1, res$estimate, col="red", pch=16)
arrows(1, res$estimate, 1, res$conf.int[1], col="red", lwd=2, angle=90,
       length=0.1)
arrows(1, res$estimate, 1, res$conf.int[2], col="red", lwd=2, angle=90,
       length=0.1)
## Run a DAPC to find optimal clusters

library(poppr)
library(adegenet) # for dapc
library(vcfR) # for reading in genetic data
library(tidyverse) # for manipulating and plotting data
library(LEA) # For sNMF
library(rnaturalearth)
library(gtools)

x <- read.genepop(file.choose())

x

par(mfrow=c(1,1))


# Identify clusters (K)

grp <- find.clusters(x, max.n.clust=30)

dapc <- dapc(x, grp$grp)

dapc

temp <- optim.a.score(dapc)

# Cross validation method to find # of PCs

mat <- as.matrix(x, NA.method="mean")

grp <- pop(x)

cv <- xvalDapc(mat, grp)

cv

xval2 = xvalDapc(mat, grp, n.pca.max=100, training.set=0.9,
                result="groupMean", center=TRUE, scale=FALSE,
                n.rep=100, n.pca=NULL, parallel="multicore", ncpus=4L)

plot(xval2)

xval <- xvalDapc(mat, grp, n.pca.max = 100, training.set = 0.9,
                 result = "groupMean", center = TRUE, scale = FALSE,
                 n.pca = NULL, n.rep = 30, xval.plot = TRUE)


## Number of PCs with best stats
xval2$`Number of PCs Achieving Highest Mean Success`

xval2$`Number of PCs Achieving Lowest MSE`

scatter(dapc2)

cv_dapc10 <- dapc(x, grp$grp, n.pca = 10, n.da = 3)
cv_dapc15 <- dapc(x, grp$grp, n.pca = 15, n.da = 3)


cols <- c("darkolivegreen4", "darkslategrey", "lightpink3", "mediumpurple4")


scatter(cv_dapc10, ratio.pca=0.3, bg="white", pch=20, cell=1,
        cstar=0, col=cols, solid=.8, cex=3, clab=0,
        mstree=FALSE, scree.da=FALSE, posi.pca="bottomright",
        leg=TRUE, txt.leg=paste("Cluster",1:4))

compoplot(cv_dapc10, subset=1:89, posi="bottomright",
          txt.leg=paste("Cluster", 1:4), lab="",
          ncol=4, xlab="individuals", col=cols)

scatter(cv_dapc15, ratio.pca=0.3, bg="white", pch=20, cell=1,
cstar=0, col=cols, solid=.8, cex=3, clab=0,
mstree=FALSE, scree.da=FALSE, posi.pca="bottomright",
leg=TRUE, txt.leg=paste("Cluster",1:4))

compoplot(cv_dapc15, subset=1:89, posi="bottomright",
          txt.leg=paste("Cluster", 1:4), lab="",
          ncol=4, xlab="individuals", col=cols)



set.seed(123)
test = tab(x, NA.method = "mean")
crossval = xvalDapc(test, x$pop, result = "groupMean", xval.plot = TRUE, n.pca.max=80)

crossval$`Number of PCs Achieving Highest Mean Success`

crossval$`Number of PCs Achieving Lowest MSE`


library(ggplot2)
library(MASS)
library(caret)
library(vegan)


### 1 - DATA INPUT

## measurements
myData <- read.csv('dataDressel.csv', header=T, sep=",")
# choose the type of Dressel to analyse
myData= subset(myData, type %in% c("Dressel C","Dressel D","Dressel E"))
# keep only the site and the measurements
myData <- myData[,5:13]


## euclidean distance through river
river <- read.csv('riverDistances.csv', header=T, sep=",")
distRiver =as.dist(xtabs(river[, "spatial"] ~ river[, "from"] + river[, "to"]), diag=F)

### 2 - EXPLORATORY DATA ANALYSUS

# PCA
pcaData <- princomp(myData[,1:8])
# plot to check the relevance of the first 2 PC'S
plot(pcaData)
# print table for PC formation
pcaData$loadings

# plot of 2 main PCs
pc1 <- pcaData$scores[,1]
pc2 <- pcaData$scores[,2]
values <- data.frame(pc1=pc1, pc2=pc2, site=myData$site)

pdf("pca.pdf", width=6, height=9)
ggplot(values, aes(x=pc2, y=pc1, col=site)) + geom_density2d(alpha=0.5, linetype="dashed", size=0.5) + geom_point(alpha=0.5) +  facet_wrap(~site,ncol=1) + xlab("PC1") + ylab("PC2") + theme_bw() + theme(legend.position="none")
dev.off()

#other way to do it faceting per type of dressel 
#changes before done: update database, change positions)
#to split by dressel types use ,4:13 with the new database

values <- data.frame(pc1=pc1, pc2=pc2, site=myData$site, type=myData$type)

pdf("testing.pdf", width=6, height=9)
ggplot(values, aes(x=pc2, y=pc1, col=site)) + geom_point(alpha=0.5) +  facet_wrap(~type,ncol=3) + xlab("PC1") + ylab("PC2") + theme_bw()
dev.off()

### 3 - Linear Discriminant Analysis

testData <- myData
# train the model using 8 measurements
DAmodel <- lda(testData[,1:8], testData$site, prior=c(1,1,1,1,1)/5)
# predict the training
DAclass <- predict(DAmodel, testData[,1:8])
# compute confusion matrix
testData$class <- DAclass$class
cm <- confusionMatrix(testData$class, testData$site)$table
# remove diagonal
cm[1,1] = cm[2,2] = cm[3,3] = cm[4,4] = cm[5,5] = NA
# get percentage of errors
cm <- t(t(cm)/colSums(cm, na.rm=TRUE))
# transform number of confused into a dissimilarity matrix
cm <- 1 - cm    
distAmph <- as.dist(cm)

### 4 - Mantel test

mantel(distRiver, distAmph)


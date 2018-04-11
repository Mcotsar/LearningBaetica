##################################    RESULTS FOR THE PAPER     ##############################################

library(ggplot2)
library(gridExtra)    
library(plyr)
library(MASS)
library(caret)

myData <- read.csv('drespaper.csv', header=T, sep=",")

##choose the type of Dressel to do the analyse
myData= subset(myData, type %in% c("Dressel C","Dressel D","Dressel E"))
myData <- myData[,5:13]
#to count the data
count(myData)

##testing elements with different variables 

ggplot(myData, aes(x=exterior_diam, y=rim_w, colour=site)) + geom_point()
ggplot(myData, aes(x=exterior_diam, fill=site)) + geom_bar() + facet_wrap(~site, ncol=1)
ggplot(myData, aes(x=rim_w_2, y=protruding_rim, colour=site)) + geom_point() + facet_wrap(~site)

#######  Minimum   ####### 
#we use this analyse to equate the sample 
#we are using a dataset of 210 to training set

sampleSize = min(count(myData,'site')$freq)

amph1 <- subset(myData, site=="delicias")
sample1 <- amph1[sample(nrow(amph1), sampleSize),]
amph2 <- subset(myData, site=="malpica")
sample2 <- amph2[sample(nrow(amph2), sampleSize),]
amph3 <- subset(myData, site=="belen")
sample3 <- amph3[sample(nrow(amph3), sampleSize),]
amph4 <- subset(myData, site=="parlamento")
sample4 <- amph4[sample(nrow(amph4), sampleSize),]
amph5 <- subset(myData, site=="villaseca")
sample5 <- amph5[sample(nrow(amph5), sampleSize),]

sample <- rbind(sample1,sample2)
sample <- rbind(sample, sample3)
sample <- rbind(sample, sample4)
sample <- rbind(sample, sample5)


######  Principal Component Analyse  ########

logDataSample <- log(sample[,1:8])
pcaResultsSample <- princomp(logDataSample)

#center=T, scale=T)optional. ggplot new is not neccesary 

# plot to check the relevance of the first 2 PC'S
plot(pcaResultsSample)
# get the scores of the data
pcaValuesSample <- as.data.frame(pcaResultsSample$scores)
# put type in pcaValues
pcaValuesSample$site <- sample$site
ggplot(pcaValuesSample, aes(x=Comp.1, y=Comp.2, col=site)) + geom_point()
ggplot(pcaValuesSample, aes(x=Comp.1, y=Comp.2, col=site)) + geom_point() + facet_wrap(~site,ncol=1)

#more testing (ERROR)
#g1 <- ggplot(pcaValuesSample, aes(x=Comp.1, y=Comp.2, colour=factor(class))) + geom_point() + facet_grid(~site) + ggtitle("pca1_2")
#ggplot(pcaValuesSample, aes(x=Comp.1, y=Comp.2, col=interaction(site,class), label=site)) + geom_text(size=5) + theme_bw() + theme(legend.position="top")

#use summary to see the cumulative stuff, standard deviation, etc. Use head to see the resuts of the variables. 

#with the application of geom_density2d and dev.off to keep it

svg('fig_dist.svg3')    
ggplot(pcaValuesSample, aes(x=Comp.1, y=Comp.2)) + geom_density2d(aes(col=site), alpha=0.3) + geom_point(aes(col=site), size=2) + facet_wrap(~site, ncol=1) + theme(legend.position='none')
dev.off()


#######  Discriminant Analysis   ##########


amphDA <- qda(site~Comp.1+Comp.2, data=pcaValuesSample, prior=c(1,1,1,1,1)/5)
predQda <- predict(amphDA, pcaValuesSample)
    
sample$probDelicias<- predQda$posterior[,"delicias"]
sample$probMalpica <- predQda$posterior[,"malpica"]
sample$probBelen <- predQda$posterior[,"belen"]
sample$probParlamento <- predQda$posterior[,"parlamento"]
sample$probVillaseca <- predQda$posterior[, "villaseca"]
pcaValuesSample$class <- predQda$class


####  Confusion Matrix  #####

confusionMatrix(pcaValuesSample$class, pcaValuesSample$site)

correct <- subset(pcaValuesSample, site==class)
incorrect <- subset(pcaValuesSample, site!=class)
correct$predict <- "pred. correcta"
incorrect$predict <- "pred. incorrecta"

result <- rbind(correct, incorrect)
result <- subset(result, Comp.1>-1.2)
result <- subset(result, Comp.2>-2.1)

svg('resultados.svg'), width=15, height=8)    
ggplot(result, aes(x=Comp.1, y=Comp.2, col=site)) + geom_point(size=3) + facet_wrap(~predict, ncol=2) + ggtitle('resultado PCA+DA')
dev.off()

#other different ways to perform the prediction
    
#ggplot(result, aes(x=Comp.1, y=Comp.2, col=site)) + geom_point(size=3) + facet_wrap(~correct, ncol=1) + ylim(c(-0.3,0.3)) (ERROR)


##### Matrix distance #######

distMetrics <- matrix(0, nrow=5, ncol=5)
rownames(distMetrics) <- c('parlamento','belen','delicias','malpica','villaseca')
colnames(distMetrics) <- c('parlamento','belen','delicias','malpica','villaseca')

parlamento <- subset(myData, site=='parlamento')    
belen <- subset(myData, site=='belen')    
delicias <- subset(myData, site=='delicias')    
malpica <- subset(myData, site=='malpica')
villaseca <- subset(myData, site=='villaseca')

# 1 - parlamento-belen
pairData <- rbind(parlamento,belen)
sampleSize = min(count(pairData,'site')$freq)

sample1 <- parlamento[sample(nrow(parlamento), sampleSize),]
sample2 <- belen[sample(nrow(belen), sampleSize),]
sample <- rbind(sample1,sample2)

logDataSample <- log(sample[,1:8])
pcaResultsSample <- princomp(logDataSample, center=T, scale=T)
pcaValuesSample <- as.data.frame(pcaResultsSample$scores)
pcaValuesSample$site <- factor(sample$site)

amphDA <- qda(site~Comp.1+Comp.2, data=pcaValuesSample, prior=c(1,1)/2)
predQda <- predict(amphDA, pcaValuesSample)
pcaValuesSample$class <- predQda$class

conf <- confusionMatrix(pcaValuesSample$class, pcaValuesSample$site)
distMetrics['parlamento','belen'] <- conf$overall['Accuracy']
distMetrics['belen','parlamento'] <- conf$overall['Accuracy']

# 2 - malpica-parlamento
pairData <- rbind(parlamento,malpica)
sampleSize = min(count(pairData,'site')$freq)

sample1 <- parlamento[sample(nrow(parlamento), sampleSize),]
sample2 <- malpica[sample(nrow(malpica), sampleSize),]
sample <- rbind(sample1,sample2)

logDataSample <- log(sample[,1:8])
pcaResultsSample <- princomp(logDataSample, center=T, scale=T)
pcaValuesSample <- as.data.frame(pcaResultsSample$scores)
pcaValuesSample$site <- factor(sample$site)

amphDA <- qda(site~Comp.1+Comp.2, data=pcaValuesSample, prior=c(1,1)/2)
predQda <- predict(amphDA, pcaValuesSample)
pcaValuesSample$class <- predQda$class

conf <- confusionMatrix(pcaValuesSample$class, pcaValuesSample$site)
distMetrics['parlamento','malpica'] <- conf$overall['Accuracy']
distMetrics['malpica','parlamento'] <- conf$overall['Accuracy']

# 3 - malpica-belen
pairData <- rbind(malpica,belen)
sampleSize = min(count(pairData,'site')$freq)

sample1 <- malpica[sample(nrow(malpica), sampleSize),]
sample2 <- belen[sample(nrow(belen), sampleSize),]
sample <- rbind(sample1,sample2)

logDataSample <- log(sample[,1:8])
pcaResultsSample <- princomp(logDataSample, center=T, scale=T)
pcaValuesSample <- as.data.frame(pcaResultsSample$scores)
pcaValuesSample$site <- factor(sample$site)

amphDA <- qda(site~Comp.1+Comp.2, data=pcaValuesSample, prior=c(1,1)/2)
predQda <- predict(amphDA, pcaValuesSample)
pcaValuesSample$class <- predQda$class

conf <- confusionMatrix(pcaValuesSample$class, pcaValuesSample$site)
distMetrics['malpica','belen'] <- conf$overall['Accuracy']
distMetrics['belen','malpica'] <- conf$overall['Accuracy']

# 4 - delicias-parlamento
pairData <- rbind(parlamento,delicias)
sampleSize = min(count(pairData,'site')$freq)

sample1 <- parlamento[sample(nrow(parlamento), sampleSize),]
sample2 <- delicias[sample(nrow(delicias), sampleSize),]
sample <- rbind(sample1,sample2)

logDataSample <- log(sample[,1:8])
pcaResultsSample <- princomp(logDataSample, center=T, scale=T)
pcaValuesSample <- as.data.frame(pcaResultsSample$scores)
pcaValuesSample$site <- factor(sample$site)

amphDA <- qda(site~Comp.1+Comp.2, data=pcaValuesSample, prior=c(1,1)/2)
predQda <- predict(amphDA, pcaValuesSample)
pcaValuesSample$class <- predQda$class

conf <- confusionMatrix(pcaValuesSample$class, pcaValuesSample$site)
distMetrics['parlamento','delicias'] <- conf$overall['Accuracy']
distMetrics['delicias','parlamento'] <- conf$overall['Accuracy']

# 5 - delicias-belen
pairData <- rbind(delicias,belen)
sampleSize = min(count(pairData,'site')$freq)

sample1 <- delicias[sample(nrow(delicias), sampleSize),]
sample2 <- belen[sample(nrow(belen), sampleSize),]
sample <- rbind(sample1,sample2)

logDataSample <- log(sample[,1:8])
pcaResultsSample <- princomp(logDataSample, center=T, scale=T)
pcaValuesSample <- as.data.frame(pcaResultsSample$scores)
pcaValuesSample$site <- factor(sample$site)

amphDA <- qda(site~Comp.1+Comp.2, data=pcaValuesSample, prior=c(1,1)/2)
predQda <- predict(amphDA, pcaValuesSample)
pcaValuesSample$class <- predQda$class

conf <- confusionMatrix(pcaValuesSample$class, pcaValuesSample$site)
distMetrics['delicias','belen'] <- conf$overall['Accuracy']
distMetrics['belen','delicias'] <- conf$overall['Accuracy']

# 6 - delicias-malpica

pairData <- rbind(delicias,malpica)
sampleSize = min(count(pairData,'site')$freq)

sample1 <- delicias[sample(nrow(delicias), sampleSize),]
sample2 <- malpica[sample(nrow(malpica), sampleSize),]
sample <- rbind(sample1,sample2)

logDataSample <- log(sample[,1:8])
pcaResultsSample <- princomp(logDataSample, center=T, scale=T)
pcaValuesSample <- as.data.frame(pcaResultsSample$scores)
pcaValuesSample$site <- factor(sample$site)

amphDA <- qda(site~Comp.1+Comp.2, data=pcaValuesSample, prior=c(1,1)/2)
predQda <- predict(amphDA, pcaValuesSample)
pcaValuesSample$class <- predQda$class

conf <- confusionMatrix(pcaValuesSample$class, pcaValuesSample$site)
distMetrics['delicias','malpica'] <- conf$overall['Accuracy']
distMetrics['malpica','delicias'] <- conf$overall['Accuracy']

# 7 - villaseca-belen

pairData <- rbind(villaseca,belen)
sampleSize = min(count(pairData,'site')$freq)

sample1 <- villaseca[sample(nrow(villaseca), sampleSize),]
sample2 <- belen[sample(nrow(belen), sampleSize),]
sample <- rbind(sample1,sample2)

logDataSample <- log(sample[,1:8])
pcaResultsSample <- princomp(logDataSample, center=T, scale=T)
pcaValuesSample <- as.data.frame(pcaResultsSample$scores)
pcaValuesSample$site <- factor(sample$site)

amphDA <- qda(site~Comp.1+Comp.2, data=pcaValuesSample, prior=c(1,1)/2)
predQda <- predict(amphDA, pcaValuesSample)
pcaValuesSample$class <- predQda$class

conf <- confusionMatrix(pcaValuesSample$class, pcaValuesSample$site)
distMetrics['villaseca','belen'] <- conf$overall['Accuracy']
distMetrics['belen','villaseca'] <- conf$overall['Accuracy']

# 8 - villaseca-parlamento

pairData <- rbind(villaseca,parlamento)
sampleSize = min(count(pairData,'site')$freq)

sample1 <- villaseca[sample(nrow(villaseca), sampleSize),]
sample2 <- parlamento[sample(nrow(parlamento), sampleSize),]
sample <- rbind(sample1,sample2)

logDataSample <- log(sample[,1:8])
pcaResultsSample <- princomp(logDataSample, center=T, scale=T)
pcaValuesSample <- as.data.frame(pcaResultsSample$scores)
pcaValuesSample$site <- factor(sample$site)

amphDA <- qda(site~Comp.1+Comp.2, data=pcaValuesSample, prior=c(1,1)/2)
predQda <- predict(amphDA, pcaValuesSample)
pcaValuesSample$class <- predQda$class

conf <- confusionMatrix(pcaValuesSample$class, pcaValuesSample$site)
distMetrics['villaseca','parlamento'] <- conf$overall['Accuracy']
distMetrics['parlamento','villaseca'] <- conf$overall['Accuracy']


# 9 - villaseca-malpica

pairData <- rbind(villaseca,malpica)
sampleSize = min(count(pairData,'site')$freq)

sample1 <- villaseca[sample(nrow(villaseca), sampleSize),]
sample2 <- malpica[sample(nrow(malpica), sampleSize),]
sample <- rbind(sample1,sample2)

logDataSample <- log(sample[,1:8])
pcaResultsSample <- princomp(logDataSample, center=T, scale=T)
pcaValuesSample <- as.data.frame(pcaResultsSample$scores)
pcaValuesSample$site <- factor(sample$site)

amphDA <- qda(site~Comp.1+Comp.2, data=pcaValuesSample, prior=c(1,1)/2)
predQda <- predict(amphDA, pcaValuesSample)
pcaValuesSample$class <- predQda$class

conf <- confusionMatrix(pcaValuesSample$class, pcaValuesSample$site)
distMetrics['villaseca','malpica'] <- conf$overall['Accuracy']
distMetrics['malpica','villaseca'] <- conf$overall['Accuracy']


# 10 - villaseca- delicias

pairData <- rbind(villaseca,delicias)
sampleSize = min(count(pairData,'site')$freq)

sample1 <- villaseca[sample(nrow(villaseca), sampleSize),]
sample2 <- delicias[sample(nrow(delicias), sampleSize),]
sample <- rbind(sample1,sample2)

logDataSample <- log(sample[,1:8])
pcaResultsSample <- princomp(logDataSample, center=T, scale=T)
pcaValuesSample <- as.data.frame(pcaResultsSample$scores)
pcaValuesSample$site <- factor(sample$site)

amphDA <- qda(site~Comp.1+Comp.2, data=pcaValuesSample, prior=c(1,1)/2)
predQda <- predict(amphDA, pcaValuesSample)
pcaValuesSample$class <- predQda$class

conf <- confusionMatrix(pcaValuesSample$class, pcaValuesSample$site)
distMetrics['villaseca','delicias'] <- conf$overall['Accuracy']
distMetrics['delicias','villaseca'] <- conf$overall['Accuracy']


distMetrics

###create a plot with the result of spatial distance and morphometric distance#### 

#plot of the distances with you have a database with the geodistance and pottery distance. 

library(ggplot2)
foo <- read.csv('distmetrics.csv', header=T, sep=",")
pdf('plot_dists.pdf')
ggplot(foo, aes(x=distpottery, y=distgeo)) + geom_point(aes(shape=from, col=to), size=3)
dev.off()

###glind both options ###


g1 <- ggplot(foo, aes(x=from, y=distgeo)) + geom_point(aes(col=to), size=3)

g2 <- ggplot(foo, aes(x=from, y=distpottery)) + geom_point(aes(col=to), size=3)

pdf('distgeopott.pdf')
grid.arrange(arrangeGrob(g2,g1), heights=c(1/3, 1/3))
dev.off()

pdf('distgeopott.pdf')
grid.arrange(arrangeGrob(g2,g1))
dev.off()


##without taking into account from and to (using auto method) 

pdf('smooth.pdf')
ggplot(foo, aes(x=distpottery, y=distgeo)) + geom_point(size=3) + geom_smooth(method= "auto")
dev.off()

##more examples with geom_smooth

pdf('smooth3.pdf')
ggplot(foo, aes(x=distgeo, y=distpottery)) + geom_point(size=3) + geom_smooth(method= "glm")
dev.off()



#########DISTANCE MEASUREMENT AND PLOT FROM XAVI RUBIO DATA ##############################

library(ggplot2)
library(gridExtra)

# raw spatial distances through the river
spatialDist <- data.frame(site=c('parlamento','belen','malpica','delicias'), distance=c(0,72,83,97))
spatialDist$site <- factor(spatialDist$site, levels = c('parlamento','belen','malpica','delicias'))

g1 <- ggplot(spatialDist, aes(x=distance, y=0, col=site)) + geom_segment(col='grey40', x=0, xend=max(spatialDist$distance), y=0, yend=0) + geom_point()  + geom_text(aes(label=site), vjust=-1)  + theme_bw() + theme(axis.text.y=element_blank(), axis.ticks.y=element_blank(), panel.grid.major=element_blank(), panel.grid.minor=element_blank(), axis.title.y=element_blank()) + xlab('distance (km)') + scale_color_manual(values=c("skyblue3","goldenrod1","indianred2","palegreen4")) + theme(legend.position='none') + ylim(c(-0.05,0.1))


# PCA data
foo <- read.csv('dres.csv', sep=",", header=T)
bar <- subset(foo, !type %in% c('Dressel 23', 'Dressel A', 'Dressel B', ''))

logData <- log(bar[,5:12])

pcaLogResults <- princomp(logData, center=T, scale=T)

pcaLogValues <- as.data.frame(pcaLogResults$scores)
# west-east order
pcaLogValues$site <- factor(bar$site, levels = c('parlamento','belen','malpica','delicias'))

g2 <- ggplot(pcaLogValues, aes(y=Comp.1, x=Comp.2, col=site)) + geom_point() + facet_wrap(~site, ncol=4) + theme_bw() + theme(legend.position='none') + scale_color_manual(values=c("skyblue3","goldenrod1","indianred2","palegreen4"))

# final composition

pdf('pcs.pdf', width=12, height=4)    
grid.arrange(arrangeGrob(g2,g1,heights=c(2/3, 1/3)))
dev.off()


####TESTING WITH MANTEL TEST#########

library('vegan')
#first you have to convert your database in a matrix between each things that you can use. In our case, we use distgeo and distpottery and we have to convert in a matrix. 
alld=read.csv("distmetrics.csv")

distg=as.dist(xtabs(alld[, "distgeo"] ~ alld[, "from"] + alld[, "to"]), diag=T)
distpot=1-as.dist(xtabs(alld[, "distpottery"] ~ alld[, "from"] + alld[, "to"]), diag=T)
mantel(distg,distpot)

#testing with Confusion Matrix

distg=as.dist(xtabs(alld[, "distgeo"] ~ alld[, "from"] + alld[, "to"]), diag=T)
distpot=1-as.dist(xtabs(alld[, "confusion"] ~ alld[, "from"] + alld[, "to"]), diag=T)
mantel(distg,distpot)

#Other way to testing with Confusion Matrix

alld2=read.csv("distmetrics.csv")
# move to 0/1 interval
alld2$normConfusion = (max(alld2$confusion)-alld2$confusion)/(max(alld2$confusion)-min(alld2$confusion))

distg2=as.dist(xtabs(alld2[, "distgeo"] ~ alld2[, "from"] + alld2[, "to"]))
distpot2=as.dist(xtabs(alld2[, "normConfusion"] ~ alld2[, "from"] + alld2[, "to"]))
mantel(distg2,distpot2)

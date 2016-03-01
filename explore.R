setwd('~/Documents')
data <- read.csv('train.csv')
data$v22 <- NULL
data$target <- as.factor(data$target)
data$ID <- as.factor(data$ID)

table(data$v3)
quantile(data$v1)
summary(data)
plot(data$v1)
hist(data$v1)
plot(density(na.omit(data$v1)))

data.complete <- data[complete.cases(data),]

plot(data1[4:10],cex=.1)
cor(data1$v2,data1$v4)
?cor
cor(data1[6:10])

data.numeric <- data.complete[,sapply(data.complete,is.numeric)]
data.numeric$ID <- NULL

data.scaled <- scale(data.numeric)

pc.cr <- princomp(data.scaled[,1:100])
data.pca <- predict(pc.cr)
plot(data.pca[,1:3], cex=.1)

cormat<-cor(data2)

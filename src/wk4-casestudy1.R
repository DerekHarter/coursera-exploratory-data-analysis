load('../data/samsungData.rda')

samsungData <- transform(samsungData, activity = factor(activity))
sub1 <- subset(samsungData, subject == 1)

# average acceleration
par(mfrow = c(1,2), mar = c(5,4,1,1))
plot(sub1[,1], col = sub1$activity, ylab = names(sub1)[1])
plot(sub1[,2], col = sub1$activity, ylab = names(sub1)[2])
legend("bottomright", legend = unique(sub1$activity), 
       col = unique(sub1$activity), pch = 1)

source("mypclust.R")
dm <- dist(sub1[, 1:3])
hclustering <- hclust(dm)
par(mfrow = c(1,1))
mypclust(hclustering, lab.col = unclass(sub1$activity))

# maximum acceleration
par(mfrow = c(1,2))
plot(sub1[, 10], pch = 19, col = sub1$activity, ylab = names(sub1)[10])
plot(sub1[, 11], pch = 19, col = sub1$activity, ylab = names(sub1)[11])

dm <- dist(sub1[, 10:12])
hclustering <- hclust(dm)
par(mfrow = c(1,1))
mypclust(hclustering, lab.col = unclass(sub1$activity))

# singular value decomposition
svd1 = svd(scale(sub1[, -c(562:563)]))

par(mfrow = c(1,2))
plot(svd1$u[, 1], col = sub1$activity, pch=19)
plot(svd1$u[, 2], col = sub1$activity, pch = 19)

par(mfrow = c(1,1))
plot(svd1$v[, 2], pch = 19)

maxContrib <- which.max(svd1$v[, 2])
dm <- dist(sub1[, c(10:12, maxContrib)])
hclustering <- hclust(dm)
mypclust(hclustering, lab.col = unclass(sub1$activity))

# kmeans clustering
kClust <- kmeans(sub1[, -c(562, 563)], centers = 6)
table(kClust$cluster, sub1$activity)

kClust <- kmeans(sub1[, -c(562, 563)], centers = 6, nstart=1)
table(kClust$cluster, sub1$activity)

plot(kClust$center[1, 1:10], pch=19, ylab = "Cluster Center", xlab="")

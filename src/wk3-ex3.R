# no pattern
set.seed(12345)
par(mfrow = c(1,1))
par(mar = rep(0.2, 4))
dm <- matrix(rnorm(400), nrow=40)
image(1:10, 1:40, t(dm))

heatmap(dm)

# what if we add a pattern?
set.seed(678910)
for (i in 1:40) {
    # flip a coin
    coinFlip <- rbinom(1, size = 1, prob = 0.5)
    
    # if coin is heads, add a common pattern to that row
    if (coinFlip) {
        dm[i,] <- dm [i,] + rep(c(0,3), each = 5)
    }
}

# patterns in rows and columns
hh <- hclust(dist(dm))
dmordered <- dm[hh$order,]

par(mfrow = c(1,3), mar = c(4,4,1,1))
image(t(dmordered)[, nrow(dmordered):1])
plot(rowMeans(dmordered), 40:1, xlab = "Row Mean", ylab = "Row", pch=19)
plot(colMeans(dmordered), xlab = "Column", ylab = "Column Mean", pch = 19)

svd1 <- svd(scale(dmordered))

par(mfrow = c(1,3))
image(t(dmordered)[, nrow(dmordered):1])
plot(svd1$u[,1], 40:1, xlab = "Row", ylab = "First left singular vector", pch=19)
plot(svd1$v[, 1], xlab = "Column", ylab = "First right singular vector", pch=19)

par(mfrow = c(1,2))
plot(svd1$d, xlab = "Column", ylab = "Singular value", pch = 19)
plot(svd1$d^2/sum(svd1$d^2), xlab = "Column", ylab = "Prop of variance explained", pch=19)

svd1 <- svd(scale(dmordered))
pca1 <- prcomp(dmordered, scale = TRUE)

plot(pca1$rotation[, 1], svd1$v[,1], pch = 19, xlab = "Principle Component1",
     ylab = "Right Singular Vector 1")
abline(c(0,1))



svd1 <- svd(cm)
par(mfrow = c(1,3))
image(t(cm)[,nrow(cm):1])
plot(svd1$d, xlab="Column", ylab="Singular value", pch=19)
plot(svd1$d^2/sum(svd1$d^2), xlab="Column", ylab="Prop of variance explained", pch=19)


# what if we add a second pattern
set.seed(678910)
for (i in 1:40) {
    # flip a coin
    coinFlip1 <- rbinom(1, size = 1, prob = 0.5)
    coinFlip2 <- rbinom(1, size = 1, prob = 0.5)
    
    # if coin is heads add a common pattern to that row
    if (coinFlip1) {
        dm[i,] <- dm[i,] + rep(c(0, 5), each = 5)
    }
    if (coinFlip2) {
        dm[i,] <- dm[i,] + rep(c(0, 5), 5)
    }
}

hh <- hclust(dist(dm))
dmordered <- dm[hh$order,]

svd2 <- svd(scale(dmordered))

par(mfrow = c(1,3))
image(t(dmordered)[, nrow(dmordered):1])
plot(rep(c(0, 1), each = 5), pch = 19, xlab = "Column", ylab = "Pattern 1")
plot(rep(c(0, 1), 5), pch = 19, xlab = "Column", ylab = "Pattern 2")  

par(mfrow = c(1,3))
image(t(dmordered)[, nrow(dmordered):1])
plot(svd2$v[,1], pch = 19, xlab = "Column", ylab = "First right singular vector")
plot(svd2$v[,2], pch = 19, xlab = "Column", ylab = "Second right singular vector")     

par(mfrow = c(1,2))
plot(svd2$d, xlab = "Column", ylab = "Singular value", pch=19)
plot(svd2$d^2/sum(svd2$d^2), xlab = "Column", ylab = "Percent of variance explained", pch=19)     

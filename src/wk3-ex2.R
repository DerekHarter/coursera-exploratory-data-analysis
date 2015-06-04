set.seed(1234)
x <- rnorm(12, mean = rep(1:3, each = 4), sd = 0.2)
y <- rnorm(12, mean = rep(c(1, 2, 1), each = 4), sd = 0.2)

par(mar = c(3,3,1,1))
plot(x, y, col = "blue", pch=19, cex=2)
text(x + 0.05, y + 0.05, labels = as.character(1:12))

df <- data.frame(x=x, y=y)
kmeansObj <- kmeans(df, centers = 3)
names(kmeansObj)

plot(x, y, col = kmeansObj$cluster, pch = 19, cex = 2)
points(kmeansObj$centers, col = 1:3, pch = 3, cex = 3, lwd = 3)

set.seed(1234)
dm <- as.matrix(df)[sample(1:12),]
kmeansObj2 <- kmeans(dm, centers = 3)

par(mfrow = c(1,2), mar=c(2,4,0.1,0.1))
image(t(dm)[, nrow(dm):1], yaxt = "n")
image(t(dm)[, order(kmeansObj2$cluster)], yaxt = "n", col=topo.colors(100))

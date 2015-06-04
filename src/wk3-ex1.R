set.seed(1234)
x <- rnorm(12, mean = rep(1:3, each = 4), sd = 0.2)
y <- rnorm(12, mean = rep(c(1, 2, 1), each = 4), sd = 0.2)

par(mar = c(3,3,1,1))
plot(x, y, col = "blue", pch=19, cex=2)
text(x + 0.05, y + 0.05, labels = as.character(1:12))

df <- data.frame(x=x, y=y)
distxy <- dist(df)
hClustering <- hclust(distxy)
plot(hClustering)

mypclust <- function(hclust, lab = hclust$labels, lab.col = rep(1, length(hclust$labels)),
                     hang = 0.1, ...) {
    ## modification of plclust for plotting hclust objects *in color*! Copyright
    ## Eva KF Chan 2009 Arguments: hclust: hclust object lab: a character vector
    ## of labels of the leaves of the tree lab.col: color for the labels;
    ## NA=default device foreground color hant: as in hclust & pclust Side
    ## effect: A display of hierarchical cluster with colored leaf labels.
    y <- rep(hclust$height, 2)
    x <- as.numeric(hclust$merge)
    y <- y[which(x < 0)]
    x <- x[which(x < 0)]
    x <- abs(x)
    y <- y[order(x)]
    x <- x[order(x)]
    plot(hclust, labels = FALSE, hang = hang, ...)
    text(x = x, y = y[hclust$order] - (max(hclust$height) * hang), labels = lab[hclust$order],
         col = lab.col[hclust$order], srt = 90, adj = c(1, 0.5), xpd = NA, ...)
}
mypclust(hClustering, lab = rep(1:3, each = 4), lab.col = rep(1:3, each = 4))

set.seed(143)
dm <- as.matrix(df)[sample(1:12), ]
heatmap(dm)


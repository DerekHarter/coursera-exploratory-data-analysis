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

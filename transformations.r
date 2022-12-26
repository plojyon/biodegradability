mcf <- function(data) {
    #' Return the major class frequency per each column
    return(sapply(data, function(x) max(table(x)) / sum(table(x))))
}
correlation <- function(data, threshold) {
    #' Return pairs of features with correlations above a threshold
    mat = cor(na.omit(data[, -ncol(data)]))
    mat[lower.tri(mat, diag = TRUE)] <- 0
    correlations <- which(abs(mat) > threshold, arr.ind = TRUE)
    rownames(correlations) <- NULL
    colnames(correlations) <- NULL
    annotated = t(apply(
        correlations,
        1,
        function(x) c(x[1], x[2], mat[x[1], x[2]])
    ))
    return(annotated)
}

transform.pca <- function(data) {
    #' Transform data using PCA
    pca <- prcomp(data[,-ncol(data)], center=TRUE, scale=TRUE)
    ret = data.frame(pca$x)
    ret$Class <- data$Class
    return(ret)
}

outliers.winsorize <- function(data, threshold=0.05) {
    #' Winsorize outliers
    for (i in 1:ncol(data)) {
        if (typeof(data[,i]) != "double") next
        quantiles <- quantile(
            data[,i],
            probs=c(threshold, 1-threshold),
            na.rm=TRUE
        )
        data[,i] <- ifelse(data[,i] < quantiles[1],
            quantiles[1],
            ifelse(data[,i] > quantiles[2],
                quantiles[2],
                data[,i]
            )
        )
    }
    return(data)
}

impute.mean <- function(data) {
    #' Impute missing values with the mean of the column
    d = apply(data, 2, function(x) ifelse(is.na(x), mean(x, na.rm = TRUE), x))
    return(data.frame(d))
}
impute.knn <- function(data) {
	
}
impute.mice <- function(data) {
	print("Not implemented")
}

prune.mcf <- function(data, cutoff = 0.95) {
    #' Prune features with major class frequency > cutoff
    MCF = mcf(data)
    return(data[, names(MCF[MCF <= cutoff])])
}
prune.variance <- function(data, cutoff = 0.1) {
    #' Prune features with variance < cutoff
    return(data[, apply(data, 2, function(x) var(x, na.rm=TRUE)) >= cutoff])
}
prune.correlated <- function(data, threshold) {
    #' Prune features that are highly correlated with each other
    correlations = correlation(data, threshold)
    return(data[, !colnames(data) %in% correlations[,1]])
}
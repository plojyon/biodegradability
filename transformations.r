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
dist <- function(datum, data) {
    #' Calculate the distance from datum to each row in data
    return(apply(data, 1, function(x) sqrt(sum((as.integer(datum) - as.integer(x))^2))))
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
    for (i in 1:ncol(data)) {
        if (typeof(data[,i]) %in% c("double", "integer")) {
            data[,i] <- ifelse(is.na(data[,i]),
                mean(data[,i], na.rm=TRUE),
                data[,i]
            )
        }
    }
    return(data.frame(data))
}
impute.filter <- function(data) {
    #' Remove rows with missing data
    return(data[complete.cases(data),])
}
impute.knn <- function(data, k=1) {
	#' Impute using k nearest neighbours
    to_impute = data[!complete.cases(data),]
    for (row in rownames(to_impute)) {
        incomplete_row = colnames(data[row,])[apply(data[row,], 2, anyNA)]
        incomplete_filter = (names(data[row,]) %in% incomplete_row) # booleans
        distances = dist(data[row,!incomplete_filter], data[complete.cases(data),!incomplete_filter])
        k_nearest = data[complete.cases(data),][order(distances)[1:k],]
        
        # replace only na's
        for (col in incomplete_row) {
            if (typeof(data[,col]) %in% c("double", "integer")) {
                data[row,col] <- ifelse(is.na(data[row,col]),
                    mean(k_nearest[,col], na.rm=TRUE),
                    data[row,col]
                )
            }
        }
    }
    return(data.frame(data))
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
prune.correlated <- function(data, threshold=0.6) {
    #' Prune features that are highly correlated with each other
    correlations = correlation(data, threshold)
    return(data[, !colnames(data) %in% correlations[,1]])
}

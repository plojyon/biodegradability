df <- read.csv("train.csv")

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
	annotated = t(apply(correlations, 1, function(x) c(x[1], x[2], mat[x[1], x[2]])))
	return(annotated)
}

outliers.winsorize <- function(data, threshold=0.05) {
	#' Winsorize outliers
	for (i in 1:ncol(data)) {
		if (typeof(data[,i]) != "double") next
		quantiles <- quantile(data[,i], probs=c(threshold, 1-threshold), na.rm=TRUE)
		data[,i] <- ifelse(data[,i] < quantiles[1], quantiles[1], ifelse(data[,i] > quantiles[2], quantiles[2], data[,i]))
	}
	return(data)
}

save_plots <- function(data, width, height, folder, plot_function, plots_per_page = c(1,1), plot_count=NaN) {
    #' Generate N plots
    if (is.na(plot_count)) plot_count = ncol(data)
    for (page in 1:(plot_count / sum(plots_per_page))) {
        png(paste(folder, "/page", page, ".png", sep=""), width, height)
        par(mfrow=plots_per_page)
        start = (page-1) * sum(plots_per_page) + 1
        end = min(plot_count, page * sum(plots_per_page))
		for (col in start:end) {
            plot_function(col)
		}
		dev.off()
	}
}
save_boxplots <- function(data, width, height, folder="box", plots_per_page = c(1,5)) {
	#' Generate boxplots of each column
    colour <- function(col)
        ifelse(typeof(col) == "double", "blue", "red")
	save_plots(
		data,
        width,
        height,
		folder,
        function(col)
            boxplot(data[,col], main=colnames(data)[col], width=1, border=colour(col)),
		plots_per_page
	)
}
save_barplots <- function(data, width, height, folder="bar", plots_per_page = c(1,5)) {
	#' Generate barplots of each column
    colour <- function(col)
        ifelse(typeof(col) != "double", "blue", "red")
	save_plots(
		data,
        width,
        height,
		folder,
        function(col)
            barplot(table(data[,col]), main=colnames(data)[col], border=colour(col)),
		plots_per_page
	)
}
save_scatterplots <- function(data, width=10000, height=10000, folder="scatter", plots_per_page = c(5,5)) {
    #' Generate scatterplots of each column, colour them by class
    save_plots(
        data,
        width,
        height,
        folder,
        function(col)
            pairs(data, col=adjustcolor(c("red", "blue", alpha=0.5)[data$Class])),
        plots_per_page=c(1,1),
        plot_count=1
    )
}

analyze <- function(data) {
	#' Perform general data analysis
	print("Percentage of rows with missing features:")
	print(sum(apply(data, 1, function(x) any(is.na(x)))) / nrow(data))

	print("Number of missing features per row:")
	print(table(apply(data, 1, function(x) sum(is.na(x)))))

	print("Total missing features by column:")
	missing_values <- apply(data, 2, function(x) sum(is.na(x)))
	print(missing_values[missing_values > 0])

	print("Class counts:")
	groups = split(data, data$Class)
	print(sapply(groups, nrow))

	print("Number of unique values per column:")
	print(sapply(data, function(x) length(unique(x))))

	print("Major class frequency per column:")
	MCF = mcf(data)
	print(MCF)
	print(MCF[MCF > 0.95])

	print("Correlation between features:")
	print(correlation(data, 0.90))

	print("Variance of features:")
	print(apply(data, 2, var))

	print("Outliers per column:")
	for (col in 1:ncol(data)) {
		s = colnames(data)[col]
		if (typeof(data[,col]) != "double")
			num = 0
		else 
			num = sum(
				data[,col] < quantile(data[,col], 0.05, na.rm=TRUE) |
				data[,col] > quantile(data[,col], 0.95, na.rm=TRUE)
			) / length(data[,col])
		print(paste(s, num, sep=": "))	
	}
}

classify.majority.train <- function(data) {
	#' Train a majority class classifier
	return(names(which.max(table(data$Class))))
}
classify.majority.execute <- function(classifier, datum) {
	#' Classify using majority class
	return(classifier)
}

classify.random.train <- function(data) {
	#' Train a random classifier
	return(c())
}
classify.random.execute <- function(classifier, datum) {
	#' Classify using random class
	return(sample(2, 1))
}

analyze(df)
save_boxplots(df, "plots")
save_boxplots(winsorize(df), "plots_winsorized")
save_barplots(winsorize(df), "histograms")


classifier = "majority"
train = match.fun(paste("classify.", classifier, ".train", sep=""))
execute = match.fun(paste("classify.", classifier, ".execute", sep=""))

preprocess = compose(outliers.winsorize, prune.mcf, prune.variance)
execute(train(preprocess(df)), testing_data)

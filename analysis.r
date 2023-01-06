source("transformations.r")

colour <- function(datatype) ifelse(datatype == "double", "blue", "red")

save_plots <- function(data, width, height, plot_function, plots_per_page = c(1,1), plot_count=NaN, filename=function(page) page) {
    #' Generate N plots
    if (is.na(plot_count)) plot_count = ncol(data)
	if (typeof(filename) == "character") filename = local(function(page) filename, list2env(list(filename=filename)))
    for (page in 1:floor(plot_count / prod(plots_per_page))) {
		name = as.character(filename(page))
        png(paste(name, ".png", sep=""), width, height)
        par(mfrow=plots_per_page)
        start = (page-1) * prod(plots_per_page) + 1
        end = min(plot_count, page * prod(plots_per_page))
        for (col in start:end) {
            plot_function(col)
        }
        dev.off()
    }
}
save_boxplots <- function(data, width=1000, height=1500, filename="boxplot") {
    #' Generate boxplots of each column
    save_plots(
        data=data,
        width=width,
        height=height,
        plot_function=function(col)
            boxplot(data[,colnames(data)[col]], width=1, border=colour(typeof(data[,col]))),
        plots_per_page=c(4,11),
		filename=filename
    )
}
save_barplots <- function(data, width=1000, height=2000, filename="barplot") {
    #' Generate barplots of each column
    save_plots(
        data=data,
        width=width,
        height=height,
        plot_function=function(col)
            barplot(table(data[,col]), main=colnames(data)[col], border=colour(typeof(data[,col]))),
        plots_per_page=c(11,4),
		filename=filename
    )
}
save_scatterplots <- function(data, width=10000, height=10000, filename="scatterplot") {
    #' Generate scatterplots of each column, colour them by class
    save_plots(
        data=data,
        width=width,
        height=height,
        plot_function=function(col)
            pairs(data, col=adjustcolor(c("red", "blue", alpha=0.5)[data$Class])),
        plots_per_page=c(1,1),
        plot_count=1,
		filename=filename
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

df <- read.csv("train.csv")

# analyze(df)
save_boxplots(df, filename="images/boxes")
save_boxplots(outliers.winsorize(df), filename="images/boxes_winsorized")
save_barplots(outliers.winsorize(df), filename="images/histograms")
# save_scatterplots(df, filename="images/scatters")
# save_scatterplots(transform.pca(impute.mean(df)), filename="images/scatters_pca")

source("transformations.r")
source("classifiers.r")
library(e1071)

k_fold_validate <- function(data, k, classifier, params = list()) {
    #' Perform k-fold cross validation on a dataset
    data = data[sample(nrow(data)),]
    fold_size = round(nrow(data) / k)
    fold_indices = seq(1, nrow(data), fold_size)
    fold_indices = c(fold_indices, nrow(data)+1)
    fold_accuracies = rep(1, k)
    for (fold in 1:k) {
        test_indices = seq(fold_indices[fold], fold_indices[fold+1]-1)
        fold_data = data[-test_indices,]
        fold_test = data[test_indices,]
        fold_classifier = classifier(fold_data, params)
        correct = as.integer(fold_classifier(fold_test)) == fold_test$Class
        fold_accuracies[fold] = sum(correct) / nrow(fold_test)
    }
    return(mean(fold_accuracies))
}

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

save_lda_scatter <- function(data, width=500, height=500, filename="scatterplot"){
    data = impute.knn(data)
    lda_classifier = classify.lda.train(data)
    lda_data = predict(lda_classifier, data)

    plot(lda_data$x[,1], lda_data$x[,2])
    text(lda_data$x[,1], lda_data$x[,2], data$Class, cex = 0.7, pos = 4, col = "red")
}

plot_gamma_svm <- function(data){
    data$Class = as.factor(data$Class)
    preprossess = compose(impute.knn, prune.variance)
    datalda = preprossess(data)
    lda_classifier = classify.lda.train(datalda)
    data[,ncol(data) + 1] = predict(lda_classifier, datalda)$x[,1]
    testing_data[,ncol(testing_data) + 1] = predict(lda_classifier, testing_data)$x[,1]

    data = preprossess(data)
    gamma = seq(0.001, 1, 0.01)
    y = rep(0, length(gamma))
    for (i in 1:length(gamma)){
        y[i] = mean(k_fold_validate(data, 5, classify.svm, list(gamma=gamma[i])))
        print(c(gamma[i], y[i]))
    }
    x11()
    plot(gamma, y, type="l", xlab="gamma", ylab="accuracy")
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
# save_boxplots(df, filename="images/boxes")
# save_boxplots(outliers.winsorize(df), filename="images/boxes_winsorized")
# save_barplots(outliers.winsorize(df), filename="images/histograms")
# save_scatterplots(df, filename="images/scatters")
# save_scatterplots(transform.pca(impute.mean(df)), filename="images/scatters_pca")

#save_lda_scatter(df, filename="images/lda_scatter")

plot_gamma_svm(df)
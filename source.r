#install.packages(c("arules", "pryr"))
library(PerformanceAnalytics)
library(arules)
library(purrr)
library(pryr)

source("transformations.r")
source("classifiers.r")

if("print" %in% ls()){
    rm(print)
}
pprint <- function(...){ print(paste(...)) }

k_fold_validate <- function(data, k, classifier) {
    #' Perform k-fold cross validation on a dataset
    # shuffle the data
    data = data[sample(nrow(data)),]
    fold_size = round(nrow(data) / k)
    fold_indices = seq(1, nrow(data), fold_size)
    fold_indices = c(fold_indices, nrow(data)+1)
    fold_accuracies = rep(1, k)
    for (fold in 1:k) {
        test_indices = seq(fold_indices[fold], fold_indices[fold+1]-1)
        fold_data = data[-test_indices,]
        fold_test = data[test_indices,]
        fold_classifier = classifier(fold_data)
        correct = as.integer(fold_classifier(fold_test)) == fold_test$Class
        fold_accuracies[fold] = sum(correct) / nrow(fold_test)
    }
    return(mean(fold_accuracies))
}


df <- read.csv("train.csv")
testing_data <- read.csv("test.csv")
df$Class = as.factor(df$Class)
testing_data$Class = as.factor(testing_data$Class)

preprossesses = c(
    function(data) {
        preprossess = compose(impute.knn, prune.variance)

        datalda = preprossess(data)
        lda_classifier = classify.lda.train(datalda)
        data[,ncol(data) + 1] = predict(lda_classifier, datalda)$x[,1]
        testing_data[,ncol(testing_data) + 1] = predict(lda_classifier, testing_data)$x[,1]

        return(preprossess(data))
    },
    function(data) {
        preprossess = compose(impute.knn, prune.variance)
        data[,(ncol(data) + 1):(ncol(data) + 3)] = transform.pca(preprossess(data))$x[,1:3]
        return(preprossess(data))
    },
    compose(impute.knn, prune.variance),
    compose(outliers.winsorize, prune.mcf, prune.variance, impute.knn),
    compose(impute.knn, outliers.winsorize, prune.mcf, prune.variance),
    compose(impute.knn, prune.mcf, prune.variance, outliers.winsorize),
    compose(impute.knn, prune.variance, outliers.winsorize)
)

# result matrix with names columns
results = matrix(0, nrow=length(classifiers), ncol=length(preprossesses))
rownames(results) = classifiers

i=1
for (classifier in classifiers) {
    j=1
    for (preprossess in preprossesses){
        results[i,j] = k_fold_validate(preprossess(df), 5, get_classifier(classifier))
        j = j + 1
    }
    print(i)
    i = i + 1
}

print(results)
# add colnames
colnames(results) = c(
    "lda column",
    "pca column",
    "knn+variance",
    "preprocess 1",
    "preprocess 2",
    "preprocess 3",
    "preprocess 4"
    )
print(results)
#k_fold_validate(preprossess(df), 5, classify.svm)


preprossess = compose(impute.knn, prune.variance)
datalda = preprossess(df)
lda_classifier = classify.lda.train(datalda)
df[,ncol(df) + 1] = predict(lda_classifier, datalda)$x[,1]
testing_data[,ncol(testing_data) + 1] = predict(lda_classifier, testing_data)$x[,1]
classifier = classify.svm(preprossess(df))
results = c()
for (row in rownames(testing_data)) {
    test = testing_data[row, names(testing_data) != "Class"]
    results = c(results, tryCatch(as.integer(classifier(test)), error=function(e) print(e)))
}
contingency_table = table(results, testing_data$Class)
print(contingency_table)
print(sum(diag(contingency_table))/sum(contingency_table))
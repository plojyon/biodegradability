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
    fold_size = round(nrow(data) / k)
    fold_indices = seq(1, nrow(data), fold_size)
    fold_indices = c(fold_indices, nrow(data)+1)
    fold_accuracies = rep(1, k)
    for (fold in 1:k) {
        test_indices = seq(fold_indices[fold], fold_indices[fold+1]-1)
        fold_data = data[-test_indices,]
        fold_test = data[test_indices,]
        fold_classifier = classifier(fold_data)
        correct = fold_classifier(fold_test) == fold_test$Class
        fold_accuracies[fold] = sum(correct) / nrow(fold_test)
    }
    return(mean(fold_accuracies))
}


df <- read.csv("train.csv")
testing_data <- read.csv("test.csv")
df$Class = as.factor(df$Class)
testing_data$Class = as.factor(testing_data$Class)


preprocess = compose(outliers.winsorize, prune.mcf, prune.variance, impute.filter)

# impute.knn(df)
# k_fold_validate(preprocess(df), 3, classify.random_forest)

classifier = classify.random_forest(preprocess(df))
results = c()
for (row in rownames(testing_data)) {
    test = testing_data[row, names(testing_data) != "Class"]
    results = c(results, tryCatch(as.integer(classifier(test)), error=function(e) "error"))
}
contingency_table = table(results, testing_data$Class)
print(contingency_table)
print(sum(diag(contingency_table))/sum(contingency_table))


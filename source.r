#install.packages(c("arules", "pryr"))
library(PerformanceAnalytics)
library(arules)
library(purrr)
library(pryr)
library(pROC)

source("transformations.r")
source("classifiers.r")

options(width=120)

if("print" %in% ls()){
    rm(print)
}
pprint <- function(...){ print(paste(...)) }


metrics <- function(data, results) {
    contingency_table = table(factor(as.integer(results), c(1, 2)), factor(data$Class, c(1, 2)))
    
    tp = contingency_table[1,1] # true positive
    fp = contingency_table[1,2] # false positive
    fn = contingency_table[2,1] # false negative
    tn = contingency_table[2,2] # true negative
    p = tp + fn # positive
    n = tn + fp # negative
    pp = tp + fp # predicted positive
    pn = tn + fn # predicted negative

    shita = as.vector(data$Class, mode="numeric")
    shitb = as.vector(results, mode="numeric")
    return(list(
        Accuracy=((tp+tn)/(tp+tn+fp+fn)),
        F1=(2*tp/(2*tp + fp + fn)),
        Precision=(tp/pp),
        Recall=(tp/p),
        AUC=(roc(shita, shitb, quiet=TRUE)$auc)
    ))
}

k_fold_validate <- function(data, k, classifier) {
    #' Perform k-fold cross validation on a dataset
    #' @param data The data to validate
    #' @param k The number of folds
    #' @param classifier A function that takes a dataset and returns a classifier
    #' @param metric The metric to use for validation is as_table is false

    data = data[sample(nrow(data)),]
    fold_size = round(nrow(data) / k)
    fold_indices = seq(1, nrow(data), fold_size)
    fold_indices = c(fold_indices, nrow(data)+1)
    fold_accuracies = array(0, c(k, 5))
    for (fold in 1:k) {
        test_indices = seq(fold_indices[fold], fold_indices[fold+1]-1)
        fold_data = data[-test_indices,]
        fold_test = data[test_indices,]
        fold_classifier = classifier(fold_data)
        fold_accuracies[fold,] = as.numeric(metrics(fold_test, fold_classifier(fold_test)))
    }
    return(fold_accuracies)
}


df <- read.csv("train.csv")
testing_data <- read.csv("test.csv")
df$Class = as.factor(df$Class)
testing_data$Class = as.factor(testing_data$Class)

preprossesses = list(
    "LDA"=function(data) {
        preprossess = compose(impute.knn, prune.variance)

        datalda = preprossess(data)
        lda_classifier = classify.lda.train(datalda)
        data[,ncol(data) + 1] = predict(lda_classifier, datalda)$x[,1]
        testing_data[,ncol(testing_data) + 1] = predict(lda_classifier, testing_data)$x[,1]

        return(preprossess(data))
    },
    "PCA"=function(data) {
        preprossess = compose(impute.knn, prune.variance)
        data[,(ncol(data) + 1):(ncol(data) + 3)] = transform.pca(preprossess(data))$x[,1:3]
        return(preprossess(data))
    },
    "knn+var"=compose(impute.knn, prune.variance),
    "prep1"=compose(outliers.winsorize, prune.mcf, prune.variance, impute.knn),
    "prep2"=compose(impute.knn, outliers.winsorize, prune.mcf, prune.variance),
    "prep3"=compose(impute.knn, prune.mcf, prune.variance, outliers.winsorize),
    "prep4"=compose(impute.knn, prune.variance, outliers.winsorize)
)

clacifiers = list(
    "majority"=get_classifier("majority"),
    "random"=get_classifier("random"),
    "bayes"=get_classifier("bayes"),
    "lda"=get_classifier("lda"),
    "random_forest"=get_classifier("random_forest"),
    "svm"=get_classifier("svm", list(gamma=0.1))
)



# result matrix with names columns
results = array(0, 
    c(length(clacifiers),length(preprossesses), 5),
    dimnames=list(
        names(clacifiers),
        names(preprossesses),
        c("Accuracy","F1","Precision","Recall","AUC")
    )
)


i=1
for (classifier in clacifiers) {
    j=1
    for (preprossess in preprossesses) {
        print(paste(names(clacifiers)[i], names(preprossesses)[j], sep=" / "))
        results[i,j,] = colMeans(k_fold_validate(preprossess(df), 5, classifier))
        j = j + 1
    }
    i = i + 1
}

print(results)

# run on best classifier only (borken rn)
# results = c()
# for (row in rownames(testing_data)) {
#     test = testing_data[row, names(testing_data) != "Class"]
#     results = c(results, tryCatch(as.integer(classifier(test)), error=function(e) print(e)))
# }

# contingency_table = table(results, testing_data$Class)
# print(contingency_table)
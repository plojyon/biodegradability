#install.packages(c("arules", "pryr"))
library(PerformanceAnalytics)
library(arules)
library(purrr)
library(pryr)

source("transformations.r")
source("classifiers.r")

df <- read.csv("train.csv")
testing_data <- read.csv("test.csv")


preprocess = compose(outliers.winsorize, prune.mcf, prune.variance)
classifier = classify.bayes(preprocess(df))
results = c()
for (row in rownames(testing_data)) {
    test = testing_data[row, names(testing_data) != "Class"]
    results = c(results, tryCatch(classifier(test), error=function(e) "failed"))
}

print(table(results, testing_data$Class))

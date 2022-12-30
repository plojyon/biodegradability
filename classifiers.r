library(MASS)
library(naivebayes)

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

classify.lda.train <- function(data) {
    #' Train a linear discriminant analysis classifier
    return(lda(Class ~ ., data=data))
}
classify.lda.execute <- function(classifier, datum) {
    #' Classify using linear discriminant analysis
    return(predict(classifier, datum)$class)
}

classify.bayes.train <- function(data) {
    #' Train a naive bayes classifier
    # # first convert all integer columns to factors
    # for (i in 1:ncol(data)) {
    #     if (typeof(data[,i]) == "integer") {
    #         data[,i] = as.factor(data[,i])
    #     }
    # }
    return(naive_bayes(Class ~ ., data=data, laplace=1))
}
classify.bayes.execute <- function(classifier, datum) {
    #' Classify using naive bayes
    return(predict(classifier, datum))
}


# Combine classify.X.train and classify.X.execute into classify.X
# classify.X(training_data) returns a function that can be used to classify.
classifiers = c("majority", "random", "bayes", "lda")
for (classifier in classifiers) {
	train = match.fun(paste("classify.", classifier, ".train", sep=""))
	execute = match.fun(paste("classify.", classifier, ".execute", sep=""))

	name = paste("classify.", classifier, sep="")

	fun = local(function(training_data) {
		classifier = train(training_data)
		return(function(testing_datum) {
			execute(classifier, testing_datum)
		})
	}, list2env(list(train=train, execute=execute)))
	assign(name, fun)
}

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

classify.bayes.train <- function(data, discretize=FALSE) {
    #' Train a naive bayes classifier
    
    # Discretize real-valued attributes.
    if (discretize) {
        for (col in 1:ncol(data)) {
            if (typeof(data[,col]) == "double") {
                data[,col] = discretize(data[,col], breaks=5)
            }
        }
    }

    probs <- list()
    for (col in 1:(ncol(data) - 1)) {
        if (typeof(data[, col]) == "double") {
            # The column hasn't been discretized; approximate with a gaussian.
            probs[[names(data)[col]]] <- list(
                mean = mean(data[, col], na.rm = TRUE),
                sd = sd(data[, col], na.rm = TRUE)
            )
        } else {
            # The column has been discretized, use regular bayes.
            ret <- list()
            for (class in names(table(data$Class))) {
                ugodne <- data[which(data$Class == class), col]
                ret[[class]] <- table(ugodne) / sum(data$Class == class)
            }
            probs[[names(data)[col]]] <- ret
        }
    }
    probs[["Class"]] <- table(data$Class) / nrow(data)
    return(probs)
}
classify.bayes.execute <- function(classifier, datum) {
    #' Classify using naive bayes
	classes = names(classifier$Class)

	probs = list()
    for (class in classes) {
        # Calculate the probability of the datum belonging to the class.
        prob = classifier$Class[[class]]
        for (col in colnames(datum)) {
            if (all(c("mean", "sd") %in% names(classifier[[col]]))) {
                # Real valued attribute; use gaussian.
				multiplier = dnorm(datum[[col]], classifier[[col]]$mean, classifier[[col]]$sd)
            } else {
                # Discrete attribute.
				multiplier = classifier[[col]][[class]][[as.character(datum[[col]])]]
            }
			if (length(multiplier) == 1 && !is.nan(multiplier))
				prob = prob * multiplier
        }
		probs[[class]] <- prob
    }
	# Return the class with the highest probability.
	return(names(which.max(probs)))
}


# Combine classify.X.train and classify.X.execute into classify.X
# classify.X(training_data) returns a function that can be used to classify.
classifiers = c("majority", "random", "bayes")
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

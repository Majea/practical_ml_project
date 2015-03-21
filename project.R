project <- function(){
    library(caret)
    library(ggplot2)
    set.seed(159753)
    
    # LOADING, CLEANING AND FEATURE SELECTION
    
    # need to load the training data set first. The training data set is in fact both for training and testing
    # there are a lot of blank data, 'NA' and '#DIV/0!' fields that we need to consider as NA values
    data <- read.csv("pml-training.csv", na.strings=c('', 'NA', '#DIV/0!', ' '), stringsAsFactors=FALSE)
    # the outcome must stay a factor for random forest
    data[,'classe'] <- as.factor(data[,'classe'])
    # remove summary & stat rows
    data <- data[data$new_window=='no',]
    # remove index (X), user_name, time stamps and window columns
    data <- data[, setdiff(names(data), c('X', 'user_name', 'raw_timestamp_part_1', 'raw_timestamp_part_2', 'cvtd_timestamp', 'new_window', 'num_window'))]
    #remove the 8 statistics columns
    toRemove <- grepl("^kurtosis\\_", names(data)) |
        grepl("^skewness\\_", names(data)) |
        grepl("^max\\_", names(data)) |
        grepl("^min\\_", names(data)) |
        grepl("^amplitude\\_", names(data)) |
        grepl("^var\\_", names(data)) |
        grepl("^avg\\_", names(data)) |
        grepl("^stddev\\_", names(data))
    data <- data[, !toRemove]
    
    # use nearZeroVar (week2, lesson 6) + (week 2, lesson 7) to remove those with little variability => bad predictors
    # toRemove <- nearZeroVar(training, saveMetrics=TRUE)  # none are near zero
    
    # DATA SPLITTING
    inTrain <- createDataPartition(data$classe, p=0.10, list=FALSE)
    training <- data[inTrain,]
    testing <- data[-inTrain,]
    
    # TRAINING WITH K-FOLD RANDOM FOREST
    fitControl <- trainControl(method="repeatedcv", # k_fold cross validation
                               number=10, # 10 fold
                               repeats=10 # repeated 10 times (shuffle between repetitions)
        )
    fit <- train(classe ~ ., data=training, method="rf", trControl=fitControl, metric="Accuracy")
    ggplot(fit)
    
    # TESTING & ERROR
    test <- predict(fit, testing)
    confMat <- confusionMatrix(test, testing$classe)
    # accuracy is printed with the confusion matrix
    print(confMat)
    fit
}

validate <- function( fit ) {
    validationData <- read.csv("pml-testing.csv", na.strings=c('', 'NA', '#DIV/0!', ' '), stringsAsFactors=FALSE)
    result <- predict(fit, validationData)
    pml_write_files(result)
    result
}

pml_write_files = function(x){
    n = length(x)
    for(i in 1:n){
        filename = paste0("problem_id_",i,".txt")
        write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
    }
}

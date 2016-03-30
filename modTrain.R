# Install and load any needed packages
pkgs = c("shiny", "TTR", "quantmod", "wikipediatrend"
    #"caret", "fscaret", "neuralnet", "kernlab", "gmodels", "C50", "nnet"
)
for (p in pkgs) {
    if (! require(p, character.only = TRUE)) {
        install.packages(p)
        require(p, character.only = TRUE)
    }
}

modTrain = function(fulldata) {
    fulldata = fulldata[,2:ncol(fulldata)]
    numRows = dim(fulldata)[1]
    lastday = fulldata[numRows, ] 
    fulldata = fulldata[1:numRows-1, ]
    
    
    
    # Create the training and testing data sets
    set.seed(123)
    splitIndex <- createDataPartition(fulldata$Target, p = .9, list = FALSE, times = 1)
    trainDF <- fulldata[splitIndex,]
    testDF <- fulldata[-splitIndex,]  
    
    # Create the model
    svm.model <- ksvm(Target ~., data = trainDF, kernel = "polydot", C=9)
    
    #Evaluating model performance
    svm.predict <- predict(svm.model,testDF)
    #table(svm.predict,testDF$Target)
    
    ##########improve model preformance (kernel selection)############
    # Train the model using different kernel, radial basis, linear, polynomial, hyperbalic tangertsigmoid
    
    svm.model.rbf <- ksvm(Target ~., data = trainDF, kernel = "rbfdot",C=9)
    svm.model.linear <- ksvm(Target ~., data = trainDF, kernel = "vanilladot",C=9)
    svm.model.poly <- ksvm(Target ~., data = trainDF, kernel = "polydot",C=9)
    svm.model.tanh <- ksvm(Target ~., data = trainDF, kernel = "tanhdot",C=9)
    
    #Get the confusion matrix
    svm.confusion.rbf <- predict(svm.model.rbf,trainDF)
    svm.confusion.linear <-  predict(svm.model.linear,trainDF)
    svm.confusion.poly <-  predict(svm.model.poly,trainDF)
    svm.confusion.tanh <-  predict(svm.model.tanh,trainDF)
    #table(svm.confusion.rbf,trainDF$Target)
    #table(svm.confusion.linear,trainDF$Target)
    #table(svm.confusion.poly,trainDF$Target)
    #table(svm.confusion.tanh,trainDF$Target)
    
    # get the predicted values for each model
    
    svm.predict.rbf <- predict(svm.model.rbf,testDF)
    svm.predict.linear <-  predict(svm.model.linear,testDF)
    svm.predict.poly <-  predict(svm.model.poly,testDF)
    svm.predict.tanh <-  predict(svm.model.tanh,testDF)
    
    # check the result
    #print(svm.predict.rbf)
    #print(svm.predict.linear)
    #print(svm.predict.poly)
    #print(svm.predict.tanh)
    
    #Compare the accuracy for each model
    agreement.rbf <- svm.predict.rbf == testDF$Target
    agreement.linear <- svm.predict.linear == testDF$Target
    agreement.poly <- svm.predict.poly == testDF$Target
    agreement.tanh <- svm.predict.tanh == testDF$Target
    
    #table(agreement.rbf)
    #prop.table(table(agreement.rbf))
    #table(agreement.linear)
    #prop.table(table(agreement.linear))
    #table(agreement.poly)
    #prop.table(table(agreement.poly))
    #table(agreement.tanh)
    #prop.table(table(agreement.tanh))
    
    accu.rbf = sum(agreement.rbf) / length(agreement.rbf)
    accu.linear = sum(agreement.linear) / length(agreement.linear)
    accu.poly = sum(agreement.poly) / length(agreement.poly)
    accu.tanh = sum(agreement.tanh) / length(agreement.tanh)
    accus = c(accu.rbf, accu.linear, accu.poly, accu.tanh)
    kernels = c(svm.model.rbf, svm.model.linear, svm.model.poly, svm.model.tanh)
    for (i in 1:length(accus)) {
        if (accus[i] == max(accu.rbf, accu.linear, accu.poly, accu.tanh))
            bestMod = kernels[i][[1]]
    }
    #print(bestModel)
    
    #Evaluating model performance
    svm.predict <- predict(bestModel,testDF)
    table(svm.predict,testDF$Target)    
    return(bestModel)
}
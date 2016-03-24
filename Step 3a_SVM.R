#-----------------------------------Support Vector Machine-------------------------------
# load the dataset
fulldata <- fulldata[,2:70]
useddata <- subset(fulldata, select = -c(Target1, Target3, Target4,Target5))

# Create the training and testing data sets
splitIndex <- createDataPartition(useddata$Target2, p = .9, list = FALSE, times = 1)
trainDF <- useddata[splitIndex,]
testDF <- useddata[-splitIndex,]

imp_vars <- c("close", "open", "high","low","P.E.ratio", "Wiki_5_day_Disparity","Wiki_Move","Wiki_MA3_Move", "Wiki_EMA5_Move", "Wiki_5_day_Disparity","Google_EMA5_Move", "Google_3dayDisparity_Move","Google_ROC_Move","Google_RSI_Move","Wiki_3_day_Disparity" ,"Stochastic.Oscillator" ,"RSI_Move", "Wiki_RSI_Move","Google_MA_6" ,"Google_Move","Target2")
imp_data_train <- trainDF[imp_vars]
imp_data_test <- testDF[imp_vars]

# Create the model
svm.model <- ksvm(Target2 ~., data = imp_data_train, kernel = "polydot", C=9)
print(svm.model)

#Evaluating model performance
svm.predict <- predict(svm.model,imp_data_test)
table(svm.predict,imp_data_test$Target2)

##########improve model preformance (kernel selection)############
# Train the model using different kernel, radial basis, linear, polynomial, hyperbalic tangertsigmoid

svm.model.rbf <- ksvm(Target2 ~., data = imp_data_train, kernel = "rbfdot",C=9)
svm.model.linear <- ksvm(Target2 ~., data = imp_data_train, kernel = "vanilladot",C=9)
svm.model.poly <- ksvm(Target2 ~., data = imp_data_train, kernel = "polydot",C=9)
svm.model.tanh <- ksvm(Target2 ~., data = imp_data_train, kernel = "tanhdot",C=9)

#Get the confusion matrix
svm.confusion.rbf <- predict(svm.model.rbf,imp_data_train)
svm.confusion.linear <-  predict(svm.model.linear,imp_data_train)
svm.confusion.poly <-  predict(svm.model.poly,imp_data_train)
svm.confusion.tanh <-  predict(svm.model.tanh,imp_data_train)
table(svm.confusion.rbf,imp_data_train$Target2)
table(svm.confusion.linear,imp_data_train$Target2)
table(svm.confusion.poly,imp_data_train$Target2)
table(svm.confusion.tanh,imp_data_train$Target2)

# get the predicted values for each model

svm.predict.rbf <- predict(svm.model.rbf,imp_data_test)
svm.predict.linear <-  predict(svm.model.linear,imp_data_test)
svm.predict.poly <-  predict(svm.model.poly,imp_data_test)
svm.predict.tanh <-  predict(svm.model.tanh,imp_data_test)

# check the result
print(svm.predict.rbf)
print(svm.predict.linear)
print(svm.predict.poly)
print(svm.predict.tanh)

#Compare the accuracy for each model
agreement.rbf <- svm.predict.rbf == imp_data_test$Target2
agreement.linear <- svm.predict.linear == imp_data_test$Target2
agreement.poly <- svm.predict.poly == imp_data_test$Target2
agreement.tanh <- svm.predict.tanh == imp_data_test$Target2

table(agreement.rbf)
prop.table(table(agreement.rbf))
table(agreement.linear)
prop.table(table(agreement.linear))
table(agreement.poly)
prop.table(table(agreement.poly))
table(agreement.tanh)
prop.table(table(agreement.tanh))
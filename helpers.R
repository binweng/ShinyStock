Input_vars = c("Open_Price","Close_Price","High_Price","Low_Price",
               "PE_Ratio","Wiki_5day_disparity","Wiki_Move",
               "Wiki_MA3_Move","Wiki_EMA5_Move","Wiki_5day_disparity_Move",
               "Google_EMA5_Move","Google_3day_disparity_Move","Google_ROC_Move",
               "Google_RSI_Move","Wiki_3day_disparity","Stoochastic_Oscillator",
               "RSI_Move","Wiki_RSI_Move","Google_MA6","Google_Move")
#__________________________Pull the market data_____________________________
# Get the market data
getPred = function(yahooData, wikiData, term_count) {
    #date_begin_m <- strptime(as.character(date_begin), "%m/%d/%Y")
    #date_end_m <- strptime(as.character(date_end), "%m/%d/%Y")
    
    # Define the variables that we are going to use
    #__________________________Pull the market data_____________________________
    
    # Get the market data
    
    # Create the date list
    #yahooData = getSymbols(input$symb, src = "yahoo", from = input$dates[1], to = input$dates[2], auto.assign = FALSE)
    date_market <- data.frame(index(yahooData))   #Change to data frame
    
    # Get Open_Price, Close_Price, High_Price, Low_Price
    
    data_from_yahoo <- as.data.frame(yahooData) # 
    
    adjust_coff <- data_from_yahoo[,4]/data_from_yahoo[,6]  # Get the adjust index
    Open_Price <- data_from_yahoo[,1]/adjust_coff   # Get the adjusted value based on index, similar below
    Close_Price <- data_from_yahoo[,6]
    High_Price <- data_from_yahoo[,2]/adjust_coff
    Low_Price <- data_from_yahoo[,3]/adjust_coff
    
    #______________________Calulate the technical indicators_____________________________
    
    # Get the Stochastic Oscillator
    
    data_for_sto <- data.frame(High_Price,Low_Price,Close_Price,row.names = NULL)
    colnames(data_for_sto) <- c("High","Low","Close")    #Meet the format of fuction
    full_sto <- data.frame(stoch(data_for_sto))
    Stochastic_Oscillator <- full_sto$fastK * 100
    
    # Get the RSI_Move
    
    the_RSI <- RSI(Close_Price)     #Get the RSI
    RSI_Move <- diff(the_RSI)       #Get the difference as previous day
    RSI_Move[RSI_Move < 0] <- 0       # 0 means going down
    RSI_Move[RSI_Move > 0] <- 1       # 1 means going up
    RSI_Move <- data.frame(RSI_Move)  # Transfer to data frame
    RSI_Move <- rbind("N/A",RSI_Move) # Move down for one row
    
    #_______________________Temporary full data_________________________________
    
    fulldata_temp <- data.frame(date_market,Open_Price,Close_Price,
                                High_Price,Low_Price, Stochastic_Oscillator,
                                RSI_Move,row.names = NULL)
    colnames(fulldata_temp) <- c("Date","Open","Close","High","Low","Stochastic Oscillator", "RSI Move")
    
    # ______________________________Create the target_____________________________
    
    # Based on Target 2: O(i+1) - O(i)
    
    Target <- diff(Open_Price)
    temp = rep(NA, length(Target) + 1)
    temp[1:length(Target)] = Target
    Target = data.frame(temp)
    
    colnames(Target) <- "Target"
    
    Target[Target<0] <- 0       # 0 means going down
    Target[Target>0] <- 1       # 1 means going up
    Target <- data.frame(Target)  # Transfer to data frame
    
    fulldata_temp <- cbind(fulldata_temp,Target)
    
    #________________________Pull the Wikipedia data___________________________
    
    # Get Wikipeida data and stock ticker
    
    # term_count <- length(terms) +1
    Wiki_traffic <- colSums(matrix(wikiData$count, nrow = term_count)) # Take the sum by each day for all search terms
    Wiki_traffic_date <- colSums(matrix(wikiData$date, nrow = term_count)/term_count) # Collect related date. To check the 
    # date, use as.Date()
    Wiki_traffic_with_date <- cbind.data.frame(Wiki_traffic_date,Wiki_traffic)  # Combine the data
    
    date_market_compare <-  data.matrix(date_market) # Tranfer from list to double for comparsion
    
    #Compare the seq date with market open date
    date_diff_wiki <- setdiff(Wiki_traffic_with_date$Wiki_traffic_date,date_market_compare) 
    
    #Only keep the dates when market opens
    Wiki_traffic_with_date_new <- Wiki_traffic_with_date[!Wiki_traffic_with_date$Wiki_traffic_date %in% date_diff_wiki,]
    
    #This is the limitation, pull wiki traffic data might miss some data point
    miss_wiki <- setdiff(date_market_compare,Wiki_traffic_with_date_new$Wiki_traffic_date)
    
    
    # Calculate the Wiki_Move
    
    Wiki_traffic_market <- data.frame(Wiki_traffic_with_date_new$Wiki_traffic)
    
    Wiki_Move <- diff(Wiki_traffic_with_date_new$Wiki_traffic)  #Get the difference as previous day
    Wiki_Move[Wiki_Move<0] <- 0       # 0 means going down
    Wiki_Move[Wiki_Move>0] <- 1       # 1 means going up
    Wiki_Move <- data.frame(Wiki_Move) # Transfer to data frame
    Wiki_Move <- rbind("N/A",Wiki_Move) # Move down for one row
    
    # Calulate Wiki_MA3_Move, Wiki_EMA5_Move, Wiki_RSI_Move
    
    Wiki_MA_3 <- SMA(Wiki_traffic_market, 3)  # 3 day Moving average of Wiki Traffic
    Wiki_EMA_5 <- EMA(Wiki_traffic_market, 5) # 5 day Exponential moving average of Wiki Traffic
    Wiki_RSI <- RSI(Wiki_traffic_market, n = as.integer(length(Wiki_traffic_market$Wiki_traffic_with_date_new.Wiki_traffic)/10) + 1 ) # RSI of wiki traffic
    
    Wiki_MA3_Move <-diff(Wiki_MA_3)
    Wiki_MA3_Move[Wiki_MA3_Move<0] <- 0       # 0 means going down
    Wiki_MA3_Move[Wiki_MA3_Move>0] <- 1       # 1 means going up
    Wiki_MA3_Move <- data.frame(Wiki_MA3_Move)
    Wiki_MA3_Move <- rbind("N/A",Wiki_MA3_Move) # Move down for one row
    
    Wiki_EMA5_Move <- diff(Wiki_EMA_5)
    Wiki_EMA5_Move[Wiki_EMA5_Move<0] <- 0       # 0 means going down
    Wiki_EMA5_Move[Wiki_EMA5_Move>0] <- 1       # 1 means going up
    Wiki_EMA5_Move <- data.frame(Wiki_EMA5_Move)
    Wiki_EMA5_Move <- rbind("N/A",Wiki_EMA5_Move) # Move down for one row
    
    Wiki_RSI_Move <- diff(Wiki_RSI)
    Wiki_RSI_Move[Wiki_RSI_Move<0] <- 0       # 0 means going down
    Wiki_RSI_Move[Wiki_RSI_Move>0] <- 1       # 1 means going up
    Wiki_RSI_Move <- data.frame(Wiki_RSI_Move)
    Wiki_RSI_Move <- rbind("N/A",Wiki_RSI_Move) # Move down for one row
    
    # Calculate Wiki_5day_Disparity, Wiki_5day_Disparity_Move, Wiki_3day_Disparity
    
    Wiki_MA_5 <- SMA(Wiki_traffic_market,5)  # 5 day moving average of Wiki Traffic
    Wiki_3day_Disparity <- Wiki_traffic_market/Wiki_MA_3  #Please refer to Appendix II formula 3 
    Wiki_5day_Disparity <- Wiki_traffic_market/Wiki_MA_5
    
    Wiki_3day_Disparity <- as.numeric(unlist(Wiki_3day_Disparity))
    Wiki_5day_Disparity <- as.numeric(unlist(Wiki_5day_Disparity))
    
    Wiki_5day_Disparity_Move <- diff(Wiki_5day_Disparity)
    Wiki_5day_Disparity_Move[Wiki_5day_Disparity_Move <0] <- 0       # 0 means going down
    Wiki_5day_Disparity_Move[Wiki_5day_Disparity_Move >0] <- 1       # 1 means going up
    Wiki_5day_Disparity_Move <- data.frame(Wiki_5day_Disparity_Move)
    Wiki_5day_Disparity_Move <- rbind("N/A",Wiki_5day_Disparity_Move) # Move down for one row
    
    #_______________________Temporary full data after Wiki_________________________________
    
    # Dealing with the missing data point after wiki
    fulldata_temp <- fulldata_temp[!fulldata_temp$Date %in% miss_wiki,]
    Wiki_data <- data.frame(Wiki_Move,Wiki_MA3_Move,Wiki_EMA5_Move, Wiki_RSI_Move,Wiki_5day_Disparity, 
                            Wiki_5day_Disparity_Move, Wiki_3day_Disparity)
    fulldata_temp <- cbind(fulldata_temp,Wiki_data)
    
    
    #_________________Finalize the data_______________________
    
    
    fulldata <- fulldata_temp[20:nrow(fulldata_temp),]

    move_cols <- sapply(fulldata, is.character)
    move_cols[["RSI Move"]] <- TRUE
    move_cols[["Target"]] <- TRUE
    move_data <- as.data.frame(sapply(fulldata[,move_cols], as.factor))
    move_col_names <- names(move_data)
    non_move_data <- fulldata[, -which(names(fulldata) %in% move_col_names)]
    fulldata <- cbind(non_move_data,move_data)
    
    Target <- fulldata$Target
    fulldata$Target <- NULL
    fulldata <- cbind(fulldata,Target)

    
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
            bestmod = kernels[i][[1]]
    }
    lastdayPred <- predict(bestmod, lastday)
    if (lastdayPred == 0)
        return("Going down")
    else
        return("Going up")
}



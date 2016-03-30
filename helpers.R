yahooAdjust = function(yahooData) {
#     symb <- "AAPL"
#     date_begin <- strptime(as.character("1/1/2016"), "%m/%d/%Y")
#     date_end <- strptime(as.character("3/1/2016"), "%m/%d/%Y")    
#     yahoo_data = getSymbols(symb, src = "yahoo", from = date_begin, to = date_end, auto.assign = FALSE)
#     getSymbols("AAPL", from=date_begin, to = date_end, src="yahoo")
#     data.frame(index(AAPL))  
    date_market <- data.frame(index(yahooData))  
    data_from_yahoo <- as.data.frame(yahooData) 
    
    # Get Open_Price, Close_Price, High_Price, Low_Price
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
    RSI_Move[RSI_Move<0] <- 0       # 0 means going down
    RSI_Move[RSI_Move>0] <- 1       # 1 means going up
    RSI_Move <- data.frame(RSI_Move)  # Transfer to data frame
    RSI_Move <- rbind("N/A",RSI_Move) # Move down for one row  
    
    #_______________________Temporary full data_________________________________
    
    fulldata_temp <- data.frame(date_market,Open_Price,Close_Price,
                                High_Price,Low_Price, Stochastic_Oscillator,
                                RSI_Move,row.names = NULL)
    colnames(fulldata_temp) <- c("Date","Open","Close","High","Low","Stochastic Oscillator",
                                 "RSI Move")
    
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
    return(fulldata_temp)
}

wikidataMerge = function(fulldata_temp, wikiData, term_count) {
    # Get Wikipeida data and stock ticker
        symb <- "AAPL"
        date_begin <- strptime(as.character("9/1/2015"), "%m/%d/%Y")
        date_end <- strptime(as.character("3/1/2016"), "%m/%d/%Y")    
        terms = c("iPhone", "iPad")
        wikiData = wp_trend(page = c(symb, terms), 
                            from=date_begin, to = date_end)
    term_count <- length(terms) +1
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
    #length(Wiki_traffic_market$Wiki_traffic_with_date_new.Wiki_traffic)
    
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
    # Dealing with the missing data point after wiki
    fulldata_temp <- fulldata_temp[!fulldata_temp$Date %in% miss_wiki,]
    Wiki_data <- data.frame(Wiki_Move,Wiki_MA3_Move,Wiki_EMA5_Move, Wiki_RSI_Move,Wiki_5day_Disparity, 
                            Wiki_5day_Disparity_Move, Wiki_3day_Disparity)
    fulldata_temp <- cbind(fulldata_temp,Wiki_data)
    #_________________Finalize the data_______________________
    fulldata <- fulldata_temp[20:nrow(fulldata_temp),]
    str(fulldata)  # report summaries of each of the data attributes
    head(fulldata) # returns the first few rows of the data so we can see how it looks like
    names(fulldata) # returns the names of each variable
    
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
    return(fulldata)    
}

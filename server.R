# Install and load any needed packages
pkgs = c("shiny", "TTR", "quantmod", "wikipediatrend",
         "caret", "fscaret", "neuralnet", "kernlab", "gmodels", "C50", "nnet")
for (p in pkgs) {
    if (! require(p, character.only = TRUE)) {
        install.packages(p)
        require(p, character.only = TRUE)
    }
}

#source(c("getFullData.R", "modTrain.R"))

shinyServer(function(input, output, session) {
    yahooData = eventReactive(input$go, {
        getSymbols(input$symb, src = "yahoo", from = input$dates[1], to = input$dates[2], auto.assign = FALSE)
    })
    wikiData = eventReactive(input$go, {
        wp_trend(page = c(input$symb, strsplit(gsub(" ", "", input$related), split = ",")[[1]]), 
                 from = input$dates[1], to = input$dates[2])
    })
    term_count = eventReactive(input$go, {length(input$related) +1 })
    
#    stock = input$stockName
#     terms = strsplit(gsub(" ", "", input$related), split = ",")[[1]]
#     date_begin = input$daterange[1]
#     date_end = input$daterange[2]
    #fulldata = getFullData(stock, terms, date_begin, date_end)
    #bestModel = modTrain(fulldata)
#     lastdayPred <- predict(bestmod, lastday)
#     
#     
#     
#     if (lastdayPred == 0)
#         output$text = renderText("Going down")
#     else
#         output$text = renderText("Going up")
#     
    
    output$fullSummary = renderPrint({
#         source("helpers.R")
#         fulldata_yahoo = yahooAdjust(yahooData())
#         fulldata = wikidataMerge(fulldata_yahoo, wikiData(), term_count())
        source("getFullData.R")
        lastdayPred = getPred(yahooData(), wikiData(), term_count())
        print(lastdayPred)
        #output$text = renderText
    })
    
    output$wikiSummary = renderPrint({
        summary(wikiData())
    })

    output$wikiPlot = renderPlot({
        ggplot(wikiData(), aes(date, count, group=page, color = page)) + geom_point() + 
            geom_smooth(method="lm", formula = y ~ poly(x, as.integer(length(wikiData())/10 + 1)), size=1.5) + theme_bw()
    })
    
    output$yahooPlot = renderPlot({
        chartSeries(yahooData(), type = "line")
    })
    
})



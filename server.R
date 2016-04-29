# Install and load any needed packages
pkgs = c("shiny", "TTR", "quantmod", "wikipediatrend",
         "caret", "fscaret", "neuralnet", "kernlab", "gmodels", "C50", "nnet")
for (p in pkgs) {
    if (! require(p, character.only = TRUE)) {
        install.packages(p)
    }
    library(p, character.only = TRUE)
}

#source("getFullData.R")
source("modTrain.R")
source("helpers.R")

shinyServer(function(input, output, session) {
    yahooData = eventReactive(input$go, {
        getSymbols(input$symb, src = "yahoo", from = input$date - 365, to = input$date, auto.assign = FALSE)
    })
    wikiData = eventReactive(input$go, {
        wp_trend(page = c(input$symb, strsplit(gsub(" ", "", input$related), split = ",")[[1]]), 
                 from = input$date - 365, to = input$date)
    })
    term_count = eventReactive(input$go, {length(input$related) +1 }) 
    
    output$tempPlotYahoo = renderPlot({
        chartSeries(yahooData(), type = "line")
    })
    output$yahooPlot = renderPlot({
        chartSeries(yahooData(), type = "line")
    })
    output$yahooSummary = renderPrint({
        head(yahooData())
    })
    output$wikiPlot = renderPlot({
        ggplot(wikiData(), aes(date, count, group=page, color = page)) + geom_point() + 
            geom_smooth(method="lm", formula = y ~ poly(x, as.integer(length(wikiData())/100) + 2), size=1.5) + theme_bw()
    })
    output$wikiSummary = renderPrint({
        summary(wikiData())
    })
    
    output$modSummary = renderPrint({
        bestMod = getBestModel(yahooData(), wikiData(), term_count())
        print(bestMod)
    })
    
    output$text = renderText({
        paste("We predict the price of ", input$symb, " on the next trading day would be\n", sep = "")
    })
    output$result = renderPrint({
        getPred(yahooData(), wikiData(), term_count())
    })
#     
#     output$paper1pdf <- renderUI({
#         PDFfile="./paper1.pdf"
#         tags$iframe(
#             src=PDFfile,
#             width="100%"),
#             height="800px")
#     })
#     
})
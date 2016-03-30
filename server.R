shinyServer(function(input, output, session) {
    output$predRes <- renderPlot({
        #plot(cars, type=input$plotType)
    })
    
    output$summary <- renderPrint({
        summary(rnorm(100))
    })
    
})
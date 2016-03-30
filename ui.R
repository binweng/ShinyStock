setwd('D:\\@Auburn\\2016Spring\\INSY7970_DataVisualization\\projects\\ShinyStock')
library(markdown)

shinyUI(navbarPage("Navbar or whatever!",
            tabPanel("Stock Prediction",
                sidebarLayout(
                    sidebarPanel(
                        textInput(inputId = "stockName", label = "Please enter the name of stock:"),
                        submitButton(text = "Submit")
                    ),
                    mainPanel(
                        h3(textOutput ("text")),
                            plotOutput("plot")
                        )
                    )
            ),
            tabPanel("Historical Trend",
                verbatimTextOutput("summary")         
            ),
        navbarMenu("More",
            tabPanel("more1",
                includeMarkdown("haha.md")
            ),
            tabPanel("more2",
                includeMarkdown("haha2.md")
            )
        )
    )
)
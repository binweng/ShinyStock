setwd('D:\\@Auburn\\2016Spring\\INSY7970_DataVisualization\\projects\\ShinyStock')
library(markdown)

shinyUI(navbarPage("Stock Market Trend Prediction and Analysis from Internet Data",
            tabPanel("Stock Prediction",
                sidebarLayout(
                    sidebarPanel(
                        helpText("Select a stock to examine. Information will be collected from yahoo finance, wikitrend, google, and so forth."),
                        textInput(inputId = "symb", label = "Please enter the name of stock:", value = "AAPL"),
                        textInput(inputId = "related", label = "Please enter related terms:", value = "iPhone, iPad, Macbook"),
                        dateRangeInput(inputId = "dates", label = "Date Range: ", 
                                       start = "2016-01-01", end = as.character(Sys.Date())
                        ),
                        actionButton(inputId = "go", label = "Update")
                    ),
                    mainPanel(
                        h3(
                            textOutput ("text")),
                            plotOutput("wikiPlot"),
                            plotOutput("yahooPlot")
                        )
                    )
            ),
            tabPanel("Historical Trend",
                verbatimTextOutput("fullSummary"),       
                verbatimTextOutput("wikiSummary")         
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
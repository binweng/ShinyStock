#setwd('D:\\@Auburn\\2016Spring\\INSY7970_DataVisualization\\projects\\ShinyStock')
library(markdown)

shinyUI(navbarPage("Stock Market Trend Prediction and Analysis Tool",
            tabPanel("Introduction",
                    includeMarkdown("abstract.md")
            ),       
            tabPanel("Stock Prediction",
                sidebarLayout(
                    sidebarPanel(
                        helpText("Select a stock to examine. Information will be collected 
                                 from yahoo finance, wikitrend, google, and so forth."),
                        textInput(inputId = "symb", label = "Please enter the name of stock:", value = "AAPL"),
                        textInput(inputId = "related", label = "Please enter related terms:", value = "iPhone, iPad, Macbook"),
                        dateInput(inputId = "date", label = "Please choose the date: ", value = Sys.Date()),
                        actionButton(inputId = "go", label = "Update")
                    ),
                    mainPanel(
                        h3(textOutput("text")),
                        h1(textOutput("result")),
                        h3(helpText("\nHistorical Data from Yahoo Finance")),
                        plotOutput("tempPlotYahoo"),
                        h4(helpText("We have trained 4 different kernels of Supported Vector Machine, 
                                    based on the result of our paper. 
                                    And the best model we chose for this prediction is as follows:")),
                        verbatimTextOutput("modSummary")
                    )
                )
            ),
            tabPanel("Yahoo Finance Trend",
                plotOutput("yahooPlot"),
                verbatimTextOutput("yahooSummary")         
            ),
            tabPanel("WikiTrend Data",
                verbatimTextOutput("fullSummary"),       
                plotOutput("wikiPlot"),
                verbatimTextOutput("wikiSummary")         
            )
    )
)
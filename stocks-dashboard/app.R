library(shiny)
library(quantmod)
library(fpp2)
library(astsa)
library(tidyverse)
library(tidyquant)
library(PortfolioAnalytics)
library(fBasics)
library(shinythemes)
# Define UI for random distribution app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Stock Market Prediction"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Select the random distribution type ----
      textInput(inputId = "dataset",
                  label = "Choose a Stock Symbol",
                  value="^GSPC"),
      
      # br() element to introduce extra vertical spacing ----
      br(),
      
      # Input: Slider for the number of observations to generate ----
      numericInput(inputId = "obs",
                   label = "Number of observations to view:",
                   value = 5),
      br(),
      
      dateRangeInput(inputId = "date",label ="Enter the range of the date",start=as.Date("2010-01-01"),end=Sys.Date()),
      
      br(),
      numericInput(inputId = "days",
                   label = "Number of Days to predict:",
                   value = 6)
      
   
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Tabset w/ plot, summary, and table ----
      tabsetPanel(type = "tabs",
                  tabPanel("Summary", verbatimTextOutput("summary"),br(),tableOutput("table")),
                  tabPanel("Plot", plotOutput("plot"),br(),plotOutput("difflogplot"),br(),plotOutput("ACFplot"),br(),plotOutput("PACFplot")),
                  tabPanel("Prediction",verbatimTextOutput("str"),br()),
                  tabPanel("Forecast",plotOutput("forecastplot"),br(),tableOutput("forecast"))
                  
      )
      
    )
  )
)

# Define server logic for random distribution app ----
server <- function(input, output) {
  
  # Reactive expression to generate the requested distribution ----
  # This is called whenever the inputs change. The output functions
  # defined below then use the value computed from this expression
  Inputdata <- reactive({
    validate(
      need(input$dataset != "", "Please select a proper Stock Symbol")
    )
    validate(
      need(
        class(try(getSymbols(input$dataset,from=input$date[1], to=input$date[2])))!="try-error","The selected Stock Symbol might not belong to a stock or the given range might be wrong .please rectify your mistake"
      )
    )
    getSymbols(input$dataset,src="yahoo",auto.assign=F,from=input$date[1], to=input$date[2])
})
  
  # Generate a plot of the data ----
  # Also uses the inputs to build the plot label. Note that the
  # dependencies on the inputs and the data reactive expression are
  # both tracked, and all expressions are called in the sequence
  # implied by the dependency graph.
  output$plot <- renderPlot({
    inputdata=Inputdata()
    chartSeries(inputdata,type="candlesticks",theme="white")
    addBBands()
  })
  output$difflogplot <- renderPlot({
    inputdata=Inputdata()
    plot.xts((Return.calculate(Cl(inputdata))),main="Stock Returns Chart",xlab ="Stock Closing")
  })
  
  # Generate a summary of the data ----
  output$summary <- renderPrint({
    basicStats(Inputdata())
  })
  
  # Generate an HTML table view of the data ----
  output$table <- renderTable({
    tail(Inputdata(),n=input$obs)
  },rownames = T)
  
  output$str <- renderPrint({
    inputdata=Inputdata()
    summary(auto.arima(Cl(inputdata)))
  })
  output$ACFplot <- renderPlot({
    inputdata=Inputdata()
    ggAcf(Cl(inputdata),lag.max = 20, main="ACF plot for the stock")
  })
  
  output$PACFplot <- renderPlot({
    inputdata=Inputdata()
    ggPacf(Cl(inputdata),lag.max = 20, main="PACF plot for the stock")
  })
  output$forecastplot <- renderPlot({
    inputdata=Inputdata()
    autoplot(forecast(auto.arima(Cl(inputdata)),input$days),50)
  })
  
  output$forecast <- renderTable({
    inputdata=Inputdata()
    forecast(auto.arima(Cl(inputdata)),input$days)
  },rownames = T)
  
}

# Create Shiny app ----
shinyApp(ui, server)




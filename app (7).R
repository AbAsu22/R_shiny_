#########################################################
#  Economic forecasting and analysis
#  Shiny App - 
#  ##########################################################


# load libraries
library(shiny)
library(fpp2)
library(quantmod)
library(shinythemes)
library(urca)
options(rsconnect.max.bundle.size=9368709120)

# load data from FRED
symbols <- c( "MRTSSM44X72USS","MRTSSM7225USN","MRTSSM4453USS" )
m = length(symbols)

getSymbols(symbols,src="FRED")


# Define UI 
ui <- fluidPage(shinythemes::themeSelector(),pageWithSidebar(
    
    # Application title
    headerPanel(h1("Retail Sales Forecasting", align='center')),
    
    # Sidebar with controls to select the dataset and forecast ahead duration
    sidebarPanel(
        # Select variable
        selectInput("variable", "Variable:",
                    list("Food Services" = "MRTSSM44X72USS", 
                         "Restaurants" = "MRTSSM7225USN",
                         "Alcohol" = "MRTSSM4453USS")),
        sliderInput("ahead", "Periods to Forecast Ahead:",min=0, max=36, value= 12, step = 2),
        numericInput("start", "Starting year:", 2010),
        
        
        submitButton("Update View")
    ),
    
    
    
    # Show the caption and forecast plots
    mainPanel(
        h3(textOutput("caption")),
        
        tabsetPanel(
            tabPanel("Timeseries plot", plotOutput("tsPlot")),
            tabPanel("Timeseries Decomposition", plotOutput("dcompPlot")),
            tabPanel("ETS Forecast", plotOutput("etsForecastPlot"), verbatimTextOutput("etsForecastTable")), 
            tabPanel("Arima Forecast", plotOutput("arimaForecastPlot"), verbatimTextOutput("arimaForecastTable")),
            tabPanel("nnetar Forecast", plotOutput("nnetarForecastPlot"), verbatimTextOutput("nnetarForecastTable")),
            tabPanel("STL Forecast", plotOutput("STLForecastPlot"), verbatimTextOutput("STLForecastTable")),
            
            
            tabPanel("TBATS Forecast", plotOutput("tbatsForecastPlot"), verbatimTextOutput("tbatsForecastTable")),
            tabPanel("Average (All Five) forecasts", verbatimTextOutput("averageForecastTable")),
            tabPanel("Unit Root Test (Test for Stationarity)", verbatimTextOutput ("unitroot")),
            
            
        )
    )
    
))



server <- (function(input, output) {
    
    getDataset <- reactive({
        if (input$variable=="MRTSSM44X72USS")
        {
            return(MRTSSM44X72USS)
        }
        else if (input$variable=="MRTSSM7225USN")
        {
            return(MRTSSM7225USN)
        }
        else
        {
            return(MRTSSM4453USS)
        }
    })
    
    
    
    output$caption <- renderText({
        paste("Dataset: ", input$variable)
    })
    
    output$tsPlot <- renderPlot({
        
        y <- getDataset()
        date.start = input$start
        y   =  ts(y[paste(date.start,end(y),sep="/")], start=date.start, freq=12 )
        
        p1 = autoplot(y) + ylab("values") + geom_line(col="red", size=1.5)
        p2 = ggAcf(y)
        gridExtra::grid.arrange(p1, p2, nrow=2)
    })
    
    output$dcompPlot <- renderPlot({
        y <- getDataset()
        date.start = input$start
        y   =  ts(y[paste(date.start,end(y),sep="/")], start=date.start, freq=12 )
        
        f <- decompose(y)
        plot(f)
    })
    
    output$arimaForecastPlot <- renderPlot({
        y <- getDataset()
        date.start = input$start
        y   =  ts(y[paste(date.start,end(y),sep="/")], start=date.start, freq=12 )
        
        fit <- auto.arima(y)
        autoplot(forecast(fit, h=input$ahead)) + ylab("sales")
    })
    
    output$unitroot <- renderPrint({
      
      y <- getDataset()
      date.start = input$start
      y   <-  ts(y[paste(date.start,end(y),sep="/")], start=date.start, freq=12 )
      
      u <- ur.pp(y) 
      print(summary(u))
      
    })
    
    output$arimaForecastTable <- renderPrint({
        y <- getDataset()
        date.start = input$start
        y   =  ts(y[paste(date.start,end(y),sep="/")], start=date.start, freq=12 )
        
        fit <- auto.arima(y)
        forecast(fit, h=input$ahead)
    })
    
    output$etsForecastPlot <- renderPlot({
        y <- getDataset()
        date.start = input$start
        y   =  ts(y[paste(date.start,end(y),sep="/")], start=date.start, freq=12 )
        
        fit <- ets(y)
        autoplot(forecast(fit, h=input$ahead)) + ylab('sales')
    })
    
    output$etsForecastTable <- renderPrint({
        y <- getDataset()
        date.start = input$start
        y   =  ts(y[paste(date.start,end(y),sep="/")], start=date.start, freq=12 )
        
        fit <- ets(y)
        forecast(fit, h=input$ahead)
    })
    
    
    output$nnetarForecastPlot <- renderPlot({
        y <- getDataset()
        date.start = input$start
        y   =  ts(y[paste(date.start,end(y),sep="/")], start=date.start, freq=12 )
        
        fit <- nnetar(y, lambda = 0)
        autoplot(forecast(fit, h=input$ahead)) + ylab('sales')
    })
    
    output$nnetarForecastTable <- renderPrint({
        y <- getDataset()
        date.start = input$start
        y   =  ts(y[paste(date.start,end(y),sep="/")], start=date.start, freq=12 )
        
        fit <- nnetar(y, lambda = 0)
        forecast(fit, h=input$ahead)
        
    })
    
    output$STLForecastPlot<- renderPlot({
      y <- getDataset()
      date.start = input$start
      y   =  ts(y[paste(date.start,end(y),sep="/")], start=date.start, freq=12 )
      
      fit <- stlf(y)
      autoplot(forecast(fit, h=input$ahead)) + ylab('sales')
      
    })
    
    output$STLForecastTable <- renderPrint({
      y <- getDataset()
      date.start = input$start
      y   =  ts(y[paste(date.start,end(y),sep="/")], start=date.start, freq=12 )
      
      fit <- stlf(y)
      forecast(fit, h=input$ahead)
      
    })
    
    output$tbatsForecastPlot <- renderPlot({
      y <- getDataset()
      date.start = input$start
      y   =  ts(y[paste(date.start,end(y),sep="/")], start=date.start, freq=12 )
      fit <- tbats(y)
      autoplot(forecast(fit, h=input$ahead)) + ylab('sales')
      
    })
    
    output$tbatsForecastTable <- renderPrint({
      y <- getDataset()
      date.start = input$start
      y   =  ts(y[paste(date.start,end(y),sep="/")], start=date.start, freq=12 )
      
      fit <- tbats(y)
      forecast(fit, h=input$ahead)
      
    })
    
    
        output$averageForecastTable <- renderPrint({
        y <- getDataset()
        date.start = input$start
        y   =  ts(y[paste(date.start,end(y),sep="/")], start=date.start, freq=12 )
        
        fit1 <- ets(y)
        fc1 = forecast(fit1, h=input$ahead)
        
        fit2 = auto.arima(y)
        fc2 = forecast(fit2, h=input$ahead)
        
        fit3 <- tbats(y)
        fc3 <- forecast(fit3, h=input$ahead)
        
        fit4 <- nnetar(y,lamda=0)
        fc4 <- forecast(fit4, h=input$ahead)
        
        fit5 <- stlf(y)
        fc5 <- forecast(fit5, h=input$ahead)
        
       
    
        fc = (fc1$mean + fc2$mean + fc4$mean + fc4$mean + fc5$mean)/5
        fc
    })
    
})

        
# Run the application 
shinyApp(ui = ui, server = server)

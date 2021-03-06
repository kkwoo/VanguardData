#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(httr)
library(dygraphs)   # zoomable graphs
library(data.table) # like using data.table
library(shiny)
library(stringi)
library(jsonlite)
library(fasttime)   # for fastPOSIXct

vanguardData <- function (productIDStr, startDate, endDate) {
    urlStr <- paste0("https://api.vanguard.com/rs/gre/gra/1.7.0/datasets/auw-retail-price-history-mf.jsonp?",
                     "vars=portId:",productIDStr,
                     ",issueType:S,sDate:",startDate,
                     ",eDate:",endDate,
                     ",as_of:DAILY&callback=angular.callbacks._4")
    # "https://api.vanguard.com/rs/gre/gra/1.7.0/datasets/auw-retail-price-history-mf.jsonp?vars=portId:8145,issueType:S,sDate:2018-02-28,eDate:2019-09-28,as_of:DAILY"
    rawData <- GET(urlStr) 
    return (fromJSON(strsplit(content(rawData, "text"), "[()]")[[1]][[2]]))
}

vanguardProducts <- function () {
    urlStr <- paste0("https://api.vanguard.com/rs/gre/gra/1.7.0/datasets/auw-retail-listview-data.jsonp?callback=angular.callbacks._0")
    rawData <- GET(urlStr) 
    sr01 <- stri_replace_first_fixed(content(rawData, "text"), "angular.callbacks._0(", "")
    sr02 <- stri_replace_last_fixed(sr01, ")", "")
    return (fromJSON(sr02)$fundData)
}
x16 <- Filter(function(x) !is.null(x$name), vanguardProducts())
x17 <- unlist(Map(function(x) paste(x$portId, "-", x$name), x16))
x18 <- Map(function (x) x$portId, x16)
x19 <- setNames(x18, x17)
x19 <- x19[order(names(x19))]

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("Vanguard Investment Unit Buy Price"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            dateInput("date1", "Date:", value = as.Date("01/02/2014", format = "%d/%m/%Y")),
            dateInput("date2", "Date:", value = Sys.Date()),
            # selectInput("productCode", "Product Code", c("8145 - International Shares" = "8145",
            #                                             "8146 - Hedged International Shares" = "8146",
            #                                             "8129 - Australian Shares Fund" = "8129",
            #                                             "fixed" = "8149")) */
            selectInput("productCode", "Product Code", x19)
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            dygraphOutput("dygraph")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    ds01 <- reactive({
        # proc02 <- MLCH7Data(as.character(input$date1, format = "%d/%m/%Y"),
        #                     as.character(input$date2, format = "%d/%m/%Y"))[, .(as.POSIXct(V3, format = "%d %b %Y"), V4, as.double(stri_replace_all(V5, regex="\";", "")))]
        proc02 <- as.data.table(vanguardData(as.character(input$productCode), as.character(input$date1, format = "%Y-%m-%d"),
                                       as.character(input$date2, format = "%Y-%m-%d"))$fundPricesBuy)[, asOfDate.ft := fastPOSIXct(substring(asOfDate, 1, 10))][, .(asOfDate.ft, price)]
    })
    output$dygraph <- renderDygraph({
        dygraph(ds01(),
                main = "Buy price") %>%
            dyOptions(useDataTimezone = FALSE) %>%
            # dyBarChart() %>%
            dyRoller(rollPeriod = 1)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

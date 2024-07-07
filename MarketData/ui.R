####################################################################################
# This is the user-interface definition for the Market Data Shiny web application.#
####################################################################################

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
    # Sidebar with a slider input for number of bins
  tabsetPanel(
    tabPanel("About",
             h2("About This App"),
             p("The purpose of this app is to explore market data. It will provide the ability to create contingency tables, numerical summaries and plots regarding the data."),
             p("Data Source: Marketstack API. To access the site please visit: ",
               a("Marketstack", href = "https://marketstack.com/", target = "_blank"), "."),
             p("Tabs:"),
             tags$ul(
               tags$li("About: Information about the app."),
               tags$li("Data Download: Download market data from Marketstack."),
               tags$li("Data Exploration: Summaries and graphs market data.")
             ),
             tags$img(src = "https://images.g2crowd.com/uploads/product/image/social_landscape/social_landscape_8532463e95e5d1a9a7f09665c11807d5/marketstack-api.png", height = "100px")
    ),    
    tabPanel("Data Download",
             fluidRow(
               column(4,
                      wellPanel(
                        textInput("symbol", "Symbol", value = "AAPL"),
                        selectInput("type", "Type", choices = c("eod", "intraday")),
                        dateInput("date_from", "Date From", value = Sys.Date() - 30),
                        dateInput("date_to", "Date To", value = Sys.Date()),
                        actionButton("get_data", "Get Data")
                      )
               ),
               column(8,
                      dataTableOutput("data_table"),
                      downloadButton("download_data", "Download Data")
               )
             )
    ),
    tabPanel("Data Exploration",
             fluidRow(
               column(4,
                      wellPanel(
                        selectInput("x_var", "X Variable", 
                                    choices = c("open", "close", "high", "low")),
                        selectInput("y_var", "Y Variable", 
                                    choices = c("open", "close", "high", "low")),
                        selectInput("plot_type", "Plot Type", 
                                    choices = c("Line", "Scatter", "Box", "Density")),
                        checkboxInput("facet", "Facet by Date", value = FALSE),
                        actionButton("plot_data", "Plot Data")
                        )
                      )
               ),
               column(8,
                      tableOutput("summary_table"),
                      #tableOutput("contingency_table"),
                      plotOutput("market_plot")
               )
             )
    )
))

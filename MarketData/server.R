####################################################################################
# This is the server logic for the Market Data Shiny web application.#
####################################################################################

library(shiny)
library(httr)
library(jsonlite)
library(tidyverse)
library(DT)

# Create function for reading in API data
MarketAPI <- function(symbol, type, date_from=NULL, date_to=NULL) {
  #Create URL based on user provided choices
  if (type == "eod") {
    url <- paste0("http://api.marketstack.com/v1/eod", 
                  "?access_key=d4f48f4fa8307e7e0163fc92cce500d2", 
                  "&symbols=", symbol,
                  "&date_from=", date_from,
                  "&date_to=", date_to)
  } else if (type == "intraday") {
    url <- paste0("http://api.marketstack.com/v1/intraday", 
                  "?access_key=d4f48f4fa8307e7e0163fc92cce500d2", 
                  "&symbols=", symbol,
                  "&date_from=", date_from,
                  "&date_to=", date_to)
  } else {
    stop("Invalid data_type. Please specify 'eod', 'splits', or 'intraday.")
  }
  
  # Use get to pull data
  market_data <- httr::GET(url)
  
  # Convert raw data
  parsed <- jsonlite::fromJSON(rawToChar(market_data$content))
  # Convert to tibble
  market_info <- tibble::as_tibble(parsed$data)
  return(market_info)
}



# Define server logic 
function(input, output, session) {
  # Use above MarketAPI function to access data
  market_data <- reactive({
    req(input$get_data)
    MarketAPI(input$symbol, input$type, input$date_from, input$date_to)
  })  
  
  # Update the data table
  #observeEvent(input$get_data, 
  output$data_table <- renderDataTable({
    datatable(
      market_data(),
      extensions = c('Buttons', 'ColReorder'),
      options = list(
        dom = 'Bfrtip',
        buttons = c('colvis'),
        colReorder = TRUE,
        pageLength = 10,
        lengthMenu = c(10, 25, 50, 100)
      ),
      filter = 'top',
      selection = 'multiple'
    )  
    })
#)
  
  # Allow for data to be downloaded as .csv
  output$download_data <- downloadHandler(
    filename = function() {
      paste("market_data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(market_data(), file)
    }
  )
  
  # Generate plots based on user inputs
  #observeEvent(input$plot_data, 
  output$market_plot <- renderPlot({
      data <- market_data()
      
      if (input$plot_type == "Line") {
        p <- ggplot(data, aes(x = as.Date(date), y = !!sym(input$x_var))) +
          geom_line(color = "red") +
          labs(title = paste0(input$x_var, "Prices Over Time", sep = " "), x = "Date", 
               y = paste0(input$x_var, "Price", sep = " "))
      } else if (input$plot_type == "Scatter") {
        p <- ggplot(data, aes(x = !!sym(input$x_var), y = !!sym(input$y_var))) +
          geom_point(color = "blue") +
          labs(title = paste0(input$x_var, "vs", input$y_var, "Prices", sep = " "), 
               x = paste0(input$x_var, "Price", sep = " "), y = paste0(input$y_var, "Price", sep = " "))
      } else if (input$plot_type == "Box") {
        p <- ggplot(data, aes(y = !!sym(input$x_var))) + 
          geom_boxplot()
      } else if (input$plot_type == "Density") {
        p <- ggplot(data, aes(x = !!sym(input$x_var), fill = group_var)) +
          geom_density(alpha = 0.5) +
          labs(title = paste0("Density Plot of", input$x_var, "Prices by", input$y_var, "Group", sep = " "),
               x = paste0(input$x_var, "Price", sep = " "), y = "Density", fill = "Open Group")
      }
      
      if (input$facet) {
        p <- p + facet_wrap(~date)
      }
      
      p
})
#)
  
  #Contingency table for grouped open prices
  output$contingency_table <- renderDataTable({
    data <- market_data() 
    data <- data %>%
      mutate(group_var = cut(data[[input$x_var]], breaks = seq(160, max(data$open, na.rm = TRUE) + 10, by = 10),
                             right = FALSE))
    contingency_table <- table(data$group_var)
    contingency_table
  })

  # Numerical Summaries of open and close prices
  output$numerical_summaries <- renderDataTable({
    data <- market_data() 
    numerical_summaries <- data %>%
      summarise(across(c(!!sym(input$x_var), !!sym(input$y_var)), list(mean = mean, sd = sd), na.rm = TRUE))
    numerical_summaries
  })
  
  output$summary_table <- renderTable({
    req(input$x_var)
    summary_table <- summary(market_data()[[input$x_var]])
    data.frame(
      Statistic = names(summary_table),
      Value = as.character(summary_table)
    )
  }, rownames = FALSE)
}

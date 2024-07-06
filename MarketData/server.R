####################################################################################
# This is the server logic for the Market Data Shiny web application.#
####################################################################################

library(shiny)
library(httr)
library(jsonlite)
library(tidyverse)
library(gridExtra)

# Create function for reading in API data
MarketAPI <- function(symbol, type, date_from=NULL, date_to=NULL) {
  #Create URL based on user provided choices
  if (type == "eod") {
    url <- paste0("http://api.marketstack.com/v1/eod", 
                  "?access_key=d4f48f4fa8307e7e0163fc92cce500d2", 
                  "&symbols=", symbol,
                  "&date_from=", date_from,
                  "&date_to=", date_to)
  } else if (type == "splits") {
    url <- paste0("http://api.marketstack.com/v1/splits", 
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
  # Create data table pulled market data 
  output$data_table <- renderDataTable({
    market_data()
  })
  
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
  output$plot <- renderPlot({
    data <- market_data()
    req(input$x_var, input$y_var)

    # Create grouped data for contingency table
    df <- data %>%
      mutate(group_var = cut(input$x_var, breaks = seq(160, max(open, na.rm = TRUE) + 10, by = 10), right = FALSE))

    # Plot 1 : x_var price over time
    p1 <- ggplot(df, aes(x = as.Date(date), y = input$x_var, group = 1)) +
            geom_line(color = "red") +
            labs(title = paste0(input$x_var, "Prices Over Time"), x = "Date", y = paste0(input$x_var, "Price"))

    # Plot 2 : y_var prices over time
    p2 <- ggplot(df, aes(x = as.Date(date), y = input$y_var, group = 1)) +
            geom_line(color = "red") +
            labs(title = paste0(input$y_var, "Prices Over Time"), x = "Date", y = paste0(input$x_var, "Price"))

    # Plot 3: Scatter plot of x_var vs y_var prices
    p3 <- ggplot(df, aes(x = input$x_var, y = input$y_var)) +
            geom_point(color = "blue") +
            labs(title = paste0(input$x_var, "vs", input$y_var, "Prices"), x = paste0(input$x_var, "Price"),
                 y = paste0(input$y_var, "Price"))

    # Plot 4: Density plot of x_var prices over group_var
    p4 <- ggplot(df, aes(x = input$x_var, fill = group_var)) +
            geom_density(alpha = 0.5) +
            labs(title = paste0("Density Plot of", input$x_var, "Prices by", input$y_var, "Group"),
                 x = paste0(input$x_var, "Price"), y = "Density", fill = "Open Group")
    grid.arrange(p1, p2, p3, p4, ncol = 2)
  })


  #Contingency table for grouped open prices
  output$contingency_table <- renderTable({
    data <- data %>%
      mutate(group_var = cut(data[[input$x_var]], breaks = seq(160, max(data$open, na.rm = TRUE) + 10, by = 10), 
                             right = FALSE))
    contingency_table <- table(data$group_var)
    contingency_table
  })

  # Numerical Summaries of open and close prices
  output$numerical_summaries <- renderTable({
  data <- data %>%
    mutate(group_var = cut(data[[input$x_var]], breaks = seq(160, max(data$open, na.rm = TRUE) + 10, by = 10), 
                           right = FALSE))
    numerical_summaries <- data %>%
      summarise(across(c(input$x_var, input$y_var), list(mean = mean, sd = sd), na.rm = TRUE))
    numerical_summaries
  })
}

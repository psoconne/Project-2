library(httr)
library(jsonlite)
library(tidyverse)


MarketAPI <- function(symbol, type, date_from=NULL, date_to=NULL) {
  #url <- paste0("https://newsapi.org/v2/everything?q=", subject, "&from=", date, "&apiKey=", api_key)
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
  
  market_data <- httr::GET(url)
  
  parsed <- jsonlite::fromJSON(rawToChar(market_data$content))
  market_info <- tibble::as_tibble(parsed$data)
  return(market_info)
}

AAPL_data <- MarketAPI("AAPL", type = "intraday")



# Contingency table for grouped open prices
df <- AAPL_data %>%
  mutate(open_group = cut(open, breaks = seq(160, max(open, na.rm = TRUE) + 10, by = 10), right = FALSE))
table(df$open_group)

# Numerical Summaries of open and close prices
numerical_summaries <- df %>%
  summarise(across(c(open, close), list(mean = mean, sd = sd), na.rm = TRUE))
print(numerical_summaries)



# Plot 1 : closing price over time
ggplot(df, aes(x = as.Date(date), y = close, group = 1)) +
  geom_line(color = "red") +
  labs(title = "Closing Prices Over Time", x = "Date", y = "Closing Price")

# Plot 2 : opening prices over time
ggplot(df, aes(x = as.Date(date), y = open, group = 1)) +
  geom_line(color = "red") +
  labs(title = "Opening Prices Over Time", x = "Date", y = "Opening Price")

# Plot 3: Scatter plot of high vs low prices
ggplot(df, aes(x = open, y = close)) +
  geom_point(color = "blue") +
  labs(title = "High vs Low Prices", x = "Low Price", y = "High Price")

# Plot 4: Density plot of open prices
ggplot(df, aes(x = open, fill = open_group)) +
  geom_density(alpha = 0.5) +
  labs(title = "Density Plot of Opening Prices by Open Group", x = "Opening Price", y = "Density", fill = "Open Group")




##############################################################################
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

library(httr)
library(jsonlite)
library(tibble)

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

MarketAPI("AAPL", type = "eod")



#################################
url <- paste0("http://api.marketstack.com/v1/eod", 
              "?access_key=d4f48f4fa8307e7e0163fc92cce500d2", 
              "&symbols=", "AAPL",
              "&date_from=", "2024-06-26",
              "&date_to=", "2024-07-06")
market_data <- httr::GET(url)
market_parsed <- jsonlite::fromJSON(rawToChar(market_data$content))
market_info <- tibble::as_tibble(market_parsed$data)
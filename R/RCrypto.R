# Currencies avalible in the coinmarketcap api
currencies_list_CoinMarketCap <- c("AUD", "BRL", "CAD", "CHF", "CLP", "CNY", "CZK", "DKK", "EUR", "GBP", "HKD", "HUF", "IDR", "ILS", "INR", "JPY", "KRW", "MXN", "MYR", "NOK", "NZD", "PHP", "PKR", "PLN", "RUB", "SEK", "SGD", "THB", "TRY", "TWD", "ZAR", "USD")

globalVariables("symbol")
#symbol <- NULL

# coinmarketcap resets the endpoint every 5 minutes
# to avoid connection timeout httr was used instead of RCurl
# JSON data extracted and conveted to UTF-8 text then set as a dataframe
# time stamps are in Unix Time

# get top 100 currencies by marketcap, default currency to USD
CoinMarketCap_Top100 <- function(currency = "USD") {

  stopifnot(currency %in% currencies_list_CoinMarketCap)

  data.frame(jsonlite::fromJSON(httr::content(httr::GET(paste0('https://api.coinmarketcap.com/v1/ticker/?convert=',currency,'&limit=0')),type = "text", encoding = "UTF-8"),simplifyVector = TRUE))[1:100,]

}

# get all currencies, default currency to USD
CoinMarketCap_All <- function(currency = "USD") {

  stopifnot(currency %in% currencies_list_CoinMarketCap)

  data.frame(jsonlite::fromJSON(httr::content(httr::GET(paste0('https://api.coinmarketcap.com/v1/ticker/?convert=',currency,'&limit=0')),type = "text", encoding = "UTF-8"),simplifyVector = TRUE))

}

# get selected currency by ticker, default currency to USD and tickr to BTC (bitcoin)
CoinMarketCap_Ticker <- function(currency = "USD", ticker = "BTC") {

  stopifnot(currency %in% currencies_list_CoinMarketCap)

  dplyr::filter(data.frame(jsonlite::fromJSON(httr::content(httr::GET(paste0('https://api.coinmarketcap.com/v1/ticker/?convert=',currency,'&limit=0')),type = "text", encoding = "UTF-8"),simplifyVector = TRUE)), symbol %in% c(ticker))

}

\name{CoinMarketCap_Ticker}
\alias{CoinMarketCap_Ticker}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{'Coinmarketcap' Select Ticker Symbol}
\description{Gets a dataframe based on selected ticker symbol}
\usage{
CoinMarketCap_Ticker(currency, ticker)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{currency, ticker}{
  defaults "USD",
  defaults "BTC"
}
}
\details{
currencies availible ("AUD", "BRL", "CAD", "CHF", "CLP", "CNY", "CZK", "DKK", "EUR", "GBP", "HKD", "HUF", "IDR", "ILS", "INR", "JPY", "KRW", "MXN", "MYR", "NOK", "NZD", "PHP", "PKR", "PLN", "RUB", "SEK", "SGD", "THB", "TRY", "TWD", "ZAR", "USD")

Dataframe Columns (id, name, symbol, rank, price_usd, price_btc, 24h_volume_usd, market_cap_usd, available_supply, total_supply, percent_change_1h, percent_change_24h, percent_change_7d, last_updated, {price_(currency),X24h_volume_(currency),market_cap_(currency)})
}
\value{
returns a dataframe with one element for the selected ticker symbol
}
\references{
https://coinmarketcap.com/api/
}
\author{
GitHub = Time-Gnome
}
\examples{
MyData <- CoinMarketCap_Ticker(currency = "CAD",ticker = "ETH")
MyData <- CoinMarketCap_Ticker(ticker = "VEN")
MyData <- CoinMarketCap_Ticker(,"HUSH")
MyData <- CoinMarketCap_Ticker("AUD")
MyData <- CoinMarketCap_Ticker()
}

\name{CoinMarketCap_Top100}
\alias{CoinMarketCap_Top100}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{'Coinmarketcap' Top 100 Currencies}
\description{Gets the top 100 currencies from 'coinmarketcap' api and creates a dataframe}
\usage{
CoinMarketCap_Top100(currency)
}
\arguments{
  \item{currency}{
  defaults "USD"
}
}
\details{
currencies availible ("AUD", "BRL", "CAD", "CHF", "CLP", "CNY", "CZK", "DKK", "EUR", "GBP", "HKD", "HUF", "IDR", "ILS", "INR", "JPY", "KRW", "MXN", "MYR", "NOK", "NZD", "PHP", "PKR", "PLN", "RUB", "SEK", "SGD", "THB", "TRY", "TWD", "ZAR", "USD")

Dataframe Columns (id, name, symbol, rank, price_usd, price_btc, 24h_volume_usd, market_cap_usd, available_supply, total_supply, percent_change_1h, percent_change_24h, percent_change_7d, last_updated, {price_(currency),X24h_volume_(currency),market_cap_(currency)})
}
\value{
returns a dataframe of the top 100 currencies
}
\references{
https://coinmarketcap.com/api/
}
\author{
GitHub = Time-Gnome
}
\examples{
MyData <- CoinMarketCap_Top100(currency = "CAD")
MyData <- CoinMarketCap_Top100("AUD")
MyData <- CoinMarketCap_Top100()
}

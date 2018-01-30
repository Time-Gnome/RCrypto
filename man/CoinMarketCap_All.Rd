\name{CoinMarketCap_All}
\alias{CoinMarketCap_All}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{'Coinamrketcap' All Currencies}
\description{Get all currencies from the 'coinmarketcap' api}
\usage{
CoinMarketCap_All(currency)
}
%- maybe also 'usage' for other objects documented here.
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
returns a dataframe of all the currencies on the coinmarketcap api
}
\references{
https://coinmarketcap.com/api/
}
\author{
GitHub = Time-Gnome
}
\examples{
MyData <- CoinMarketCap_All(currency = "CAD")
MyData <- CoinMarketCap_All("AUD")
MyData <- CoinMarketCap_All()
}


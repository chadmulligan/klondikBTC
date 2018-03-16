#' Appending the USD prices to the dataset of transactions
#'
#' \code{addPrices} add an extra column to the transactions df with the USD BTC value of the transactions, calculated from daily closing prices
#' @param df a dataframe of transactions, result of get()
#' @param prices a 2-column dataframe of daily BTC closing prices 
#' @return Returns a data frame 
#' @export


addPrices <- function(df, prices = df.prices) {
  
  df %>% 
    mutate(Date = format(.$TimeUTC[1], "%Y-%m-%d") %>%
    left_join(prices, by = c("Date")) 
  # %>%
  #   mutate(USD = Close.Price * BTCValue) %>%
  #   select(Hash.transac, Address, BTCValue, USD, I.O, TimeUTC, Block.height, ip.relay)
          
} 
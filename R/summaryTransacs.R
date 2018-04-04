#' Summarize a dataframe of transactions
#'
#' \code{summaryTransacs} returns number of transacitons, date of first and last transactions, transactions over $1000 and biggest transactions in which addresses are involved. 
#' @param df a dataframe obtained with getBTC
#' @param addresses a vector of addresses hash
#' @return Returns a list 
#' @export


summaryTransacs <- function(df, addresses) {

  summaryAddresses <- list(addresses = addresses,
                           totalTransacs = data.frame(),
                           lastfirstTransacs = data.frame(),
                           transacs1000 = data.frame(),
                           biggestTransacs = data.frame(),
                           summaryInput = c(),
                           summaryOutput = c())
  
  class(summaryAddresses) <- c("summaryBTC", class(summaryAddresses))
  
  
  ###Total transactions per address
  a %>% 
    filter(Address %in% addresses) %>% 
    group_by(Address, I.O) %>%
    summarise(nbTransacs = n(), 
              totalBTC = sum(BTCValue), 
              totalUSD = sum(USD))  %>%
    arrange(desc(totalUSD), Address) -> summaryAddresses$totalTransacs
  
  summaryAddresses$summaryInput <- colSums(summaryAddresses$totalTransacs[summaryAddresses$totalTransacs$I.O == "Input", 3:5])
  summaryAddresses$summaryOutput <- colSums(summaryAddresses$totalTransacs[summaryAddresses$totalTransacs$I.O == "Output", 3:5])
  
  names(summaryAddresses$summaryInput) <- c("Number of Transactions", "Total BTC", "Total USD")
  names(summaryAddresses$summaryOutput) <- c("Number of Transactions", "Total BTC", "Total USD")  
  
  
  
  ###first and last transactions of each address
  a %>%
    filter(Address %in% addresses) %>% 
    select(Address, TimeUTC) %>%
    group_by(Address) %>% 
    arrange(desc(TimeUTC)) %>%
    slice(c(1, length(TimeUTC))) %>%
    cbind(Type = rep(c("Last", "First"), nrow(.)/2)) %>%
    spread(Type, TimeUTC) -> summaryAddresses$lastfirstTransacs
  
  
  
  ###Addresses transactions over $1000 from the set of addresseses
  a %>%
    filter(Address %in% addresses) %>% 
    group_by(Address, Hash.transac, I.O, TimeUTC) %>%
    summarise(totalUSD = sum(USD), totalBTC = sum(BTCValue)) %>%
    arrange(desc(totalUSD)) %>% 
    filter(totalUSD > 1000) -> summaryAddresses$transacs1000
  
  
  
  ###Transactions over $1000 involving set of addresseses
  a %>%
    group_by(Hash.transac) %>%
    summarise(totalUSD = sum(USD), totalBTC = sum(BTCValue)) %>%
    arrange(desc(totalUSD)) %>% 
    filter(totalUSD > 1000) -> summaryAddresses$biggestTransacs
  
  
  ###Results
  summaryAddresses

}

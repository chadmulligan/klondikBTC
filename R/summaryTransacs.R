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
                           summaryInputOutput = data.frame()
                           )

  class(summaryAddresses) <- c("summaryBTC", class(summaryAddresses))


  ###Total transactions per address
  df %>%
    filter(Address %in% addresses) %>%
    group_by(Address, I.O) %>%
    summarise(nbTransacs = n(),
              totalBTC = sum(BTCValue),
              totalUSD = sum(USD)) -> summaryAddresses$totalTransacs


  ###Input/Output summary
  summaryAddresses$summaryInputOutput <- as.data.frame(rbind(colSums(summaryAddresses$totalTransacs[summaryAddresses$totalTransacs$I.O == "Input",
                                                                                                    3:5]),
                                                             colSums(summaryAddresses$totalTransacs[summaryAddresses$totalTransacs$I.O == "Output",
                                                                                                    3:5])),
                                                       stringsAsFactors = FALSE
                                                       )

  summaryAddresses$summaryInputOutput[3, ] <- c(sum(summaryAddresses$summaryInputOutput[1]),
                                                diff(-summaryAddresses$summaryInputOutput[, 2]),
                                                diff(-summaryAddresses$summaryInputOutput[, 3]))

  colnames(summaryAddresses$summaryInputOutput) <- c("NbofTransactions", "TotalBTC", "TotalUSD")
  row.names(summaryAddresses$summaryInputOutput) <- c("Total Input", "Total Output", "TOTAL")



  ###first and last transactions of each address
  df %>%
    filter(Address %in% addresses) %>%
    select(Address, TimeUTC) %>%
    group_by(Address) %>%
    arrange(desc(TimeUTC)) %>%
    slice(c(1, length(TimeUTC))) %>%
    cbind(Type = rep(c("Last", "First"), nrow(.)/2)) %>%
    spread(Type, TimeUTC) -> summaryAddresses$lastfirstTransacs



  ###Addresses transactions over $1000 from the set of addresseses
  df %>%
    filter(Address %in% addresses) %>%
    group_by(Address, Hash.transac, I.O, TimeUTC) %>%
    summarise(totalBTC = sum(BTCValue), totalUSD = sum(USD)) %>%
    arrange(desc(totalUSD)) %>%
    filter(totalUSD > 1000) -> summaryAddresses$transacs1000



  ###Biggest transacitons involving set of addresseses
  df %>%
    group_by(Hash.transac, TimeUTC) %>%
    summarise(totalBTC = sum(BTCValue), totalUSD = sum(USD)) %>%
    arrange(desc(totalUSD)) %>%
    filter(totalUSD > 1000) -> summaryAddresses$biggestTransacs


  ###Results
  summaryAddresses

}

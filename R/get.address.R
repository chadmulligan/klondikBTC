#' Downloads an address object from a single BTC address including offset transactions (when txs >50)
#'
#' \code{get.address} downloads address object from blockchain.info API.
#' @param address a vector of a single address hash
#' @param apikey Blockchain.info API key
#' @return Returns a list
#' @export

get.address <- function(address=c(), apikey) {

  l <- list()

  url <- paste0("https://blockchain.info/rawaddr/", address, "?api_code=", apikey)

  #returns the first 50 transactions
  l[[1]] <- fromJSON(url)


  #gets all transactions if >50
  if(l[[1]]$n_tx > 50) {

    for (i in 1:trunc(l[[1]]$n_tx/50)) {
      url <- paste0("https://blockchain.info/rawaddr/", address, "?api_code=", apikey, paste0("&offset=", i*50))
      l[[i+1]] <- fromJSON(url)
    }

    l

    } else {l}

}

#' Downloads an address object from a single BTC address including offset transactions (when txs >50)
#'
#' \code{get.address} downloads address object from blockchain.info API.
#' @param addresses a vector of a single address hash
#' @param offset an integer that defines how many sets of 50 txs must be skipped backward starting from the last tx to date
#' @param apikey Blockchain.info API key
#' @return Returns a list
#' @export

get.address <- function(address=c(), offset=c(), apikey) {

  l <- list()

  get.info <- function(addr=c(), offs=c()) {
    url <- paste0("https://blockchain.info/rawaddr/", addr, offs, "?api_code=", apikey)
    res <- fromJSON(url)
  }

  #returns the first 50 transactions
  l[[1]] <- get.info(address, offset)

  #gets all transactions if >50
  if(l[[1]]$n_tx > 50) {

    for (i in 1:trunc(l[[1]]$n_tx/50)) {
      l[[i+1]] <- get.info(addr=address, offs=paste0("?offset=", i*50))
    }

    l

  } else {l}

}

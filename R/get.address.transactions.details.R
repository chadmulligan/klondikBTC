#' Extracts the txs details from an address object
#'
#' \code{get.address.transactions.details} returns inputs, outputs, value, time, block height and ip relay for each transaction
#' @param addr.list an address list
#' @return Returns a data.frame
#' @export


get.address.transactions.details <- function(addr.list=list()) {
  
  addr.list %$% txs %>%
    lapply(get.transaction.details) %>%
    do.call(rbind, .) %>% 
    data.frame %>% 
    mutate(X3 = as.numeric(as.character(X3))/100000000,
           X5 = X5 %>% as.character %>% as.numeric %>% anytime(asUTC=TRUE)) %>% 
    setNames(c("Hash.transac", "Address", "BTCValue",  "I.O", 
               "TimeUTC", "Block.height", "ip.relay")) -> res
  
}
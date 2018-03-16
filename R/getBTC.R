#' Downloading and flattening ina df all transactions info from a set of BTC addresses
#'
#' \code{getBTC} is a wrapper - returns all transactions info of a vector of addresses from blockchain.info API.
#' @param addresses a vector of addresses hash
#' @return Returns a data frame
#' @export

getBTC <- function(addresses = c()) {

  addresses %>% lapply(get.address) %>% 
    unlist(recursive=FALSE) %>%
    lapply(get.address.transactions.details)  %>% 
    bind_rows %>%
    unique() %>%
    mutate(Hash.transac = unlist(.$Hash.transac),
           Address = unlist(.$Address), 
           BTCValue = unlist(.$BTCValue), 
           I.O = unlist(.$I.O),
           Block.height = unlist(.$Block.height),
           ip.relay = unlist(.$ip.relay)) %>% 
    addPrices()

  } 
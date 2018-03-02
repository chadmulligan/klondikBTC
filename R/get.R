#' Downloading and flattening ina df all transactions info from a set of BTC addresses
#'
#' \code{get} is a wrapper - returns all transactions info of a vector of addresses from blockchain.info API.
#' @param addresses a vector of addresses hash
#' @return Returns a data frame
#' @export

get <- function(addresses = c()) {

#deprecated for reducing queries to blockchain.info
# validate addresses - returns a boolean vector
# test <- suppressWarnings(sapply(addresses, function(x) {
#   jsonlite::fromJSON(paste0("https://blockexplorer.com/api/addr-validate/", x))
# }))

# if(sum(test)!=length(addresses)){
#   
#   #identify invalid addresses
#   stop(paste(paste(c(names(test)[which(!test)]), collapse = ", "), "is not a valid bitcoin address"), 
#        call.=FALSE)
#   
# } else{
  
  addresses %>% lapply(get.address) %>% 
    unlist(recursive=FALSE) %>%
    lapply(get.address.transactions.details)  %>% 
    bind_rows %>%
    unique() %>%
    mutate(Hash.transac = unlist(.$Hash.transac),
           Address = unlist(.$Address), 
           I.O = unlist(.$I.O),
           Block.height = unlist(.$Block.height),
           ip.relay = unlist(.$ip.relay))

}

#}

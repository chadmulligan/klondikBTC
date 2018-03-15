#' Checks whether the addresses have been reported on bitcoinwhoswho.com
#'
#' \code{get.scam} gets scam report date and description for the addresses, or returns NULL not report has been made.
#' @param adds a vector of a single address hash
#' @param apikey bitcoiwhoswho.com API key
#' @return Returns a dataframe or NULL
#' @export

get.scam <- function (adds, apikey="5d3a75b8-22ccf291-025f4e8d-739bfeaf") { 
  
  get.singleScam <- function (add) {
    
    url <- paste0("http://bitcoinwhoswho.com/api/scam/", apikey, "?address=", add)
    res <- fromJSON(url)
    
  }
  
  res <- lapply(adds, get.singleScam) 
  
  scams <- sapply(res, function(x) length(x$scams)) 
  
  if(all(scams==0)) {
    
    NULL 
    
    } else { 
      
      scam.adds <- adds[scams!=0]  
      scams.details <- res[scams!=0]
      
      nb.scamrep <- sapply(scams.details, function(x) length(x$scams))
      
      addresses <- rep(scam.adds, times = nb.scamrep)
      r.date <- c(unlist(sapply(scams.details, function(x) sapply(x$scams, function(y) y["reported_date"]))))
      descr <- c(unlist(sapply(scams.details, function(x) sapply(x$scams, function(y) y["scam_details"]))))
      
      df.scams<- data.frame(address = addresses, reportdate = r.date, description = descr, row.names = NULL)
      colnames(df.scams) <- c("Address", "Date of Report", "Description") 
      
      df.scams
      
    }
  
  } 
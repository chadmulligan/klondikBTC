#' Extracts the details of a single transaction
#'
#' \code{get.transaction.details} returns inputs, outputs, value, time, block height and ip relay for a single transaction
#' @param tx an transaction list
#' @return Returns a data.frame
#' @export


get.transaction.details <- function(tx=list()) {

  #Getting inputs
  tx %$% inputs %>% sapply("[", "prev_out")  %>%
    sapply("[", c("addr", "value")) %>%
    t %>%
    cbind("Input") -> ins


  #Getting outputs
  tx %$% out %>% sapply("[", c("addr", "value")) %>%
    t %>%
    cbind("Output") -> outs

  #Binding and getting extra info
  block_height <- if(tx$lock_time!=0) tx$lock_time else tx$block_height #taking care of locktime if applicable

  rbind(ins, outs) %>%
    cbind(tx$hash, ., tx$time, block_height, tx$relayed_by) %>%
    unname -> res

}

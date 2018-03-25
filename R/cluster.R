#' Pairs the original address with addresses from the same wallet
#'
#' \code{cluster} pairs the original vector of address(es) with siblings input addresses
#' @param addresses a vector of addresses hash
#' @param df a dataframe obtained with getBTC
#' @return Returns a vector of addresses hash
#' @export


cluster <- function(df=data.frame(),address=c()) {

   # n<=10 removes mixers
  df %>%
    select(Hash.transac, Address, I.O) %>%
    group_by(Hash.transac) %>%
    add_count(I.O) %>%
    filter(Address %in% address, I.O == "Input", n<=10) %>%
    select(Hash.transac) -> res


  df %>%
    select(Hash.transac, Address, I.O) %>%
    filter((Hash.transac %in% pull(res)), I.O == "Input") %>%
    select(Address) %>%
    distinct() %>%
    pull() -> res2


  if (length(res2[!(res2 %in% address)]) == 0) {
    stop("no other address found") } else {res2[!(res2 %in% address)]}

}


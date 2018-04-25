#' Getting the forceNetworkD3 graph of transactions.
#'
#' \code{graphD3} takes a dataframe returned by getBTC() and graphes the transactions.
#' @param df a dataframe of transactions returned by get().
#' @param addresses a vector of addresses of interest for grouping purposes.
#' @return Returns a forceNetworkD3 graph.
#' @export

graphD3 <- function(df = data.frame(), addresses = c()) {

###preparing nodes###

  nodes <- tibble::tibble(nodes = c(unique(df$Hash.transac), unique(df$Address)))

  nodes %>%
    tibble::rowid_to_column("id") %>%
    mutate(id = id - 1,
           group = c(rep("Transaction", length(unique(df$Hash.transac))),
                     rep("Address", length(unique(df$Address))))) %>%
    as.data.frame() -> nodes

  nodes$group[nodes$nodes %in% addresses] <- "Address Searched"


###preparing edges###

  df %>%
      select(Hash.transac, Address, I.O, BTCValue) %>%
      mutate(input= ifelse(I.O=="Input", Address, Hash.transac),
             output= ifelse(I.O=="Input", Hash.transac, Address)) %>%
      select(input, output, BTCValue, -c(Address, Hash.transac, I.O)) -> edges

  edges %>%
    left_join(nodes, by = c("input" = "nodes")) %>%
    rename(from = id) %>%
    left_join(nodes, by = c("output" = "nodes")) %>%
    rename(to = id) %>%
    select(from, to, BTCValue) -> edges

  ##taking care of cases of multiple inputs/outputs from same address
  edges %>%
    group_by(from, to) %>%
    summarise(sum(BTCValue)) %>%
    as.data.frame()-> edges


###Graph###
  networkD3::forceNetwork(Links = edges,
                          Nodes = nodes,
                          Source = "from",
                          Target = "to",
                          NodeID = "nodes",
                          Value = "sum(BTCValue)",
                          Group = "group",
                          opacity = 1,
                          fontSize = 16,
                          linkDistance = 150,
                          zoom = TRUE,
                          legend = TRUE,
                          arrows = TRUE,
                          colourScale = JS("d3.scaleOrdinal(d3.schemeCategory20);"))

}

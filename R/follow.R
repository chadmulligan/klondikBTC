

follow <- function(df = data.frame, ) {
  
  ###follow by amounts###
  
  a %>%
    arrange(desc(BTCValue)) %>% 
    mutate(ratio = BTCValue / lag(BTCValue)) %>% 
    filter(abs(ratio - 1) < 0.01) %>% 
    pull(distinct(Hash.transac)) -> aa 
    
  filter(a, Hash.transac %in% aa)
   
  data.table::shift(a[a$BTCValue == 0.00300000,])
  
}



 res <- c(sort(a$BTCValue)[-nrow(a)] / sort(a$BTCValue)[-1], NA)
 pos <- which(abs(aa$res-1)<0.01) 
 postotal <- c(pos, pos + 1)
 transacs <- unique(aa$Hash.transac[postotal][aa[postotal, ]$I.O %in% "Input"])
    
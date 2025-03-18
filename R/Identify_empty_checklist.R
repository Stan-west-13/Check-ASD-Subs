Identify_empty_checklist <-  function(vocab_checklist, p_na, code){
  
  z <- vocab_checklist %>%
    select(starts_with(code)) %>%
    mutate(empty = rowMeans(is.na(.)) >= p_na)
  
  return(z[z$empty == FALSE,])
}




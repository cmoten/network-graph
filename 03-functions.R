get_node_values <- function(node_id){
  node_row <- which(nodes$id == node_id)
  vec <- nodes$evn_state[node_row] %>%
    stringr::str_split(pattern = "_") %>%
    unlist() %>%
    str_split(pattern = "") %>%
    unlist()
  
  vec <- vec[c(FALSE, TRUE)] %>% as.integer()
  vec
}

calc_path_weight <- function(node_id1, node_id2){
  vec_diff <- get_node_values(node_id1) - get_node_values(node_id2)
  res <- 0
  if(any(vec_diff < 0)){
    res <- 10 #ex from 11111 - 12111
  } else if(any(vec_diff > 0)){
    res <- 1 #ex: from 13111 - 12111
  } else {
    res <- 5
  }
  
  res
}


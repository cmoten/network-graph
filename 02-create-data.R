
#data-----
factor_one <- 1:5
factor_two <- 1:5
factor_three <- 1:4
factor_four <- 1:4
factor_five <- 1:4

data_table <- expand.grid(f1 = factor_one,
                          f2 = factor_two,
                          f3 = factor_three,
                          f4 = factor_four,
                          f5 = factor_five,
                          stringsAsFactors = FALSE)
  

remove_rows <- which(data_table[,1] == 4 & data_table[,2] == 1 |
                          data_table[,1] == 5 & data_table[,2] == 1 |
                       data_table[,1] == 5 & data_table[,2] == 2 |
                       data_table[,1] == 4 & data_table[,3] == 1 |
                       data_table[,1] == 5 & data_table[,3] == 1 |
                       data_table[,1] == 5 & data_table[,3] == 2 |
                       data_table[,1] == 4 & data_table[,4] == 1 |
                       data_table[,1] == 5 & data_table[,4] == 1 |
                       data_table[,1] == 5 & data_table[,4] == 2 |
                       data_table[,1] == 4 & data_table[,5] == 1 |
                       data_table[,1] == 5 & data_table[,5] == 1 |
                       data_table[,1] == 5 & data_table[,5] == 2 |
                       data_table[,2] == 4 & data_table[,3] == 1 |
                       data_table[,2] == 5 & data_table[,3] == 1 |
                       data_table[,2] == 5 & data_table[,3] == 2 |
                       data_table[,2] == 4 & data_table[,4] == 1 |
                       data_table[,2] == 5 & data_table[,4] == 1 |
                       data_table[,2] == 5 & data_table[,4] == 2 |
                       data_table[,2] == 4 & data_table[,5] == 1 |
                       data_table[,2] == 5 & data_table[,5] == 1 |
                       data_table[,2] == 5 & data_table[,5] == 2 |
                       data_table[,3] == 4 & data_table[,4] == 1 |
                       data_table[,3] == 4 & data_table[,5] == 1 |
                       data_table[,4] == 4 & data_table[,5] == 1 |
                       data_table[,1] == 1 & data_table[,2] == 4 |
                       data_table[,1] == 1 & data_table[,2] == 5 |
                       data_table[,1] == 2 & data_table[,2] == 5 |
                       data_table[,1] == 1 & data_table[,3] == 4 |
                       data_table[,1] == 1 & data_table[,4] == 4 |
                       data_table[,1] == 1 & data_table[,5] == 4 |
                       data_table[,2] == 1 & data_table[,3] == 4 |
                       data_table[,2] == 1 & data_table[,4] == 4 |
                       data_table[,2] == 1 & data_table[,5] == 4 |
                       data_table[,3] == 1 & data_table[,4] == 4 |
                       data_table[,3] == 1 & data_table[,5] == 4 |
                       data_table[,4] == 1 & data_table[,5] == 4) 

data_table <- data_table[-remove_rows, ]

factor_types <- c("O", "L", "C", "F", "R")



data_table$score <- 30 - rowSums(data_table)

data_table$f1 <- paste0("O", data_table$f1)
data_table$f2 <- paste0("L", data_table$f2)
data_table$f3 <- paste0("C", data_table$f3)
data_table$f4 <- paste0("F", data_table$f4)
data_table$f5 <- paste0("R", data_table$f5)








data_table <- data_table[order(data_table$score, decreasing = TRUE), ]

data_table <- data_table %>%
  tidyr::unite("nodes", f1:f5, remove = FALSE)

#nodes-----
node_table <- data.frame(
  id = paste0("n", stringr::str_pad(1:nrow(data_table), width = 3, side = "left", pad = "0")),
  evn_state = data_table$nodes,
  weight = data_table$score,
  stringsAsFactors = FALSE
)

write.csv(node_table, file = "data/env-state-nodes.csv", row.names = FALSE)

#edges-----
edge_table <- data.frame( from = "n000",
                          to = "n000",
                          stringsAsFactors = FALSE)

loop_limit <- nrow(node_table) - 1

print("creating edge table...")
for(i in 1:loop_limit){
  j <- i + 1
  diff <- node_table$weight[i] - node_table$weight[j]
  while(diff >= 0 & diff < 2){
    next_row <- nrow(edge_table) + 1
    new_data <- c(node_table$id[j], node_table$id[i])
    edge_table[next_row, ] <- new_data
    j <- j + 1
    diff <- data_table$score[i] - data_table$score[j]
    if(is.na(diff)){ #j is past the last row of the node table
      break
    }
  }
}
print("done!")

edge_table <- edge_table[-1, ]
write.csv(edge_table, file = "data/env-state-edges.csv", row.names = FALSE)

rm(list = ls())

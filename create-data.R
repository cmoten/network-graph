
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
                          f5 = factor_five)

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

data_table$score <- 30 - rowSums(data_table)

data_table$f1 <- paste0("O", data_table$f1)
data_table$f2 <- paste0("L", data_table$f2)
data_table$f3 <- paste0("C", data_table$f3)
data_table$f4 <- paste0("F", data_table$f4)
data_table$f5 <- paste0("R", data_table$f5)

data_table <- data_table[order(data_table$score, decreasing = TRUE), ]



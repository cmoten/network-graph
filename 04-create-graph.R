#full-network-----
nodes <- read.csv("data/env-state-nodes.csv", stringsAsFactors = FALSE)
edges <- read.csv("data/env-state-edges.csv", stringsAsFactors = FALSE)
start_node <- which(nodes$evn_state == "O3_L3_C3_F3_R3")
goal <- which(nodes$evn_state == "O1_L1_C1_F1_R1")
fail <- which(nodes$evn_state == "O5_L5_C4_F4_R4")
edges$path_weight <- purrr::map2_dbl(edges$from, edges$to, calc_path_weight)
node_col <- rep("lightgrey", nrow(nodes))
node_col[c(start_node, goal, fail)] <- c("yellow", "green", "red")
edges$weight <- rep(1.5, nrow(edges))
edges$weight[c(start_node, goal, fail)] <- 10
net <- igraph::graph_from_data_frame(d=edges, vertices=nodes, directed=T)

write.csv(edges, file = "data/env-state-edges.csv", row.names = FALSE)

#networkplot-----
# net_layout <- igraph::layout_with_gem(net)
# plot(net,vertex.label=NA, layout = net_layout, vertex.size=edges$weight,
#      vertex.color=node_col)


#shortest-paths-----
way_point <-  which(nodes$evn_state == "O3_L3_C3_F2_R3")
paths <- igraph::all_shortest_paths(net, from = start_node, to = c(way_point, goal), weights = edges$path_weight)

path_next <- sapply(paths$res, "[[", 2)
path_next_id_fin <- path_next == 285
possible_next_paths <- which(path_next_id_fin == TRUE)
nodes$evn_state[paths$res[[possible_next_paths[10000]]]]

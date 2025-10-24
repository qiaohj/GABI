library(data.table)
library(sf)
library(RSQLite)
library(DBI)
library(ggplot2)
setwd("/media/huijieqiao/Butterfly/GABI/GABI")
conn<-dbConnect(RSQLite::SQLite(), "../Configuration/configuration.sqlite")
distances<-data.table(dbReadTable(conn, "distances"))
mask<-data.table(dbReadTable(conn, "mask"))
dbDisconnect(conn)


distances<-distances[dist==1]
build_adjacency_list <- function(dt) {
  adj_list <- list()
  nodes <- unique(c(dt$i, dt$j))
  for (node in nodes) {
    adj_list[[as.character(node)]] <- integer(0)
  }
  for (k in 1:nrow(dt)) {
    i_node <- as.character(dt$i[k])
    j_node <- as.character(dt$j[k])
    adj_list[[i_node]] <- c(adj_list[[i_node]], dt$j[k])
  }
  for (node in names(adj_list)) {
    adj_list[[node]] <- unique(adj_list[[node]])
  }
  return(adj_list)
}
adj_list <- build_adjacency_list(distances)

shortest_distance_bfs <- function(start_node, end_node, adj_list) {
  if (start_node == end_node) {
    return(0)
  }
  
  start_char <- as.character(start_node)
  end_char <- as.character(end_node)
  if (!(start_char %in% names(adj_list)) || !(end_char %in% names(adj_list))) {
    warning("Start or end node not found in the graph.")
    return(-1)
  }
  distances <- list()
  for (node in names(adj_list)) {
    distances[[node]] <- Inf
  }
  distances[[start_char]] <- 0
  queue <- c(start_node)
  
  while (length(queue) > 0) {
    current_node <- queue[1]
    queue <- queue[-1]
    current_char <- as.character(current_node)
    
    current_dist <- distances[[current_char]]
    
    neighbors <- adj_list[[current_char]]
    
    for (neighbor in neighbors) {
      neighbor_char <- as.character(neighbor)
      
      if (distances[[neighbor_char]] == Inf) {
        new_dist <- current_dist + 1
        distances[[neighbor_char]] <- new_dist
        if (neighbor == end_node) {
          return(new_dist)
        }
        
        queue <- c(queue, neighbor)
      }
    }
  }
  
  return(-1)
}


#NA, 33434,33515,33514,33595,33676,33757
#SA, 9076, 9157, 9238
distances[j==X]
X <- 33434
Y <- 33757
polygon<-read_sf("../Shape/isea3h8/N_S_America.shp")
table(polygon$continent)

NA.target<-c(33434,33515,33514,33595,33676,33757)
SA.target<-c(9076, 9157, 9238)
polygon$min.dist<-0
for (i in c(1:nrow(polygon))){
  print(paste(i, nrow(polygon)))
  item<-polygon[i,]
  if (item$min.dist>0){
    next()
  }
  target<-NULL
  if (item$continent=="North America"){
    target<-SA.target
  }
  if (item$continent=="South America"){
    target<-NA.target
  }
  if (is.null(target)){
    next()
  }
  min.dist<-Inf
  for (j in c(1:length(target))){
    dist<-shortest_distance_bfs(start_node = item$seqnum, end_node = target[j], adj_list = adj_list)
    min.dist<-min(min.dist, dist)
  }
  polygon[i, "min.dist"]<-min.dist
}
saveRDS(polygon, "../Data/cells.with.dist.rda")
ggplot(polygon)+geom_sf(aes(fill=min.dist))+
  scale_fill_continuous(low="blue", high="red")

seeds<-data.table(polygon)
seeds$geometry<-NULL

table(seeds[between(min.dist, 1, 65)]$continent)
seeds<-seeds[between(min.dist, 1, 65)]
ggplot(polygon)+
  geom_sf(fill="lightgrey")+
  geom_sf(data=polygon[which(polygon$seqnum %in% seeds$seqnum),], aes(fill=min.dist))+
  scale_fill_continuous(low="blue", high="red")
seeds
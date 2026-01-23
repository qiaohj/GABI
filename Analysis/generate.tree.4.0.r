library(data.table)
library(sf)
library(RSQLite)
library(DBI)
library(ggplot2)
library(ggtree)
library(treeio)

setwd("/media/huijieqiao/Butterfly/GABI/GABI")

conn<-dbConnect(RSQLite::SQLite(), "../Configuration/conf.sqlite")
simulations<-data.table(dbReadTable(conn, "simulations"))
dbDisconnect(conn)

result_folder<-"/media/huijieqiao/Butterfly/GABI/Results"
ns<-read_sf("../Shape/isea3h8/N_S_America.shp")
ns<-data.table(ns)
ns$geometry<-NULL

build_tree_string <- function(current_id, data) {
  node_info <- data[sp_id == current_id]
  
  origin <- node_info$origin_year
  last_seen <- node_info$last_seen_year
  duration <- abs(origin - last_seen)
  
  node_label <- paste0("SP", current_id, " @ ", origin, "-", last_seen, ":", duration)
  
  children <- data[parent_id == current_id, sp_id]
  
  if (length(children) == 0) {
    return(node_label)
  } else {
    child_strings <- sapply(children, function(x) build_tree_string(x, data))
    combined_children <- paste(child_strings, collapse = ",")
    return(paste0("(", combined_children, ")", node_label))
  }
}


for (i in c(1:nrow(simulations))){
  item<-simulations[i]
  f<-sprintf("%s/%s", result_folder, item$label)
  log.f<-sprintf("%s/%s/species_record.rda", result_folder, item$label)
  if (!file.exists(log.f)){
    next()
  }
  tree.f<-sprintf("%s/%s/tree.rda", result_folder, item$label)
  if (file.exists(tree.f)){
    next()
  }
  saveRDS(NULL, tree.f)
  x<-readRDS(log.f)
  root_id <- x[is.na(parent_id), sp_id]
  
  
  tree_result <- paste0(build_tree_string(root_id, x), ";")
  saveRDS(tree_result, tree.f)
  if (F){
    tree <- read.tree(text = tree_result)
    
    ggtree(tree) + 
      geom_tiplab(size = 3) + 
      geom_nodelab(aes(label = label),
                   hjust = -0.1, 
                   vjust = -0.5, 
                   size = 2, 
                   color = "darkred") +
      theme_tree2() + 
      labs(title = "Visualization from tree_result String")
  }
}



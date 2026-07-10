library(data.table)
library(sf)
library(RSQLite)
library(DBI)
library(ggplot2)
library(ggh4x)
library(ape)
library(phytools)
#library(ggtree)
library(phangorn)
setDTthreads(20)
setwd("/media/huijieqiao/Butterfly/GABI/GABI")
sp<-readRDS("../Data/Tables/virtual.species.NULL.rda")
table(sp$continent)
sp_N<-sp[,.(N=.N), by=list(seed_id, nb, da)]
sp$Parent<-sub("-[^-]*$", "", sp$sp_id)

if (F){
  ff<-gsub("/media/huijieqiao/Butterfly/GABI/Results/", "", folders)
  
  ff[!ff %in% labels]
}
labels<-unique(sp$label)
sp$year<-as.numeric(sp$year)
#sp_clean<-sp[continent %in% c("North America", "South America")]

#l<-labels[6]
#which(labels=="40680.MODERATE-MODERATE.GOOD")
#l<-"40680.MODERATE-MODERATE.GOOD"
#sp$sp_id


assign_named_row <- function(dt, row, vals) {
  for (name in names(vals)) {
    dt[row, (name) := vals[[name]]]
  }
}
labels<-labels[sample(length(labels), length(labels))]
j=1
#labels<-labels[labels %in% c("2782.BIG.GOOD", "11030.MODERATE.GOOD", "36125.MODERATE.GOOD",
#                             "11021.NARROW.GOOD", "40356.NARROW.GOOD")]
for (j in c(1:length(labels))){
  l<-labels[j]
  print(paste(j, length(labels), l))
  target<-sprintf("../Data/temp.N.sp.NULL/%s.rda", l)
  if (file.exists(target)){
    next()
  }
  saveRDS(NULL, target)
  d<-sp[label==l]
  
  d$year<-d$year * -1
  
  species<-d[, .(from=min(year), to=max(year)), by=list(sp_id, Parent)]
  
  species[sp_id==Parent, Parent:="Seed"]
  root_id <- "Seed"
  edges_dt <- species[sp_id != Parent]
  all_ids <- unique(c(species$sp_id, species$Parent))
  
  
  parent_ids <- unique(species$Parent)
  tip_labels <- setdiff(all_ids, setdiff(parent_ids, root_id))
  tip_labels<-tip_labels[tip_labels!=root_id]
  
  all_nodes_except_root <- setdiff(all_ids, c(tip_labels, root_id))
  node_labels <- c(root_id, all_nodes_except_root)
  
  tip_map <- setNames(seq_along(tip_labels), tip_labels)
  node_map <- setNames(seq_along(node_labels) + length(tip_labels), node_labels)
  id_map <- c(tip_map, node_map)
  edge_matrix <- matrix(c(id_map[edges_dt$Parent], id_map[edges_dt$sp_id]), ncol = 2)
  
  tree <- list(
    edge = edge_matrix,
    tip.label = tip_labels,
    node.label = node_labels,
    Nnode = length(node_labels),
    edge.length = edges_dt$to - edges_dt$from
  )
  class(tree) <- "phylo"
  if (F){
    ggtree(tree) + 
      geom_tiplab() +
      theme_tree2()
  }
  
  
  
  species$is_leaf<-!(species$sp_id %in% species$Parent)
  if (nrow(species)==1){
    species$is_leaf<-T
  }
  species$type<-"Species"
  species[is_leaf==T & to!=0]$type<-"Extinction"
  species[is_leaf==F]$type<-"Speciation"
  species$handle<-F
  setorderv(species, c("from", "to"), c(1, 1))
  species$continent<-""
  species$Extinction<-0
  species$Speciation<-0
  species$Species<-0
  if (nrow(species)>=1){
    for (i in c(1:nrow(species))){
      if (i/100==round(i/100)){
        print(paste(i, nrow(species)))
      }
      sp.id<-species[i]$sp_id
      item<-d[sp_id==sp.id]
      item<-item[year==min(year) & continent %in% c("North America", "South America")]
      item<-item[,.(N=sum(N)), by=list(year, continent, sp_id, seed_id, nb, da, label, Parent)]
      if (nrow(item)==1){
        continent<-item$continent
      }
      if (nrow(item)==0){
        continent<-"bridge"
      }
      if (nrow(item)==2){
        continent<-"Two continents"
      }
      if (nrow(item)>2){
        asdasdf
      }
      species[i]$continent<-continent
      if (species[i]$is_leaf==F){
        
        n_tips <- length(tree$tip.label)
        node_number <- which(tree$node.label == as.character(sp.id)) + n_tips
        descendants <- Descendants(tree, node_number, type = "all")
        descendant_tips <- descendants[descendants <= n_tips]
        descendant_labels <- tree$tip.label[descendant_tips]
        subtree <- keep.tip(tree, descendant_labels)
        sub.sp<-species[sp_id %in% c(subtree$tip.label, subtree$node.label)]
        n<-table(sub.sp$type)
        assign_named_row(species, i, n)
        
      }
    }
  }else{
    n<-table(species$type)
    assign_named_row(species, 1, n)
  }
  species$origin_continent<-unique(d[year==-1899 & 
                                       continent %in% c("North America", "South America")]$continent)
  
  saveRDS(species, target)
}

if (F){
  all_N<-list()
  for (j in c(1:length(labels))){
    l<-labels[j]
    strs<-strsplit(l, "\\.")[[1]]
    print(paste(j, length(labels), l))
    target<-sprintf("../Data/temp.N.sp.NULL/%s.rda", l)
    df<-readRDS(target)
    df$NB<-strs[[2]]
    df$DA<-strs[[3]]
    if (is.null(df)){
      asdf
    }
    all_N[[length(all_N)+1]]<-df
  }
  all_N<-rbindlist(all_N, fill=T)
  all_N[continent=="", continent:=all_N[continent==""]$origin_continent]
  all_N<-all_N[!is.na(sp_id)]
  saveRDS(all_N, "../Data/Tables/species.type.N.NULL.rda")
  table(all_N$NB)
  all_N_gg<-all_N
  p<-ggplot(all_N_gg[Speciation<500])+geom_point(aes(x=Extinction, y=Speciation))+
    facet_grid(origin_continent+NB~continent)+
    scale_x_sqrt()+scale_y_sqrt()
  p
  ggsave(p, filename="../Figures/N.Speciation.Extinction.by.species.type.NULL.png",
         width=10, height=12)
  
  p<-ggplot(all_N_gg)+geom_violin(aes(x=continent, color=origin_continent, y=Speciation))+
    facet_wrap(~NB)+
    theme(axis.text.x = element_text(angle = 45, hjust = 1))+scale_y_sqrt()
  p
  ggsave(p, filename="../Figures/N.Speciation.boxplot.by.species.type.NULL.png",
         width=8, height=6)
}

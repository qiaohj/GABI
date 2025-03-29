library(data.table)
library(ape)
library(phytools)
library(ggtree)
library(ggplot2)
setwd("/media/huijieqiao/WD22T_11/GABI/Script")
df<-readRDS("../Data/Tables/100k.speciation.years/virtual.species.rda")
df_N<-df[,.(N=.N), by=list(seed_id, nb, da)]
View(df_N)
head(df)
seed_id<-"39581"
df$Parent<-sub("-[^-]*$", "", df$sp_id)
df[sp_id==seed_id]$Parent<-""
df[sp_id==seed_id]
d<-df[label=="39581.MODERATE-MODERATE.GOOD"]
d$year<-d$year * -1
#d<-d[continent %in% c("South America", "North America")]
#View(d)

species_withns<-d[,.(from=min(year), to=max(year)), by=list(sp_id, Parent, continent)]
species_withns$sp_id<-as.factor(species_withns$sp_id)
species_withns$Parent<-as.factor(species_withns$Parent)
species_withns[Parent==""]$Parent<-NA

species<-d[, .(from=min(year), to=max(year)), by=list(sp_id, Parent)]
species$sp_id<-as.factor(species$sp_id)
species$Parent<-as.factor(species$Parent)
species[Parent==""]$Parent<-NA

tree <- as.phylo(species)
tree$edge.length <- species$to - species$from
node_times <- node.depth.edgelength(tree)
cutoff_time <- 2000
nodes_before <- which(node_times <= cutoff_time)
total_nodes <- length(nodes_before) 
is_leaf <- nodes_before <= Ntip(tree)
leaf_count <- sum(is_leaf)
node_count <- total_nodes - leaf_count

species$is_leaf<-!(species$sp_id %in% species$Parent)
species$type<-"Species"
species[is_leaf & to!=0]$type<-"Extinction"
species[!is_leaf & sp_id!=seed_id]$type<-"Speciation"
species[sp_id==seed_id]$type<-"Root"
species$handle<-F
setorderv(species, c("from", "to"), c(1, 1))

for (i in c(1:nrow(species))){
  item<-species[i]
  if (item$type=="Speciation" & item$handle==F){
    brothers<-species[Parent==item$Parent]
    species_details<-d[sp_id %in% brothers$sp_id & year==item$from]
  }
}
item_prev<-NULL

for (y in c(max(species_withns$from):min(species_withns$to))){
  item<-species_withns[from>=y & to<=y]
  item_all<-species_withns[from>=y]
  N_species<-item[, .(N=length(unique(sp_id))), by=list(continent)]
  if (!is.null(item_prev)){
    gain_item<-item[!(sp_id %in% item_prev$sp_id)]
    loss_item<-item_prev[!(sp_id %in% item$sp_id)]
    if (nrow(gain_item)>0){
      adsf
    }
    if (nrow(loss_item)>0){
      adsf
    }
  }
  item_prev<-item
  
}
species_withns[!sp_id %in% species$sp_id]








species[sp_id=="39581-1-2-2-1"]
species[Parent=="39581-1-2-2-1"]


ggtree(tree) +
  #geom_nodelab(hjust=-.1)+
  geom_tiplab() +
  theme_tree2() +
  labs(color = "End time") +
  scale_x_continuous(breaks = seq(-1800, max(species$to), by = 100))

N.Sp.Ex<-readRDS("../Data/Tables/100k.speciation.years/N.Speciation.Extinction.rda")
d_N.Sp.Ex<-N.Sp.Ex[nb=="MODERATE-MODERATE" & da=="GOOD" & seed_id==39581]
View(d_N.Sp.Ex)

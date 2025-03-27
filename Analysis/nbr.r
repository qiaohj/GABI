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
df$Parent<-sub("-[^-]*$", "", df$sp_id)
df[sp_id==seed_id]$Parent<-""
df[sp_id==39581]
d<-df[label=="39581.MODERATE-MODERATE.GOOD"]
View(d)


species<-d[, .(from=max(year), to=min(year)), by=list(sp_id, Parent)]
species$sp_id<-as.factor(species$sp_id)
species$Parent<-as.factor(species$Parent)
species$from<-1800-species$from
species$to<-1800-species$to
species[Parent==""]$Parent<-NA
setorderv(species, c("from", "to"), c(-1, -1))
tree <- as.phylo(species)
tree$edge.length <- species$to - species$from
node_times <- node.depth.edgelength(tree)

cutoff_time <- 2000

nodes_before <- which(node_times <= cutoff_time)
total_nodes <- length(nodes_before) 

is_leaf <- nodes_before <= Ntip(tree)
leaf_count <- sum(is_leaf)
node_count <- total_nodes - leaf_count

ggtree(tree) +
  geom_tiplab() +
  theme_tree2() +
  labs(color = "End time") +
  scale_x_continuous(breaks = seq(0, max(species$to), by = 100))

N.Sp.Ex<-readRDS("../Data/Tables/100k.speciation.years/N.Speciation.Extinction.rda")
d_N.Sp.Ex<-N.Sp.Ex[nb=="MODERATE-MODERATE" & da=="GOOD" & seed_id==39581]
View(d_N.Sp.Ex)

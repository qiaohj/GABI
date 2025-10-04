library(data.table)
library(sf)
library(RSQLite)
library(DBI)
library(ggplot2)
library(ggh4x)
library(ape)
library(phytools)
library(ggtree)
library(phangorn)
setwd("/media/huijieqiao/Butterfly/GABI/GABI")
if (F){
  sp<-readRDS("../Data/Tables/100k.speciation.years/virtual.species.rda")
  sp_N<-sp[,.(N=.N), by=list(seed_id, nb, da)]
  sp$Parent<-sub("-[^-]*$", "", sp$sp_id)
  
  sp[continent=="bridge1" | continent=="bridge2", continent:="North America"]
  sp.new<-sp[,.(N=sum(N)), by=list(year, continent, sp_id, seed_id, nb, da, label, Parent)]
  
  saveRDS(sp.new, ("../Data/Tables/100k.speciation.years/virtual.species.without.bridge.rda"))
}
sp<-readRDS("../Data/Tables/100k.speciation.years/virtual.species.without.bridge.rda")
#sp[year==1182 & sp_id=="11125-1-2-2"]

#sp.new[year==1182 & sp_id=="11125-1-2-2"]

labels<-unique(sp$label)

assign_named_row <- function(dt, row, vals) {
  for (name in names(vals)) {
    dt[row, (name) := vals[[name]]]
  }
}
labels<-labels[sample(length(labels), length(labels))]

for (j in c(1:length(labels))){
  l<-labels[j]
  print(paste(j, length(labels), l))
  target<-sprintf("../Data/temp.N.sp/%s.rda", l)
  if (file.exists(target)){
    next()
  }
  saveRDS(NULL, target)
  d<-sp[label==l]
  
  d$year<-d$year * -1
  
  species<-d[, .(from=min(year), to=max(year)), by=list(sp_id, Parent)]
  species$sp_id<-as.factor(species$sp_id)
  species$Parent<-as.factor(species$Parent)
  species[Parent==""]$Parent<-"Seed"
  
  tree <- as.phylo(species)
  tree$edge.length <- species$to - species$from
  
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
  if (nrow(species)>1){
    for (i in c(1:nrow(species))){
      sp.id<-species[i]$sp_id
      item<-d[sp_id==sp.id]
      item<-item[year==min(year) & continent %in% c("North America", "South America")]
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
  species$origin_continent<-d[year==-1800]$continent
  saveRDS(species, target)
}

all_N<-list()
for (j in c(1:length(labels))){
  l<-labels[j]
  strs<-strsplit(l, "\\.")[[1]]
  print(paste(j, length(labels), l))
  target<-sprintf("../Data/temp.N.sp/%s.rda", l)
  df<-readRDS(target)
  df$NB<-strs[[2]]
  df$DA<-strs[[3]]
  if (is.null(df)){
    asdf
  }
  all_N[[length(all_N)+1]]<-df
}
all_N_df<-rbindlist(all_N, fill=T)
all_N<-all_N_df[!is.na(sp_id)]
all_N[continent=="", continent:=all_N[continent==""]$origin_continent]
saveRDS(all_N, "../Data/Tables/100k.speciation.years/species.type.N.without.bridge.rda")
table(all_N$NB)
all_N_gg<-all_N[NB %in% c("BROAD-BROAD", "BIG-BIG", "MODERATE-MODERATE", "NARROW-NARROW")]

p<-ggplot(all_N_gg)+geom_point(aes(x=Extinction, y=Speciation))+
  facet_grid(origin_continent+NB~continent)+
  scale_x_sqrt()+scale_y_sqrt()
p
ggsave(p, filename="../Figures/N.Speciation.Extinction.by.species.type.png",
       width=10, height=12)

p<-ggplot(all_N_gg)+geom_violin(aes(x=continent, color=origin_continent, y=Speciation))+
  facet_wrap(~NB)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+scale_y_sqrt()
p
ggsave(p, filename="../Figures/N.Speciation.boxplot.by.species.type.png",
       width=8, height=6)

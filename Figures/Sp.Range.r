library(data.table)
library(ggplot2)
library(sf)
library(patchwork)
setDTthreads(20)
sf::sf_use_s2(FALSE)

setwd("/media/huijieqiao/Butterfly/GABI/GABI")
if (F){
  seeds.all<-readRDS("../Data/Tables/random.seeds.rda")
  seeds.all$rep<-NULL
  seeds.all$N_SPECIES<-NULL
  seeds.all<-unique(seeds.all)
  xx<-list()
  for (y in c(1799:0)){
    xx[[as.character(y)]]<-list()
  }
  for (i in c(1:nrow(seeds.all))){
    print(paste(i, nrow(seeds.all)))
    item<-seeds.all[i]
    f<-sprintf("../Results/%s/nb.rda", item$label)
    log<-readRDS(f)
    list_of_data_tables <- split(log, by = "year")
    for (y in c(1799:0)){
      y_item<-list_of_data_tables[[as.character(y)]]
      if (nrow(y_item)>0){
        xx[[as.character(y)]][[length(xx[[as.character(y)]])+1]]<-y_item
      }
    }
  }
  for (y in c(0:1799)){
    print(y)
    ddd<-rbindlist(xx[[as.character(y)]])
    saveRDS(ddd, sprintf("../Data/Richness/%d.rda", y))
  }
}

if (F){
  seeds.all<-readRDS("../Data/Tables/random.seeds.rda")
  seeds.all$rep<-NULL
  seeds.all$N_SPECIES<-NULL
  seeds.all<-unique(seeds.all)
  xx<-list()
  for (y in c(1799:0)){
    xx[[as.character(y)]]<-list()
  }
  
  
  for (i in c(1:nrow(seeds.all))){
    print(paste(i, nrow(seeds.all)))
    item<-seeds.all[i]
    f<-sprintf("../Results.NULL/%s/species.richness.rda", item$label)
    log<-readRDS(f)
    list_of_data_tables <- split(log, by = "year")
    for (y in c(1799:0)){
      y_item<-list_of_data_tables[[as.character(y)]]
      if (nrow(y_item)>0){
        xx[[as.character(y)]][[length(xx[[as.character(y)]])+1]]<-y_item
      }
    }
  }
  
  for (y in c(0:1799)){
    print(y)
    ddd<-rbindlist(xx[[as.character(y)]])
    saveRDS(ddd, sprintf("../Data/Richness.NULL/%d.rda", y))
  }
}

if (F){
  hexagon<-readRDS("../Data/cells.with.dist.rda")
  
  mammals<-st_read("../Shape/IUCN/MAMMALS/MAMMALS_TERRESTRIAL_ONLY.shp")
  species<-unique(mammals$binomial)
  dis.list<-list()
  for (i in c(1:length(species))){
    print(paste(i, length(species)))
    sp<-species[i]
    item<-mammals[which(mammals$binomial ==sp),]
    #item<-item[which(item$presence %in% c(1)),]
    #item<-item[which(item$origin %in% c(1, 2, 3)),]
    #item<-item[which(item$seasonal %in% c(1, 2)),]
    if (nrow(item)==0){
      next()
    }
    index<-st_intersects(item, hexagon)
    index<-unique(unlist(index))
    if (F){
      plot(hexagon$geometry)
      plot(item$geometry, add=T)
      plot(hexagon[index,]$geometry, add=T)
    }
    v_items<-data.table(hexagon[index,])
    
    if (nrow(v_items)>0){
      v_items$species<-sp
      
      dis.list[[length(dis.list)+1]]<-v_items
    }
  }
  
  dis.df<-rbindlist(dis.list)
  dis.range<-dis.df[, .(N.Cell=length(unique(seqnum)),
                        min.lon=min(lon),
                        max.lon=max(lon),
                        min.lat=min(lat),
                        max.lat=max(lat)),
                    by=list(species)]
  dis.range<-dis.range[N.Cell>1]
  saveRDS(dis.range, "../Data/IUCN/mammals.dis.range.rda")
  dis.df<-dis.df[species %in% dis.range$species]
  dis.df$geometry<-NULL
  dis.df$min.dist<-NULL
  dis.df$continent<-NULL
  dis.df$lat<-NULL
  dis.df$lon<-NULL
  dis.geo<-merge(hexagon, dis.df, by="seqnum")
  saveRDS(dis.geo, "../Data/IUCN/mammals.dis.rda")
  dis.richness<-dis.df[, .(N.species=length(unique(species))),
                       by=list(seqnum)]
  dis.richness<-merge(hexagon, dis.richness, by="seqnum")
  saveRDS(dis.richness, "../Data/IUCN/mammals.richness.rda")
  #ggplot(dis.richness)+geom_sf(aes(fill=N.species))
}

seeds<-readRDS("../Data/Tables/random.seeds.rda")
seeds<-unique(seeds$seed_id)
sim.dis<-readRDS("../Data/Richness/0.rda")

richness<-df[, .(N_SP=sum(N_SP)), by=list(global_id)]
colnames(richness)<-c("seqnum", "N.sim.species")

mammal.richness<-readRDS("../Data/IUCN/mammals.richness.rda")
full.richness<-merge(mammal.richness, richness, by="seqnum")
table(mammal.richness[which(mammal.richness$seqnum %in% seeds),]$continent)
ggplot(mammal.richness[which(mammal.richness$seqnum %in% seeds),])+
  geom_sf((aes(fill=N.species)))+
  scale_fill_gradient(low="blue", high="red")
plot(full.richness$N.sim.species, 
     full.richness$N.species)
cor(full.richness$N.sim.species, 
    full.richness$N.species)


p1<-ggplot()+ 
  geom_sf(data=full.richness,  aes(fill=N.species),
          color=NA, linewidth=0.1) +
  scale_fill_gradient2(low="#2166AC",
                       mid="#F7F7F7",
                       high="#B2182B",
                       midpoint = median(full.richness$N.species))+
  theme(
    axis.line = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_line(linetype = "dashed", linewidth = 0.5, color="#dab27f"),
    plot.background = element_rect(fill="#fde7c0"),
    panel.background = element_rect(fill="#fde7c0"),
    legend.background = element_rect(fill = "#fde7c0", color = NA),
    legend.title = element_blank(),
    legend.position="bottom",
    legend.key.width = unit(1, 'cm'),
    plot.title = element_text(hjust = 0.5)
  )

p2<-ggplot()+ 
  geom_sf(data=full.richness,  aes(fill=N.sim.species),
          color=NA, linewidth=0.1) +
  scale_fill_gradient2(low="#2166AC",
                       mid="#F7F7F7",
                       high="#B2182B",
                       midpoint = median(full.richness$N.sim.species))+
  theme(
    axis.line = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_line(linetype = "dashed", linewidth = 0.5, color="#dab27f"),
    plot.background = element_rect(fill="#fde7c0"),
    panel.background = element_rect(fill="#fde7c0"),
    legend.background = element_rect(fill = "#fde7c0", color = NA),
    legend.title = element_blank(),
    legend.position="bottom",
    legend.key.width = unit(1, 'cm'),
    plot.title = element_text(hjust = 0.5)
  )

p1+p2+plot_annotation(
  title = 'Mammals richness (left) and Simulated richess (right)',
  tag_levels = 'A'
)


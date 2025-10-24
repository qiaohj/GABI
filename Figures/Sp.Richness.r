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
    f<-sprintf("../Results/%s/species.richness.rda", item$label)
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

if (F){
  seeds.all<-readRDS("../Data/Tables/random.seeds.rda")
  sim.dis<-readRDS("../Data/Richness/0.rda")
  hexagon<-readRDS("../Data/cells.with.dist.rda")
  cells<-data.table(seqnum=hexagon$seqnum, seed.continent=hexagon$continent)
  sim.dis$seed_id<-as.numeric(sim.dis$seed_id)
  sim.dis.geo.full<-merge(sim.dis, cells, by.y="seqnum", by.x="seed_id")
  sim.dis.geo.full[continent %in% c("bridge1", "bridge2"), continent:="North America"]
  coms<-data.table(expand.grid(nb=c("MODERATE-MODERATE", "BIG-BIG"),
                               da=c("POOR", "GOOD")))
  
  full.richness.list<-list()
  by.continent.richness.list<-list()
  by.continent.nb.da.richness.list<-list()
  
  for (r in c(1:10)){
    print(r)
    sim.dis.geo<-sim.dis.geo.full[seed_id %in% seeds.all[rep==r]$seed_id]
    richness<-sim.dis.geo[, .(N.species=sum(N_SP),
                              rep=r), 
                          by=list(global_id)]
    full.richness.list[[length(full.richness.list)+1]]<-richness
    for (ccc in c("North America", "South America")){
      item<-sim.dis.geo[seed.continent==ccc]
      richness<-item[, .(N.species=sum(N_SP),
                         rep=r,
                         seed.continent=ccc), 
                     by=list(global_id)]
      by.continent.richness.list[[length(by.continent.richness.list)+1]]<-richness
      if (F){
        ggplot()+ 
          geom_sf(data=richness,  aes(fill=N.species),
                  color=NA, linewidth=0.1) +
          scale_fill_gradient2(low="#2166AC",
                               mid="#F7F7F7",
                               high="#B2182B",
                               midpoint = median(richness$N.species))
      }
      for (i in c(1:nrow(coms))){
        com<-coms[i]
        item2<-item[nb==com$nb & da==com$da]
        richness<-item2[, .(N.species=sum(N_SP),
                            rep=r,
                            seed.continent=ccc,
                            nb=com$nb,
                            da=com$da), 
                        by=list(global_id)]
        
        by.continent.nb.da.richness.list[[length(by.continent.nb.da.richness.list)+1]]<-richness
      }
    }
  }
  full.richness.df<-rbindlist(full.richness.list)
  by.continent.richness.df<-rbindlist(by.continent.richness.list)
  by.continent.nb.da.richness.df<-rbindlist(by.continent.nb.da.richness.list)
  
  saveRDS(full.richness.df, "../Data/Tables/Richness/full.richness.rda")
  saveRDS(by.continent.richness.df, "../Data/Tables/Richness/by.continent.richness.rda")
  saveRDS(by.continent.nb.da.richness.df, "../Data/Tables/Richness/by.continent.nb.da.richness.rda")
  
}
seeds<-readRDS("../Data/Tables/random.seeds.rda")
seeds<-unique(seeds$seed_id)
hexagon<-readRDS("../Data/cells.with.dist.rda")
richness<-readRDS("../Data/Tables/Richness/full.richness.rda")
richness<-richness[, .(N.sim.species=mean(N.species)),
                   by=list(global_id)]

colnames(richness)<-c("seqnum", "N.sim.species")

mammal.richness<-readRDS("../Data/IUCN/mammals.richness.rda")
full.richness<-merge(mammal.richness, richness, by="seqnum")
table(mammal.richness[which(mammal.richness$seqnum %in% seeds),]$continent)
plot(full.richness$N.sim.species, 
    full.richness$N.species)
cor(full.richness$N.sim.species, 
     full.richness$N.species, 
    method = "spearman")


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

richness<-readRDS("../Data/Tables/Richness/by.continent.richness.rda")
richness<-richness[, .(N.species=mean(N.species)),
                   by=list(global_id, seed.continent)]

richness.na<-richness[seed.continent=="North America"]
richness.sa<-richness[seed.continent=="South America"]
richness.na<-merge(hexagon, richness.na, by.x="seqnum", by.y="global_id")
richness.sa<-merge(hexagon, richness.sa, by.x="seqnum", by.y="global_id")

median.v<-median(c(richness.na$N.species, richness.sa$N.species))
range.v<-range(c(richness.na$N.species, richness.sa$N.species))
p.na<-ggplot()+ 
  geom_sf(data=richness.na,  aes(fill=N.species),
          color=NA, linewidth=0.1) +
  scale_fill_gradient2(low="#2166AC",
                       mid="#F7F7F7",
                       high="#B2182B",
                       midpoint = median.v,
                       limits=range.v)+
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


p.sa<-ggplot()+ 
  geom_sf(data=richness.sa,  aes(fill=N.species),
          color=NA, linewidth=0.1) +
  scale_fill_gradient2(low="#2166AC",
                       mid="#F7F7F7",
                       high="#B2182B",
                       midpoint = median.v,
                       limits=range.v)+
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

p.na+p.sa+plot_annotation(
  title = 'NA origin (left) and SA origin (right)',
  tag_levels = 'A'
)

richness<-readRDS("../Data/Tables/Richness/by.continent.nb.da.richness.rda")
richness<-richness[, .(N.species=mean(N.species)),
                   by=list(global_id, seed.continent, nb, da)]



for (i in c(1:nrow(coms))){
  com<-coms[i]
  richness.na<-richness[seed.continent=="North America" & nb==com$nb & da==com$da]
  richness.sa<-richness[seed.continent=="South America" & nb==com$nb & da==com$da]
  richness.na<-merge(hexagon, richness.na, by.x="seqnum", by.y="global_id")
  richness.sa<-merge(hexagon, richness.sa, by.x="seqnum", by.y="global_id")
  
  
  median.v<-median(c(richness.na$N.species, richness.sa$N.species))
  range.v<-range(c(richness.na$N.species, richness.sa$N.species))
  p.na<-ggplot()+ 
    geom_sf(data=richness.na,  aes(fill=N.species),
            color=NA, linewidth=0.1) +
    scale_fill_gradient2(low="#2166AC",
                         mid="#F7F7F7",
                         high="#B2182B",
                         midpoint = median.v,
                         limits=range.v)+
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
  
  
  p.sa<-ggplot()+ 
    geom_sf(data=richness.sa,  aes(fill=N.species),
            color=NA, linewidth=0.1) +
    scale_fill_gradient2(low="#2166AC",
                         mid="#F7F7F7",
                         high="#B2182B",
                         midpoint = median.v,
                         limits=range.v)+
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
  
  ppp<-p.na+p.sa+plot_annotation(
    title = sprintf('NA origin (left) and SA origin (right), %s %s', com$nb, com$da),
    tag_levels = 'A'
  )
  ggsave(ppp, filename=sprintf("../Figures/Richness/%s.%s.png", com$nb, com$da), width=10, height=8)
}

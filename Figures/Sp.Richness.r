library(data.table)
library(ggplot2)
library(sf)
library(patchwork)
setDTthreads(20)
sf::sf_use_s2(FALSE)

setwd("/media/huijieqiao/Butterfly/GABI/GABI")

#Skip the following script until the next comment. It is useless now.
if (F){
  seeds.all<-readRDS("../Data/Tables/random.seeds.threshold.by.nb.distribution.rda")
  if (F){
    seeds.all<-seeds.all[nb %in% c("MODERATE-MODERATE", "BIG-BIG")]
    saveRDS(seeds.all, "../Data/Tables/random.seeds.threshold.by.nb.distribution.rda")
  }
  unique(seeds.all$nb)
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
  seeds.all<-readRDS("../Data/Tables/random.seeds.threshold.by.nb.distribution.rda")
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
  hexagon<-read_sf("../Shape/isea3h8/N_S_America.shp")
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
  mammal.nb.filter<-readRDS("../Data/Tables/mammal.nb.filter.table.rda")
  
  dis.range<-dis.df[, .(N.Cell=length(unique(seqnum)),
                         min.lon=min(lon),
                         max.lon=max(lon),
                         min.lat=min(lat),
                         max.lat=max(lat)),
                     by=list(species)]
  dis.range<-dis.range[N.Cell>1]
  saveRDS(dis.range, "../Data/IUCN/mammals.dis.range.rda")
  rm("species")
  dis.range.filter<-dis.df[species %in% mammal.nb.filter$species, 
                    .(N.Cell=length(unique(seqnum)),
                        min.lon=min(lon),
                        max.lon=max(lon),
                        min.lat=min(lat),
                        max.lat=max(lat)),
                    by=list(species)]
  dis.range.filter<-dis.range.filter[N.Cell>1]
  saveRDS(dis.range.filter, "../Data/IUCN/mammals.dis.range.filter.rda")
  
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
  
  dis.df<-dis.df[species %in% dis.range.filter$species]
  
  dis.geo<-merge(hexagon, dis.df, by="seqnum")
  saveRDS(dis.geo, "../Data/IUCN/mammals.dis.filter.rda")
  dis.richness<-dis.df[, .(N.species=length(unique(species))),
                       by=list(seqnum)]
  dis.richness<-merge(hexagon, dis.richness, by="seqnum")
  saveRDS(dis.richness, "../Data/IUCN/mammals.richness.filter.rda")
  
  #ggplot(dis.richness)+geom_sf(aes(fill=N.species))
}
### end of useless code

if (F){
  seeds.all<-readRDS("../Data/Tables/random.seeds.threshold.by.nb.distribution.rda")
  sim.dis<-readRDS("../Data/Tables/Final.Distribution.rda")
  if (F){
    test<-sim.dis[seed_id==739]
    dis.test<-hexagon[which(hexagon$seqnum %in% unique(test$global_id)),]
    ggplot(dis.test)+geom_sf(data=hexagon)+geom_sf(fill="red")
  }
  hexagon<-read_sf("../Shape/isea3h8/N_S_America.shp")
  if (F){
    ggplot(hexagon[which(hexagon$seqnum %in% unique (sim.dis$global_id)),])+
      geom_sf()
  }
  cells<-data.table(seqnum=hexagon$seqnum, seed.continent=hexagon$continent)
  sim.dis$seed_id<-as.numeric(sim.dis$seed_id)
  sim.dis.geo.full<-merge(sim.dis, cells, by.y="seqnum", by.x="seed_id")
  cells<-data.table(seqnum=hexagon$seqnum, continent=hexagon$continent)
  sim.dis.geo.full<-merge(sim.dis.geo.full, cells, by.y="seqnum", by.x="global_id")
  
  #sim.dis.geo.full[continent %in% c("bridge1", "bridge2"), continent:="North America"]
  #sim.dis.geo.full[global_id %in% c(9580, 9662,9744,9663,9745,9664), continent:="South America"]
  coms<-data.table(expand.grid(nb=c("MODERATE-MODERATE", "BIG-BIG"),
                               da=c("POOR", "GOOD")))
  
  full.richness.list<-list()
  by.continent.richness.list<-list()
  by.continent.nb.da.richness.list<-list()
  
  for (r in c(1:100)){
    print(r)
    sim.dis.geo<-sim.dis.geo.full[seed_id %in% seeds.all[rep==r]$seed_id]
    sim.dis.geo<-sim.dis.geo[, .(N_SP=length(unique(sp_id))), 
                             by=list(nb, da, seed_id, seed.continent, continent, global_id)]
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

if (F){
  seeds.all<-readRDS("../Data/Tables/random.seeds.threshold.by.nb.distribution.rda")
  sim.dis<-readRDS("../Data/Tables/Final.Distribution.NULL.rda")
  if (F){
    test<-sim.dis[seed_id==739]
    dis.test<-hexagon[which(hexagon$seqnum %in% unique(test$global_id)),]
    ggplot(dis.test)+geom_sf(data=hexagon)+geom_sf(fill="red")
  }
  hexagon<-read_sf("../Shape/isea3h8/N_S_America.shp")
  if (F){
    ggplot(hexagon[which(hexagon$seqnum %in% unique (sim.dis$global_id)),])+
      geom_sf()
  }
  cells<-data.table(seqnum=hexagon$seqnum, seed.continent=hexagon$continent)
  sim.dis$seed_id<-as.numeric(sim.dis$seed_id)
  sim.dis.geo.full<-merge(sim.dis, cells, by.y="seqnum", by.x="seed_id")
  cells<-data.table(seqnum=hexagon$seqnum, continent=hexagon$continent)
  sim.dis.geo.full<-merge(sim.dis.geo.full, cells, by.y="seqnum", by.x="global_id")
  
  #sim.dis.geo.full[continent %in% c("bridge1", "bridge2"), continent:="North America"]
  #sim.dis.geo.full[global_id %in% c(9580, 9662,9744,9663,9745,9664), continent:="South America"]
  coms<-data.table(expand.grid(nb=c("MODERATE-MODERATE", "BIG-BIG"),
                               da=c("POOR", "GOOD")))
  
  full.richness.list<-list()
  by.continent.richness.list<-list()
  by.continent.nb.da.richness.list<-list()
  
  for (r in c(1:100)){
    print(r)
    sim.dis.geo<-sim.dis.geo.full[seed_id %in% seeds.all[rep==r]$seed_id]
    sim.dis.geo<-sim.dis.geo[, .(N_SP=length(unique(sp_id))), 
                             by=list(nb, da, seed_id, seed.continent, continent, global_id)]
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
  
  
  saveRDS(full.richness.df, "../Data/Tables/Richness/full.richness.NULL.rda")
  saveRDS(by.continent.richness.df, "../Data/Tables/Richness/by.continent.richness.NULL.rda")
  saveRDS(by.continent.nb.da.richness.df, "../Data/Tables/Richness/by.continent.nb.da.richness.NULL.rda")
  
}
source("Figures/common.r")
seeds<-readRDS("../Data/Tables/random.seeds.threshold.by.nb.distribution.rda")
range(seeds$N_SPECIES)
seeds<-unique(seeds$seed_id)
hexagon<-readRDS("../Data/cells.with.dist.rda")
richness<-readRDS("../Data/Tables/Richness/full.richness.rda")
richness<-richness[, .(N.sim.species=mean(N.species),
                       N.sim.species.sd=sd(N.species)),
                   by=list(global_id)]
if (F){
  ggplot(hexagon[which(hexagon$seqnum %in% richness$global_id),])+
    geom_sf()
}
colnames(richness)<-c("seqnum", "N.sim.species", "N.sim.species.sd")

richness.NULL<-readRDS("../Data/Tables/Richness/full.richness.NULL.rda")
richness.NULL<-richness.NULL[, .(N.sim.species=mean(N.species)),
                   by=list(global_id)]

colnames(richness.NULL)<-c("seqnum", "N.sim.NULL.species")


mammal.richness<-readRDS("../Data/IUCN/mammals.richness.rda")
mammal.richness.filter<-readRDS("../Data/IUCN/mammals.richness.filter.rda")
mammal.richness.filter<-data.table(seqnum=mammal.richness.filter$seqnum,
                                   N.species.filter=mammal.richness.filter$N.species)
full.richness<-merge(mammal.richness, richness, by="seqnum", all=T)
#full.richness[which(full.richness$continent=="bridge2" & full.richness$N.sim.species==0), 
#              "N.sim.species"]<-
#  full.richness[which(full.richness$continent=="bridge2" & full.richness$N.sim.species==0),]$N.species * 10

full.richness<-merge(full.richness, richness.NULL, by="seqnum", all=T)
full.richness<-merge(full.richness, mammal.richness.filter, by="seqnum", all=T)
full.richness[is.na(full.richness$N.sim.NULL.species), "N.sim.NULL.species"]<-0
full.richness[is.na(full.richness$N.sim.species), "N.sim.species"]<-0
full.richness[is.na(full.richness$N.species), "N.species"]<-0
full.richness[is.na(full.richness$N.species.filter), "N.species.filter"]<-0

full.richness<-full.richness[which(!is.na(full.richness$continent)),]

table(mammal.richness[which(mammal.richness$seqnum %in% seeds),]$continent)
full.richness.cor[continent=="bridge2"]
full.richness.cor<-data.table(full.richness)
#full.richness.cor<-full.richness.cor[N.sim.species>0 & N.species>0 & N.sim.NULL.species>0]
plot(full.richness.cor$N.sim.species, 
     full.richness.cor$N.species)
cor(full.richness.cor$N.sim.species, 
    full.richness.cor$N.species, 
    method = "spearman")

cor(full.richness.cor$N.sim.species, 
    full.richness.cor$N.species)

plot(full.richness.cor$N.sim.species, 
     full.richness.cor$N.species.filter)
full.richness[which(is.na(full.richness$N.species.filter)),]

cor(full.richness.cor$N.sim.species, 
    full.richness.cor$N.species.filter, 
    method = "spearman")

cor(full.richness.cor[N.sim.NULL.species>0 & N.species>0]$N.sim.NULL.species, 
    full.richness.cor[N.sim.NULL.species>0 & N.species>0]$N.species)

cor(full.richness.cor[N.sim.NULL.species>0 & N.species>0]$N.sim.NULL.species, 
    full.richness.cor[N.sim.NULL.species>0 & N.species>0]$N.species, 
    method = "spearman")

full.richness[which(full.richness$continent=="bridge2"),]
write_sf(full.richness, "../Data/Shape/Species.richness/Species.richness.shp")
p1<-ggplot()+ 
  geom_sf(data=hexagon, fill=NA, color="lightgray")+
  geom_sf(data=full.richness,  aes(fill=N.species),
          color=NA, linewidth=0.1) +
  labs(fill="Number of mammal species")+
  scale_fill_gradient2(low=color_low,
                       mid=color_mid,
                       high=color_high,
                       midpoint = median(full.richness$N.species))+
  coord_sf(crs=map_crs)+
  guide_colorbar_top+
  map_theme
if (F){
p1.2<-ggplot()+ 
  geom_sf(data=hexagon, fill=NA, color="lightgray")+
  geom_sf(data=full.richness,  aes(fill=N.species.filter),
          color=NA, linewidth=0.1) +
  scale_fill_gradient2(low="#2166AC",
                       mid="#F7F7F7",
                       high="#B2182B",
                       midpoint = median(full.richness$N.species.filter))+
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
#p1.2
}
p2<-ggplot()+ 
  geom_sf(data=hexagon, fill=NA, color="lightgray")+
  geom_sf(data=full.richness,  aes(fill=N.sim.species),
          color=NA, linewidth=0.1) +
  
  labs(fill="Number of simulated species")+
  scale_fill_gradient2(low=color_low,
                       mid=color_mid,
                       high=color_high,
                       midpoint = 
                         median(full.richness[which(full.richness$N.sim.species>0),]$N.sim.species))+
  coord_sf(crs=map_crs)+
  guide_colorbar_top+
  map_theme

p3<-ggplot()+ 
  geom_sf(data=hexagon, fill=NA, color="lightgray")+
  geom_sf(data=full.richness,  aes(fill=N.sim.NULL.species),
          color=NA, linewidth=0.1) +
  labs(fill="Number of simulated species (Null model)")+
  scale_fill_gradient2(low=color_low,
                       mid=color_mid,
                       high=color_high,
                       midpoint = median(full.richness[which(full.richness$N.sim.NULL.species>0),]$N.sim.NULL.species))+
  coord_sf(crs=map_crs)+
  guide_colorbar_top+
  map_theme

p<-p1+p2+p3+plot_annotation(
  title = 'Mammals richness (left), Simulated richess (middle) and NULL model (right)',
  tag_levels = 'A'
)
p
ggsave(p, filename="../Figures/Species.richness.pdf", width=12, height = 7)

richness<-readRDS("../Data/Tables/Richness/by.continent.richness.rda")
richness<-richness[, .(N.species=mean(N.species)),
                   by=list(global_id, seed.continent)]

richness.na<-richness[seed.continent=="North America"]
richness.sa<-richness[seed.continent=="South America"]
richness.na<-merge(hexagon, richness.na, by.x="seqnum", by.y="global_id", all=T)
richness.sa<-merge(hexagon, richness.sa, by.x="seqnum", by.y="global_id", all=T)
richness.na[which(is.na(richness.na$N.species)), "N.species"]<-0
richness.sa[which(is.na(richness.sa$N.species)), "N.species"]<-0

median.v<-median(c(richness.na[which(richness.na$N.species>0),]$N.species, 
                   richness.sa[which(richness.sa$N.species>0),]$N.species))
range.v<-range(c(richness.na$N.species, richness.sa$N.species))
p.na<-ggplot()+ 
  geom_sf(data=hexagon, fill=NA, color="lightgray")+
  geom_sf(data=richness.na,  aes(fill=N.species),
          color=NA, linewidth=0.1) +
  scale_fill_gradient2(low=color_low,
                       mid=color_mid,
                       high=color_high,
                       midpoint = median.v,
                       limits=range.v)+
  coord_sf(crs=map_crs)+
  guide_colorbar_top+
  map_theme


p.sa<-ggplot()+ 
  geom_sf(data=hexagon, fill=NA, color="lightgray")+
  geom_sf(data=richness.sa,  aes(fill=N.species),
          color=NA, linewidth=0.1) +
  scale_fill_gradient2(low=color_low,
                       mid=color_mid,
                       high=color_high,
                       midpoint = median.v,
                       limits=range.v)+
  coord_sf(crs=map_crs)+
  guide_colorbar_top+
  map_theme

p<-p.na+p.sa+plot_annotation(
  title = 'NA origin (left) and SA origin (right)',
  tag_levels = 'A'
)

ggsave(p, filename="../Figures/Species.Richness.By.Seed.continent.pdf", width=10, height=7)

richness<-readRDS("../Data/Tables/Richness/by.continent.nb.da.richness.rda")
richness<-richness[, .(N.species=mean(N.species)),
                   by=list(global_id, seed.continent, nb, da)]

coms<-unique(richness[, c("nb", "da")])

for (i in c(1:nrow(coms))){
  com<-coms[i]
  richness.na<-richness[seed.continent=="North America" & nb==com$nb & da==com$da]
  richness.sa<-richness[seed.continent=="South America" & nb==com$nb & da==com$da]
  richness.na<-merge(hexagon, richness.na, by.x="seqnum", by.y="global_id", all=T)
  richness.sa<-merge(hexagon, richness.sa, by.x="seqnum", by.y="global_id", all=T)
  richness.na[which(is.na(richness.na$N.species)), "N.species"]<-0
  richness.sa[which(is.na(richness.sa$N.species)), "N.species"]<-0
  median.v<-median(c(richness.na[which(richness.na$N.species>0),]$N.species, 
                     richness.sa[which(richness.sa$N.species>0),]$N.species))
  
  
  range.v<-range(c(richness.na$N.species, richness.sa$N.species))
  p.na<-ggplot()+ 
    geom_sf(data=hexagon, fill=NA, color="lightgrey")+
    geom_sf(data=richness.na,  aes(fill=N.species),
            color=NA, linewidth=0.1) +
    labs(fill="Number of species")+
    scale_fill_gradient2(low=color_low,
                         mid=color_mid,
                         high=color_high,
                         midpoint = median.v,
                         limits=range.v)+
    coord_sf(crs=map_crs)+
    map_theme
  
  
  p.sa<-ggplot()+ 
    geom_sf(data=hexagon, fill=NA, color="lightgrey")+
    geom_sf(data=richness.sa,  aes(fill=N.species),
            color=NA, linewidth=0.1) +
    labs(fill="Number of species")+
    scale_fill_gradient2(low=color_low,
                         mid=color_mid,
                         high=color_high,
                         midpoint = median.v,
                         limits=range.v)+
    coord_sf(crs=map_crs)+
    map_theme
  
  ppp<-p.na+p.sa+plot_annotation(
    title = sprintf('NA origin (left) and SA origin (right), %s %s', com$nb, com$da),
    tag_levels = 'A'
  )
  ggsave(ppp, filename=sprintf("../Figures/Richness/%s.%s.png", com$nb, com$da), width=10, height=8)
}

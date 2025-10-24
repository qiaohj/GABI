library(data.table)
library(ggplot2)
library(sf)
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
  hexagon<-hexagon[which(hexagon$seqnum %in% centroids$global_id),]
  #plot(hexagon$geometry)
  centroids<-st_centroid(hexagon)
  mammals<-st_read("../Shape/IUCN/MAMMALS/MAMMALS_TERRESTRIAL_ONLY.shp")
  species<-unique(mammals$binomial)
  dis.list<-list()
  for (i in c(276:length(species))){
    print(paste(i, length(species)))
    sp<-species[i]
    item<-mammals[which(mammals$binomial ==sp),]
    item<-item[which(item$presence %in% c(1)),]
    item<-item[which(item$origin %in% c(1, 2, 3)),]
    item<-item[which(item$seasonal %in% c(1, 2)),]
    if (nrow(item)==0){
      next()
    }
    index<-st_contains(item, centroids)
    index<-unique(unlist(index))
    if (F){
      plot(hexagon$geometry)
      plot(item$geometry, add=T)
      plot(centroids[index,]$geometry, add=T)
    }
    v_items<-data.table(centroids[index,])
    
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


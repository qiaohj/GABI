library(data.table)
library(sf)
library(RSQLite)
library(DBI)

setwd("/media/huijieqiao/WD22T_11/continental_movement/Script")
conn<-dbConnect(RSQLite::SQLite(), "../Configuration/configuration.sqlite")
pr<-data.table(dbReadTable(conn, "pr"))
tasmax<-data.table(dbReadTable(conn, "tasmax"))
tasmin<-data.table(dbReadTable(conn, "tasmin"))
dbDisconnect(conn)
pr$var<-"pr"
tasmax$var<-"tasmax"
tasmin$var<-"tasmin"
all_v<-rbindlist(list(pr, tasmax, tasmin))
all_v_last<-all_v[year==0]
shpfname = "../Data/Shape/isea3h8/isea3h8_sf.shp"
hexagon<-read_sf(shpfname)
hexagon<-hexagon[which(hexagon$seqnum %in% all_v_last$global_id),]
plot(hexagon$geometry)
centroids<-st_centroid(hexagon)
all_v_last<-merge(centroids, data.frame(all_v_last), by.y="global_id", by.x="seqnum")
sf_use_s2(FALSE)

vessel <- sf::st_read(dsn = "../Data/Shape/IUCN/BIRDS/BOTW.gdb", layer = "All_Species")
i=1
species<-unique(vessel$SCINAME)
nb_full<-list()
for (i in c(1:length(species))){
  print(paste(i, length(species)))
  sp<-species[i]
  item<-vessel[which(vessel$SCINAME ==sp),]
  item<-item[which(st_geometry_type(item)=="MULTIPOLYGON"),]
  item<-item[which(item$PRESENCE %in% c(1)),]
  item<-item[which(item$ORIGIN %in% c(1, 2, 3)),]
  item<-item[which(item$SEASONAL %in% c(1, 2)),]
  if (nrow(item)==0){
    next()
  }
  index<-st_contains(item, all_v_last)
  index<-unique(unlist(index))
  if (F){
    plot(hexagon$geometry)
    plot(item$geometry, add=T)
    plot(all_v_last[index,]$geometry, add=T)
  }
  v_items<-data.table(all_v_last[index,])
  if (nrow(v_items)>0){
    item_df<-v_items[, .(species=sp,
                         N_CELLS=length(unique(seqnum)),
                         min=min(v),
                         max=max(v),
                         sd=sd(v),
                         mean=mean(v),
                         q1=quantile(v, 0.25),
                         q3=quantile(v, 0.75)),
                     by=list(var)]
    nb_full[[length(nb_full)+1]]<-item_df
  }
}
nb_full_df<-rbindlist(nb_full)

saveRDS(nb_full_df, "../Data/IUCN_NB/Birds.rda")



mammals<-st_read("../Data/Shape/IUCN/MAMMALS/MAMMALS_TERRESTRIAL_ONLY.shp")

i=1
species<-unique(mammals$binomial)
nb_full<-list()
for (i in c(1:length(species))){
  print(paste(i, length(species)))
  sp<-species[i]
  item<-mammals[which(mammals$binomial ==sp),]
  item<-item[which(item$presence %in% c(1)),]
  item<-item[which(item$origin %in% c(1, 2, 3)),]
  item<-item[which(item$seasonal %in% c(1, 2)),]
  if (nrow(item)==0){
    next()
  }
  index<-st_contains(item, all_v_last)
  index<-unique(unlist(index))
  if (F){
    plot(hexagon$geometry)
    plot(item$geometry, add=T)
    plot(all_v_last[index,]$geometry, add=T)
  }
  v_items<-data.table(all_v_last[index,])
  if (nrow(v_items)>0){
    item_df<-v_items[, .(species=sp,
                         N_CELLS=length(unique(seqnum)),
                         min=min(v),
                         max=max(v),
                         sd=sd(v),
                         mean=mean(v),
                         q1=quantile(v, 0.25),
                         q3=quantile(v, 0.75)),
                     by=list(var)]
    nb_full[[length(nb_full)+1]]<-item_df
  }
}
nb_full_df<-rbindlist(nb_full)
saveRDS(nb_full_df, "../Data/IUCN_NB/Mammals.rda")






AMPHIBIANS<-st_read("../Data/Shape/IUCN/AMPHIBIANS/AMPHIBIANS.shp")

i=1
species<-unique(AMPHIBIANS$sci_name)
nb_full<-list()
for (i in c(1:length(species))){
  print(paste(i, length(species)))
  sp<-species[i]
  item<-AMPHIBIANS[which(AMPHIBIANS$sci_name ==sp),]
  item<-item[which(item$presence %in% c(1)),]
  item<-item[which(item$origin %in% c(1, 2, 3)),]
  item<-item[which(item$seasonal %in% c(1, 2)),]
  if (nrow(item)==0){
    next()
  }
  index<-st_contains(item, all_v_last)
  index<-unique(unlist(index))
  if (F){
    plot(hexagon$geometry)
    plot(item$geometry, add=T)
    plot(all_v_last[index,]$geometry, add=T)
  }
  v_items<-data.table(all_v_last[index,])
  if (nrow(v_items)>0){
    item_df<-v_items[, .(species=sp,
                         N_CELLS=length(unique(seqnum)),
                         min=min(v),
                         max=max(v),
                         sd=sd(v),
                         mean=mean(v),
                         q1=quantile(v, 0.25),
                         q3=quantile(v, 0.75)),
                     by=list(var)]
    nb_full[[length(nb_full)+1]]<-item_df
  }
}
nb_full_df<-rbindlist(nb_full)
saveRDS(nb_full_df, "../Data/IUCN_NB/Amphibians.rda")



REPTILES1<-st_read("../Data/Shape/IUCN/REPTILES/REPTILES_PART1.shp")
REPTILES2<-st_read("../Data/Shape/IUCN/REPTILES/REPTILES_PART2.shp")
REPTILES1$OBJECTID<-NULL
REPTILES<-rbind(REPTILES1, REPTILES2)
i=1
species<-unique(REPTILES$sci_name)
nb_full<-list()
for (i in c(1:length(species))){
  print(paste(i, length(species)))
  sp<-species[i]
  item<-REPTILES[which(REPTILES$sci_name ==sp),]
  item<-item[which(item$presence %in% c(1)),]
  item<-item[which(item$origin %in% c(1, 2, 3)),]
  item<-item[which(item$seasonal %in% c(1, 2)),]
  if (nrow(item)==0){
    next()
  }
  index<-st_contains(item, all_v_last)
  index<-unique(unlist(index))
  if (F){
    plot(hexagon$geometry)
    plot(item$geometry, add=T)
    plot(all_v_last[index,]$geometry, add=T)
  }
  v_items<-data.table(all_v_last[index,])
  if (nrow(v_items)>0){
    item_df<-v_items[, .(species=sp,
                         N_CELLS=length(unique(seqnum)),
                         min=min(v),
                         max=max(v),
                         sd=sd(v),
                         mean=mean(v),
                         q1=quantile(v, 0.25),
                         q3=quantile(v, 0.75)),
                     by=list(var)]
    nb_full[[length(nb_full)+1]]<-item_df
  }
}
nb_full_df<-rbindlist(nb_full)
saveRDS(nb_full_df, "../Data/IUCN_NB/Reptiles.rda")

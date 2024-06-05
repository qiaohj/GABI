library(data.table)
library(sf)
library(RSQLite)
library(DBI)

setwd("/media/huijieqiao/WD22T_11/GABI/Script")
#for north and south america only.
label<-"N_S_America"
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
#plot(hexagon$geometry)
centroids<-st_centroid(hexagon)
all_v_last<-merge(centroids, data.frame(all_v_last), by.y="global_id", by.x="seqnum")
sf_use_s2(FALSE)

#for all the world
library(data.table)
library(sf)
library(RSQLite)
library(DBI)

setwd("/media/huijieqiao/WD22T_11/GABI/Script")
label<-"World"
conn<-dbConnect(RSQLite::SQLite(), "../Configuration/configuration_continent.sqlite")
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
#plot(hexagon$geometry)
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
                         q3=quantile(v, 0.75),
                         q01=quantile(v, 0.01),
                         q99=quantile(v, 0.99)),
                     by=list(var)]
    nb_full[[length(nb_full)+1]]<-item_df
  }
}
nb_full_df<-rbindlist(nb_full)

saveRDS(nb_full_df, sprintf("../Data/IUCN_NB/%s/Birds.rda", label))



mammals<-st_read("../Data/Shape/IUCN/MAMMALS/MAMMALS_TERRESTRIAL_ONLY.shp")

i=1
species<-unique(mammals$binomial)
nb_full<-list()
#all_v_last<-all_v_last[which(!all_v_last$seqnum %in% c(8920, 8921, 9002)),]

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
                         q3=quantile(v, 0.75),
                         q01=quantile(v, 0.01),
                         q99=quantile(v, 0.99)),
                     by=list(var)]
    nb_full[[length(nb_full)+1]]<-item_df
  }
}
nb_full_df<-rbindlist(nb_full)

saveRDS(nb_full_df, sprintf("../Data/IUCN_NB/%s/Mammals.rda", label))





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
saveRDS(nb_full_df, sprintf("../Data/IUCN_NB/%s/Amphibians.rda", label))



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
saveRDS(nb_full_df, sprintf("../Data/IUCN_NB/%s/Reptiles.rda", label))


if (F){
  label<-"World"
  groups<-c("Birds", "Mammals", "Reptiles", "Amphibians")
  dfs<-list()
  for (g in groups){
    df<-readRDS(sprintf("../Data/IUCN_NB/%s/%s.rda",label,  g))
    df$group<-g
    dfs[[g]]<-df
  }
  dfs<-rbindlist(dfs)
  #dfs<-dfs[N_CELLS>3]
  dfs$iqr<-dfs$q3 - dfs$q1
  dfs$range_iqr<-dfs$q3 + dfs$iqr*1.5 - (dfs$q1 - dfs$iqr*1.5)
  dfs$range_3sd<-dfs$sd * 6
  dfs$range_min_max<-dfs$max - dfs$min
  
  pr_item<-dfs[var=="pr"]
  tasmax_item<-dfs[var=="tasmax"]
  tasmin_item<-dfs[var=="tasmin"]
  
  pr_nb<-pr_item[, c("species", "N_CELLS", "group", "range_iqr", "range_3sd", "range_min_max")]
  pr_nb$type<-"pr"
  tas<-merge(tasmax_item, tasmin_item, by=c("species", "N_CELLS", "group"))
  tas$range_3sd<-tas$mean.x + tas$sd.x * 3  - (tas$mean.y - tas$sd.y * 3)
  tas$range_iqr<-tas$q3.x + tas$iqr.x * 1.5 - (tas$q1.y - tas$iqr.y * 1.5)
  tas$range_min_max<-tas$max.x - tas$min.y
  tas_nb<-tas[, c("species", "N_CELLS", "group", "range_iqr", "range_3sd", "range_min_max")]
  tas_nb$type<-"tm"
  nb<-rbindlist(list(pr_nb, tas_nb))
  nb[, .(N=.N), by=list(group, type)]
  p1<-ggplot(nb)+
    geom_histogram(aes(x=range_iqr/100), bins=100)+
    geom_histogram(data=nb, aes(x=range_3sd/100), fill="red", alpha=0.5, bins=100)+
    geom_histogram(data=nb, aes(x=range_min_max/100), fill="blue", alpha=0.3, bins=100)+
    facet_grid(group~type, scale="free")+
    scale_x_log10()+
    theme_bw()
  
  p2<-ggplot(nb)+
    geom_density(aes(x=range_iqr/100))+
    geom_density(data=nb, aes(x=range_3sd/100), color="red", alpha=0.5)+
    geom_density(data=nb, aes(x=range_min_max/100), color="blue", alpha=0.3)+
    facet_grid(group~type, scale="free")+
    scale_x_log10()+
    theme_bw()
  ggpubr::ggarrange(p1, p2, nrow=2)
  
  nb_table<-merge(pr_nb, tas_nb, by=c("species", "N_CELLS", "group"))
  colnames(nb_table)<-c("species", "N_CELLS", "group", "range_iqr.pr", "range_3sd.pr",
                        "range_min_max.pr", "type.pr", "range_iqr.tm" ,
                        "range_3sd.tm", "range_min_max.tm", "type.tm")
  
  ggplot(nb_table) + geom_point(aes(x=range_min_max.pr/100, y=range_min_max.tm/100, color=group))+
    facet_wrap(~group)+
    theme_bw()+
    scale_x_log10()+
    scale_y_log10()
  saveRDS(nb, "../Data/IUCN_NB/iucn_nb.rda")
  saveRDS(nb_table, "../Data/IUCN_NB/iucn_nb_table.rda")
  range(nb$range_3sd)
  dfs[species %in% nb[range_iqr==0]$species]
  range(nb$range_iqr)
  range(nb$range_min_max)
  
  #compare the world and n_s
  df1<-readRDS(sprintf("../Data/IUCN_NB/%s/%s.rda","N_S_America",  "Mammals"))
  df1$group<-"Mammals"
  df1$area<-"N_S_America"
  
  df2<-readRDS(sprintf("../Data/IUCN_NB/%s/%s.rda","World",  "Mammals"))
  df2$group<-"Mammals"
  df2$area<-"World"
  
  dfs<-rbindlist(list(df1, df2))
  dfs<-dfs[N_CELLS>1]
  dfs$range_min_max<-dfs$max - dfs$min
  dfs$range_q01_q99<-dfs$q99 - dfs$q01
  pr_item<-dfs[var=="pr"]
  tasmax_item<-dfs[var=="tasmax"]
  tasmin_item<-dfs[var=="tasmin"]
  
  pr_nb<-pr_item[, c("species", "N_CELLS", "group", "area", "range_min_max", "range_q01_q99")]
  pr_nb$type<-"pr"
  tas<-merge(tasmax_item, tasmin_item, by=c("species", "N_CELLS", "area", "group"))
  tas$range_min_max<-tas$max.x - tas$min.y
  tas$range_q01_q99<-tas$q99.x - tas$q01.y
  tas_nb<-tas[, c("species", "N_CELLS", "group", "area", "range_min_max", "range_q01_q99")]
  tas_nb$type<-"tm"
  nb<-rbindlist(list(pr_nb, tas_nb))
  nb[, .(N_species=length(unique(species))), by=list(group, area)]
  max(nb[type=="pr"]$range_min_max)
  table(nb[round(range_min_max)==2497.92]$area)
  View(pr_item[species %in% nb[range_min_max==2497.92]$species])
  ggplot(nb)+geom_histogram(aes(x=range_q01_q99), bins=50)+
    facet_grid(area~type, scale="free")
  ggplot(nb)+geom_density(aes(x=range_q01_q99, color=area))+
    facet_wrap(~type, scale="free", nrow=2)
  nb_n<-nb[,.(N=.N),by=list(range_min_max)]
  nb_n[N==max(N)]
  mammals<-st_read("../Data/Shape/IUCN/MAMMALS/MAMMALS_TERRESTRIAL_ONLY.shp")
  
  mammals_x<-mammals[which(mammals$binomial %in% nb[range_min_max==259688]$species),]
  plot(mammals_x$geometry)
  
  #hexagon_low_pr<-hexagon[which(hexagon$seqnum %in% 
  #                                all_v_last[which(round(all_v_last$v)==139),]$seqnum),]
  
  hexagon_low_pr<-hexagon[which(hexagon$seqnum %in% c(8920, 8921, 9002)),]
  hexagon_high_pr<-hexagon[which(hexagon$seqnum %in% 
                                  all_v_last[which(round(all_v_last$v)==2736),]$seqnum),]
  
  all_v_last_df<-data.frame(all_v_last)
  all_v_last_df$geometry<-NULL
  coord<-data.frame(st_coordinates(all_v_last))
  all_v_last_df$lon<-coord$X
  all_v_last_df$lat<-coord$Y
  
  all_v_last_df<-all_v_last_df[which(all_v_last_df$lon>(-160)),]
 
  hexagonxx<-merge(hexagon, all_v_last_df[which(all_v_last_df$var=="pr"),], by="seqnum")
  hexagonxx<-hexagonxx[which(!(hexagonxx$seqnum %in% c(32634, 62227, 62388, 62790, 
                                                      63031, 63272))),]
  hexagonxx_dt<-data.table(hexagonxx)
  ggplot(hexagonxx[which((hexagonxx$lon>=-90) &
                     (hexagonxx$lon<=-60) &
                       (hexagonxx$lat>=-10)&
                       (hexagonxx$lat<=15)),])+
    geom_sf(aes(fill=v))+
    geom_sf(data=hexagon_low_pr, color="green", fill=NA)+
    geom_sf(data=hexagon_high_pr, color="red", fill=NA)
  #write_sf(hexagonxx, "~/Downloads/xxx.shp")
}


library(dggridR)
library(terra)
library(sf)
library(dplyr)
library(furrr)
library(RSQLite)
library(reshape2)
library(data.table)
library(ggplot2)

setwd("/media/huijieqiao/Butterfly/GABI/GABI")
find_connected_hexagon <- function(hexagons){
  
  neighbors_list <- tibble(index=1:nrow(hexagons)) %>% 
    mutate(neighbors =future_map(index,function(index){
      current_hexagon <- hexagons[index, ]
      hexagons$seqnum[st_touches( current_hexagon,hexagons) %>% unlist()]
    })
    )
  
  neighbors_list
  
}
if (F){
  
  dggs<-dgconstruct(res = 8)
  dg_closest_res(dggs, col="spacing_km", val=100)
  earth<-dgearthgrid(dggs)
  
  shpfname = "../Data/Shape/isea3h8/isea3h8_sf.shp"
  write_sf(earth, shpfname)
  neighbors<-find_connected_hexagon(earth)
  
}
shpfname = "../Data/Shape/isea3h8/isea3h8_sf.shp"
hexagon<-read_sf(shpfname)
hexa_coords<-st_centroid(hexagon)

continents<-read_sf("../Data/Shape/continents/continent.shp")
america<-continents[which(continents$id %in% c(2, 5)),]
st_crs(america)<-st_crs(hexagon)
america_items<-st_cast(america, "POLYGON")
america_items$area<-as.numeric(st_area(america_items))
coords<-st_coordinates(st_centroid(america_items))
america_items$lon_centroid<-coords[,1]
america_items$lat_centroid<-coords[,2]
quantile(america_items$area, 0.99)
america_items<-america_items[which(america_items$area>5e12 |
                                     between(america_items$lat_centroid, -30, 30)),]
plot(america_items$geometry)
n_index<-st_intersects(america_items, hexagon)

hexagon_ns<-hexagon
hexagon_ns$continent<-"World"
#hexagon_ns[which(hexagon_ns$seqnum %in% 
#                   c(7865,7864,8520,7782,8932,9581,9582,9583,7211,33535)), 
#]$continent<-"Bridge"
for (i in c(1:length(n_index))){
  print(paste(i, length(n_index)))
  index<-n_index[[i]]
  if (length(index)==0){
    next()
  }
  america_item<-america_items[i,]
  hexagon_ns[index, ]$continent<-america_item$CONTINENT
}
hexagon_ns_raw<-hexagon_ns
hexagon_ns_raw<-hexagon_ns_raw[which(hexagon_ns_raw$continent!="World"),]
hexagon_ns_raw[which(hexagon_ns_raw$seqnum %in% c(9004, 9086, 9168)), ]$continent<-"South America"

bridges<-read_sf("../Data/Shape/bridges/bridges.shp")
plot(america_items$geometry)
plot(bridges$geometry, fill="red", add=T)
bridge_index<-st_intersects(bridges, hexagon_ns_raw)
for (i in c(1:length(bridge_index))){
  bridge_item<-bridges[i,]
  hexagon_ns_raw[bridge_index[[i]],]$continent<-bridge_item$bridge_nam
}
hexagon_ns_raw[which(hexagon_ns_raw$seqnum %in% c(34100, 34747)), ]$continent<-"remove"
hexagon_ns_raw<-hexagon_ns_raw[which(hexagon_ns_raw$continent!="remove"),]
hexagon_ns_raw[which(hexagon_ns_raw$seqnum %in% c(9004, 9086, 9168, 9416)), ]$continent<-"South America"
if (F){
  ggplot(hexagon_ns_raw)+geom_sf(aes(fill=continent))
  
  write_sf(hexagon_ns_raw, "../Data/Shape/isea3h8/N_S_America.shp")
}

#plot(hexagon_ns$geometry)
coords<-st_coordinates(st_centroid(hexagon_ns_raw))
hexagon_ns_raw$lon<-coords[,1]
hexagon_ns_raw$lat<-coords[,2]
write_sf(hexagon_ns_raw, "../Data/Shape/isea3h8/N_S_America.shp")
table(hexagon_ns_raw$continent)

hexagon_ns_raw<-read_sf("../Data/Shape/isea3h8/N_S_America.shp")
hexagon_ns_raw<-hexagon_ns_raw[which(!hexagon_ns_raw$seqnum %in% 
                                       c(7292, 7946,8027,8026,8108,7942,33048,33292,33373,33454,
                                         33045,33126,33289,33452,33534)),]
write_sf(hexagon_ns_raw, "../Data/Shape/isea3h8/N_S_America.shp")

hexagon<-read_sf(shpfname)
continents<-read_sf("../Data/Shape/continents/continent.shp")
st_crs(continents)<-st_crs(hexagon)
n_index<-st_intersects(continents, hexagon)
hexagon$continent<-0
for (i in c(1:length(n_index))){
  hexagon[n_index[[i]], ]$continent<-i
}
hexagon<-hexagon[which(hexagon$continent!=0),]
ggplot(hexagon)+geom_sf(aes(color=factor(continent)))
coords<-st_coordinates(st_centroid(hexagon))
hexagon$lon<-coords[,1]
hexagon$lat<-coords[,2]
write_sf(hexagon, "../Data/Shape/isea3h8/Continent.shp")

if (F){
  xx<-read_sf("../Data/Shape/isea3h8/N_S_America.shp")
  table(xx$continent)
  base_db<-"../Data/Temp/env_Hadley3D.sqlite"
  envdb <- dbConnect(RSQLite::SQLite(), base_db)
  
  v_prcp<-dbReadTable(envdb, "Debiased_Maximum_Monthly_Precipitation")
  v_tmax<-dbReadTable(envdb, "Debiased_Maximum_Monthly_Temperature")
  v_tmin<-dbReadTable(envdb, "Debiased_Minimum_Monthly_Temperature")
  dbDisconnect(envdb)
}

vars<-c("pr", "tasmax", "tasmin")
n_s_america<-read_sf("../Data/Shape/isea3h8/N_S_America.shp")
continent<-read_sf("../Data/Shape/isea3h8/Continent.shp")
conn_ns_america<-dbConnect(RSQLite::SQLite(), "../Configuration/configuration.sqlite")
conn_continent<-dbConnect(RSQLite::SQLite(), "../Configuration/configuration_continent.sqlite")
for (v in vars){
  print(v)
  tif<-rast(sprintf("../Data/Raster/Fine.1x1/%s.tif", v))
  values<-extract(tif, data.frame(lon=n_s_america$lon, lat=n_s_america$lat))
  template<-data.frame(global_id=as.integer(n_s_america$seqnum), v=-9999, year=-9999)
  full_df<-list()
  for (y in names(values)){
    if (y=="ID"){
      next()
    }
    item<-template
    item$v<-values[, y]
    cyear<-as.integer(gsub("y", "", y))
    item$year<-cyear
    if (cyear>=3100){
      item<-item[which(item$global_id %in% 
                         n_s_america[which(n_s_america$continent %in% c("North America", "South America")),]$seqnum
                       ),]
    }
    full_df[[length(full_df)+1]]<-item
  }
  full_df<-rbindlist(full_df)
  dbWriteTable(conn_ns_america, v, full_df, overwrite=T)
  
  values<-extract(tif, data.frame(lon=continent$lon, lat=continent$lat))
  template<-data.frame(global_id=as.integer(continent$seqnum), v=-9999, year=-9999)
  full_df<-list()
  for (y in names(values)){
    if (y=="ID"){
      next()
    }
    item<-template
    item$v<-values[, y]
    cyear<-as.integer(gsub("y", "", y))
    item$year<-cyear
    
    full_df[[length(full_df)+1]]<-item
  }
  full_df<-rbindlist(full_df)
  dbWriteTable(conn_continent, v, full_df, overwrite=T)
}
dbDisconnect(conn_continent)
dbDisconnect(conn_ns_america)



hexagon_ns<-read_sf("../Shape/isea3h8/N_S_America.shp")
centroids<-st_centroid(hexagon_ns)
plot(centroids$geometry)
dist<-st_distance(centroids)
colnames(dist)<-centroids$seqnum
rownames(dist)<-centroids$seqnum
dist_df<-as.data.frame(as.table(dist))
dist_df$N<-round(as.numeric(dist_df$Freq)/100000)
dist_df<-dist_df[which(dist_df$N==1),]
dist_df$Freq<-NULL
colnames(dist_df)<-c("i", "j", "dist")
dist_df$i<-as.integer(as.character(dist_df$i))
dist_df$j<-as.integer(as.character(dist_df$j))
dist_df$dist<-as.integer(dist_df$dist)
dist_df<-data.table(dist_df)
id=3121
all_dist<-list()
for (id in unique(c(dist_df$i, dist_df$j))){
  print(id)
  dist_item<-dist_df[i==id]
  for (distance in c(2:4)){
    dist_item2<-dist_df[(i %in% dist_item[dist==(distance-1)]$j)]
    dist_item2<-dist_item2[! j %in% c(id, dist_item$j)]
    dist_item2$i<-id
    dist_item2$dist<-distance
    dist_item2<-unique(dist_item2)
    dist_item<-rbind(dist_item, dist_item2)
  
  }
  all_dist[[length(all_dist)+1]]<-dist_item
  if (F){
    neighbors<-dist_item
    neighbors_po<-merge(hexagon_ns, neighbors, by.x="seqnum", by.y="j")
    ggplot(hexagon_ns_pr)+geom_sf()+
      geom_sf(data=neighbors_po, aes(fill=factor(dist)))
    
    ggplot()+
      geom_sf(data=neighbors_po, aes(fill=factor(dist)))
    
  }
}
all_dist<-rbindlist(all_dist)
all_dist$i<-as.integer(as.character(all_dist$i))
all_dist$j<-as.integer(as.character(all_dist$j))
all_dist$dist<-as.integer(all_dist$dist)

conn<-dbConnect(RSQLite::SQLite(), "../Configuration/env_Hadley3D.sqlite")
dist<-data.table(dbReadTable(conn, "distances"))
dbDisconnect(conn)
hexagon_ns<-read_sf("../Shape/isea3h8/N_S_America.shp")
dist<-dist[i %in% hexagon_ns$seqnum | j %in% hexagon_ns$seqnum]
dist[i==7130 & dist==2]

conn<-dbConnect(RSQLite::SQLite(), "../Configuration/null.sqlite")
dbWriteTable(conn, "distances", dist, overwrite=T)
dbDisconnect(conn)

#check the data
hexagon<-read_sf(shpfname)
conn<-dbConnect(RSQLite::SQLite(), "../Configuration/configuration.sqlite")
pr<-data.table(dbReadTable(conn, "pr"))
tasmax<-data.table(dbReadTable(conn, "tasmax"))
tasmin<-data.table(dbReadTable(conn, "tasmin"))
dist<-data.table(dbReadTable(conn, "distances"))
dbDisconnect(conn)

#before 3.1My 
pr_item<-pr[year==1800]
hexagon_pr<-merge(hexagon, pr_item, by.x="seqnum", by.y="global_id")
ggplot(hexagon_pr)+
  geom_sf(aes(fill=v))
#after 3.1My 
pr_item<-pr[year==0]
hexagon_pr<-merge(hexagon, pr_item, by.x="seqnum", by.y="global_id")
ggplot(hexagon_pr)+
  geom_sf(aes(fill=v))
pr$var<-"pr"
tasmax$var<-"tasmax"
tasmin$var<-"tasmin"

#check the overall pattern
all_v<-rbindlist(list(pr, tasmax, tasmin))
all_v_se<-all_v[,.(v=mean(v)), by=list(year, var)]
ggplot(all_v_se)+geom_line(aes(x=year * -1, y=v))+
  facet_wrap(~var, nrow=3, scale="free")
#check neighber
dist[i>5000]
id<-6585
neighbors<-dist[i==id]
table(neighbors$j)
neighbors_po<-merge(hexagon, neighbors, by.x="seqnum", by.y="j")
ggplot(hexagon_pr)+geom_sf()+
  geom_sf(data=neighbors_po, aes(fill=factor(dist)))

ggplot()+
  geom_sf(data=neighbors_po, aes(fill=factor(dist)))


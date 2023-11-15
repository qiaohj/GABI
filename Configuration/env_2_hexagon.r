library(dggridR)
library(terra)
library(sf)
library(dplyr)
library(furrr)
library(RSQLite)
library(reshape2)
library(data.table)
library(ggplot2)

setwd("/media/huijieqiao/WD22T_11/continental_movement/Script")
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
coods<-st_centroid(hexagon)

continents<-read_sf("../Data/Shape/continents/continent.shp")
america<-continents[which(continents$id %in% c(2, 5)),]
st_crs(america)<-st_crs(hexagon)
america_items<-st_cast(america, "POLYGON")
america_items$area<-as.numeric(st_area(america_items))
quantile(america_items$area, 0.99)
america_items<-america_items[which(america_items$area>5e12),]
n_index<-st_contains(america_items, coods)
hexagon$continent<-"World"
hexagon[n_index[[2]], ]$continent<-"South America"
hexagon[c(n_index[[1]], 35236), ]$continent<-"North America"
hexagon[c(8912,8994,9075,35152,35234,35235,35236,35316),]$continent<-"Bridge"
hexagon<-hexagon[which(hexagon$continent!="World"),]
hexagon<-hexagon[which(!hexagon$seqnum %in% c(6200, 6668, 6669)),]
#plot(hexagon$geometry)
coords<-st_coordinates(st_centroid(hexagon))
hexagon$lon<-coords[,1]
hexagon$lat<-coords[,2]
write_sf(hexagon, "../Data/Shape/isea3h8/N_S_America.shp")

if (F){
  base_db<-"../Data/Temp/env_Hadley3D.sqlite"
  envdb <- dbConnect(RSQLite::SQLite(), base_db)
  
  v_prcp<-dbReadTable(envdb, "Debiased_Maximum_Monthly_Precipitation")
  v_tmax<-dbReadTable(envdb, "Debiased_Maximum_Monthly_Temperature")
  v_tmin<-dbReadTable(envdb, "Debiased_Minimum_Monthly_Temperature")
  dbDisconnect(envdb)
}

vars<-c("pr", "tasmax", "tasmin")
conn<-dbConnect(RSQLite::SQLite(), "../Configuration/configuration.sqlite")
for (v in vars){
  print(v)
  tif<-rast(sprintf("../Data/Raster/Fine.1x1/%s.tif", v))
  values<-extract(tif, data.frame(lon=hexagon$lon, lat=hexagon$lat))
  template<-data.frame(global_id=as.integer(hexagon$seqnum), v=-9999, year=-9999)
  full_df<-list()
  for (y in names(values)){
    if (y=="ID"){
      next()
    }
    item<-template
    item$v<-as.integer(round(values[, y]*100))
    cyear<-as.integer(gsub("y", "", y))
    item$year<-cyear
    if (cyear>=3100){
      item<-item[which(item$global_id %in% 
                         hexagon[which(hexagon$continent %in% c("North America", "South America")),]$seqnum
                       ),]
    }
    full_df[[length(full_df)+1]]<-item
  }
  full_df<-rbindlist(full_df)
  dbWriteTable(conn, v, full_df, overwrite=T)
}
dbDisconnect(conn)

centroids<-st_centroid(hexagon)
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
    neighbors_po<-merge(hexagon, neighbors, by.x="seqnum", by.y="j")
    ggplot(hexagon_pr)+geom_sf()+
      geom_sf(data=neighbors_po, aes(fill=factor(dist)))
    
    ggplot()+
      geom_sf(data=neighbors_po, aes(fill=factor(dist)))
    
  }
}
all_dist<-rbindlist(all_dist)
all_dist$i<-as.integer(as.character(all_dist$i))
all_dist$j<-as.integer(as.character(all_dist$j))
all_dist$dist<-as.integer(all_dist$dist)
conn<-dbConnect(RSQLite::SQLite(), "../Configuration/configuration.sqlite")
dbWriteTable(conn, "distances", all_dist, overwrite=T)
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
pr_item<-pr[year==3100]
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
ggplot(all_v_se)+geom_line(aes(x=year * -1, y=v/100))+
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

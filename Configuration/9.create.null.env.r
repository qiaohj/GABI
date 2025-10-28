library(data.table)
library(sf)
library(RSQLite)
library(DBI)
library(ggplot2)
library(terra)
setwd("/media/huijieqiao/Butterfly/GABI/GABI")
conn<-dbConnect(RSQLite::SQLite(), "../Configuration/configuration.sqlite")
pr<-data.table(dbReadTable(conn, "pr"))
tasmax<-data.table(dbReadTable(conn, "tasmax"))
tasmin<-data.table(dbReadTable(conn, "tasmin"))
distances<-data.table(dbReadTable(conn, "distances"))
mask<-data.table(dbReadTable(conn, "mask"))
environments<-data.table(dbReadTable(conn, "environments"))
dbDisconnect(conn)



first.pr<-pr[year==0]
first.tasmax<-pr[year==0]
first.tasmin<-pr[year==0]

hexagon<-read_sf("../Shape/isea3h8/N_S_America.shp")

if (F){
  plot(hexagon[which(hexagon$seqnum %in% first.pr$global_id),]$geometry)
}
vars<-c("pr", "tasmax", "tasmin")
for (v in vars){
  print(v)
  template<-pr[year==0]
  template$v<-NULL
  tif<-rast(sprintf("../Data/Raster/Fine.1x1/%s.tif", v))
  values<-extract(tif, data.frame(lon=hexagon$lon, lat=hexagon$lat))
  values<-data.table(global_id=hexagon$seqnum, v=values$y3600)
  if (v=="pr"){
    first.pr<-merge(template, values, by="global_id")
  }
  if (v=="tasmax"){
    first.tasmax<-merge(template, values, by="global_id")
  }
  if (v=="tasmin"){
    first.tasmin<-merge(template, values, by="global_id")
  }
  
  
}

first.tasmin<-first.tasmin[, c("global_id", "v", "year")]
first.tasmax<-first.tasmax[, c("global_id", "v", "year")]
first.pr<-first.pr[, c("global_id", "v", "year")]

plot(tasmin[year==1800]$v, first.tasmin[global_id %in% tasmin[year==1800]$global_id]$v)
pr.list<-list()
tasmax.list<-list()
tasmin.list<-list()

for (y in c(1800:0)){
  item.pr<-first.pr
  item.pr$year<-y
  pr.list[[length(pr.list)+1]]<-item.pr
  
  item.tasmax<-first.tasmax
  item.tasmax$year<-y
  tasmax.list[[length(tasmax.list)+1]]<-item.tasmax
  
  item.tasmin<-first.tasmin
  item.tasmin$year<-y
  tasmin.list[[length(tasmin.list)+1]]<-item.tasmin
}

null.pr<-rbindlist(pr.list)
null.tasmax<-rbindlist(tasmax.list)
null.tasmin<-rbindlist(tasmin.list)

base_db<-"../Configuration/null.sqlite"
mydb <- dbConnect(RSQLite::SQLite(), base_db)
dbWriteTable(mydb, "pr", null.pr, overwrite=T)
dbWriteTable(mydb, "tasmax", null.tasmax, overwrite=T)
dbWriteTable(mydb, "tasmin", null.tasmin, overwrite=T)
dbWriteTable(mydb, "distances", distances, overwrite=T)
dbWriteTable(mydb, "mask", mask, overwrite=T)
dbWriteTable(mydb, "environments", environments, overwrite=T)
dbDisconnect(mydb)



range(pr$year)



conn<-dbConnect(RSQLite::SQLite(), "../Configuration/conf.sqlite")
simulations<-data.table(dbReadTable(conn, "simulations"))
timeline<-data.table(dbReadTable(conn, "timeline"))
dbDisconnect(conn)
simulations$speciation_years<-10000
conn<-dbConnect(RSQLite::SQLite(), "../Configuration/conf.null.sqlite")
dbWriteTable(conn, "simulations", simulations, overwrite=T)
dbWriteTable(conn, "timeline", timeline, overwrite=T)
dbDisconnect(conn)

library(data.table)
library(sf)
library(ggplot2)
library(RSQLite)
library(DBI)
setwd("/media/huijieqiao/WD22T_11/GABI/Script")
ns<-read_sf("../Data/Shape/isea3h8/N_S_America.shp")
ggplot(ns)+geom_sf(aes(color=continent))

if (F){
  conn<-dbConnect(RSQLite::SQLite(), "../Configuration/configuration.sqlite")
  tasmin<-data.table(dbReadTable(conn, "tasmin"))
  dbDisconnect(conn)
  
  cells<-data.table(table(tasmin$year))
  cells$V1<-as.numeric(cells$V1)
  cells[between(V1, 1500, 1555)]
  #50: 1549 5449 : 3098
  #51: 1550 5187 : 3100
}

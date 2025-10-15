library(data.table)
library(sf)
library(RSQLite)
library(DBI)
library(ggplot2)
setwd("/media/huijieqiao/Butterfly/GABI/GABI")
conn<-dbConnect(RSQLite::SQLite(), "../Configuration/configuration.sqlite")
pr<-data.table(dbReadTable(conn, "pr"))
tasmax<-data.table(dbReadTable(conn, "tasmax"))
tasmin<-data.table(dbReadTable(conn, "tasmin"))
distances<-data.table(dbReadTable(conn, "distances"))
mask<-data.table(dbReadTable(conn, "mask"))
environments<-data.table(dbReadTable(conn, "environments"))
dbDisconnect(conn)

first.pr<-pr[year==1800]
first.tasmax<-tasmax[year==1800]
first.tasmin<-tasmin[year==1800]

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

library(data.table)
library(sf)
library(RSQLite)
library(DBI)
setwd("/media/huijieqiao/Butterfly/GABI/GABI")


folders<-list.dirs("../Results")
folders<-folders[2:length(folders)]
dt <- as.data.table(tstrsplit(basename(folders), "\\.", names = c("Seed_ID", "NB", "DA")))
dt$Seed_ID<-as.numeric(dt$Seed_ID)
seeds<-read_sf("../Shape/isea3h8/N_S_America.shp")

seeds<-data.table(Seed_ID=seeds$seqnum, Continent=seeds$continent)
table(seeds$Continent)
dt<-merge(dt, seeds, by="Seed_ID")

broad<-dt[NB=="BROAD" & DA=="GOOD"]
table(broad$Continent)



N<-dt[, .(N=.N), by=list(Seed_ID)]
N[Seed_ID %in% broad$Seed_ID & N==8]

conn<-dbConnect(RSQLite::SQLite(), "../Configuration/conf.2000.sqlite")
simulations.2000<-data.table(dbReadTable(conn, "simulations"))
dbDisconnect(conn)
sa.simulations<-simulations.2000[global_id %in% seeds[Continent=="South America"]$Seed_ID & is_run==1]
sa.simulations<-sa.simulations[!global_id %in% broad$Seed_ID]
sa.simulations<-unique(sa.simulations$global_id)
missing.N<-500-nrow(broad[Continent=="South America"])
rand_index<-sa.simulations[sample(length(sa.simulations), missing.N)]
conn<-dbConnect(RSQLite::SQLite(), "../Configuration/conf.sqlite")
simulations<-data.table(dbReadTable(conn, "simulations"))
simulations$is_run<-0
simulations[global_id %in% broad$Seed_ID, is_run:=1]
simulations[global_id %in% rand_index, is_run:=1]
dbWriteTable(conn, "simulations", simulations, overwrite=T)
dbDisconnect(conn)



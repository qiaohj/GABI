library(data.table)
library(sf)
library(RSQLite)
library(DBI)
library(ggplot2)
library(ggh4x)
library(ape)
library(phytools)
#library(ggtree)
library(phangorn)
setwd("/media/huijieqiao/Butterfly/GABI/GABI")
target<-"/media/huijieqiao/Butterfly/GABI/Results"
distribution<-readRDS("../Data/Tables/Final.Distribution.Unique.rda")

ll<-readRDS("../Data/Tables/cells.with.dist.rda")
local2<-data.table(global_id=as.numeric(ll$seqnum), 
                   continent=ll$continent,
                   lon=ll$lon, lat=ll$lat)
local2$lat_bin<-floor((local2$lat+0.5)/1)*1
distribution_ll<-merge(distribution, local2, by="global_id")
local2<-data.table(seed_id=as.numeric(ll$seqnum), 
                   seed_continent=ll$continent)
distribution_ll<-merge(distribution_ll, local2, by="seed_id")

seeds.all<-readRDS("../Data/Tables/random.seeds.threshold.by.nb.distance.rda")

rep.list<-list()
rep.list.all<-list()

rep.list2<-list()
rep.list.all2<-list()

threshold<-40
threshold2<-54
for (rrrr in c(1:100)){
  print(rrrr)
  seeds<-seeds.all[rep==rrrr]
  item<-distribution_ll[label %in% seeds$label]
  item$lat_threshold<-"LOW"
  item[lat>=threshold | lat<=(-1 * threshold), lat_threshold:="HIGH"]
  item$lat_threshold2<-"LOW"
  item[lat>=threshold | lat<=(-1 * threshold), lat_threshold2:="HIGH"]
  item[between(lat, threshold, threshold2), lat_threshold2:="MID-HIGH"]
  
  item.N<-item[, .(N_SP=length(sp_label)),
               by=list(lat_threshold, seed_continent, continent)]
  item.N$type<-ifelse(item.N$seed_continent==item.N$continent, "Native", "Immigrant")
  item.N.ALL<-item[, .(N_SP_ALL=length(sp_label)),
                   by=list(lat_threshold, continent)]
  item.N<-merge(item.N, item.N.ALL, by=c("lat_threshold", "continent"))
  item.N$rep<-rrrr
  setorderv(item.N, c("continent", "lat_threshold", "type"))
  rep.list.all[[rrrr]]<-item.N
  
  item.N<-item[, .(N_SP=length(sp_label)),
               by=list(lat_threshold, seed_continent, continent, nb, da)]
  item.N$type<-ifelse(item.N$seed_continent==item.N$continent, "Native", "Immigrant")
  item.N.ALL<-item[, .(N_SP_ALL=length(sp_label)),
                   by=list(lat_threshold, continent, nb, da)]
  item.N<-merge(item.N, item.N.ALL, by=c("lat_threshold", "continent", "nb", "da"))
  item.N$rep<-rrrr
  rep.list[[rrrr]]<-item.N
  
  
  item.N<-item[, .(N_SP=length(sp_label)),
               by=list(lat_threshold2, seed_continent, continent)]
  item.N$type<-ifelse(item.N$seed_continent==item.N$continent, "Native", "Immigrant")
  item.N.ALL<-item[, .(N_SP_ALL=length(sp_label)),
                   by=list(lat_threshold2, continent)]
  item.N<-merge(item.N, item.N.ALL, by=c("lat_threshold2", "continent"))
  item.N$rep<-rrrr
  setorderv(item.N, c("continent", "lat_threshold2", "type"))
  rep.list.all2[[rrrr]]<-item.N
  
  item.N<-item[, .(N_SP=length(sp_label)),
               by=list(lat_threshold2, seed_continent, continent, nb, da)]
  item.N$type<-ifelse(item.N$seed_continent==item.N$continent, "Native", "Immigrant")
  item.N.ALL<-item[, .(N_SP_ALL=length(sp_label)),
                   by=list(lat_threshold2, continent, nb, da)]
  item.N<-merge(item.N, item.N.ALL, by=c("lat_threshold2", "continent", "nb", "da"))
  item.N$rep<-rrrr
  rep.list2[[rrrr]]<-item.N
  
}

rep.df<-rbindlist(rep.list)
rep.df.all<-rbindlist(rep.list.all)
saveRDS(rep.df.all, "../Data/Tables/Lat.low.high.without.bridges.rda")
saveRDS(rep.df, "../Data/Tables/Lat.low.high.without.bridges.nb.da.rda")

rep.df2<-rbindlist(rep.list2)
rep.df.all2<-rbindlist(rep.list.all2)
saveRDS(rep.df.all2, "../Data/Tables/Lat.low.mid.high.without.bridges.rda")
saveRDS(rep.df2, "../Data/Tables/Lat.low.mid.high.without.bridges.nb.da.rda")
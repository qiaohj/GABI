library(data.table)
library(sf)
library(RSQLite)
library(DBI)
library(ggplot2)
library(ggh4x)
library(ape)
library(phytools)
library(ggtree)
library(phangorn)
setwd("/media/huijieqiao/Butterfly/GABI/GABI")
base_db<-"../Configuration/conf.sqlite"
mydb <- dbConnect(RSQLite::SQLite(), base_db)
simulations<-dbReadTable(mydb, "simulations")
dbDisconnect(mydb)
simulations<-data.table(simulations)
ll<-readRDS("../Data/Tables/cells.with.dist.rda")
local<-data.table(global_id=as.numeric(ll$seqnum), lon=ll$lon, lat=ll$lat)
local$lat_bin<-floor((local$lat+0.5)/1)*1
yearlist<-list()
yearsx<-seq(0, 1800, by=100)
yearsx<-as.character(yearsx)
for (year in yearsx){
  yearlist[[year]]<-list()
}
i=1
for (i in c(1:nrow(simulations))){
  
  item<-simulations[i]
  
  source<-sprintf("../Results/%d.%s.%s/species.richness.rda",
                  item$global_id, item$nb, item$da)
  print(paste(i, nrow(simulations), source))
  first<-data.table(year=1900, continent=item$continent, global_id=item$global_id,
                    N_SP=1, seed_id=item$global_id, nb=item$nb, da=item$da)
  
  if (!file.exists(source)){
    richness<-first
  }else{
    richness<-readRDS(source)
    
    if (nrow(richness)==0){
      richness<-first
    }else{
      
      richness<-rbindlist(list(first, richness))
    }
    richness<-merge(richness, local, by="global_id")
  }
  years<-split(richness, by="year")
  for (y in yearsx){
    yearlist[[y]][[length(yearlist[[y]])+1]]<-years[[y]]
  }
}

for (y in yearsx){
  print(y)
  year.df<-rbindlist(yearlist[[y]])
  saveRDS(year.df, sprintf("../Data/Tables/Yearly.Richness/%s.rda", y))
}

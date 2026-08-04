library(data.table)
library(sf)
library(RSQLite)
library(DBI)
library(ggplot2)
library(ggh4x)
setwd("/media/huijieqiao/Butterfly/GABI/GABI")
target<-"/media/huijieqiao/Butterfly/GABI/Results"
if (F){
  folders<-list.dirs(target, full.names=T)
  length(folders)
  ns<-read_sf("../Shape/isea3h8/N_S_America.shp")
  ns<-data.table(ns)
  ns$geometry<-NULL
  
  saveRDS(folders, "../Data/LOG/all.sim.folders.rda")
}
cells<-readRDS("../Data/Tables/cells.with.dist.rda")
colnames(cells)[1]<-"global_id"
cells$geometry<-NULL
cells<-data.table(cells)
folders<-list.dirs(target)
folders<-folders[2:length(folders)]
folders<-folders[sample(length(folders), length(folders))]
f<-folders[1]


for (i in c(1:length(folders))){
  f<-folders[i]
  
  info<-basename(f)
  infos<-strsplit(info, "\\.")[[1]]
  
  print(paste(f, i, length(folders)))
  fffff<-sprintf("%s/%s.log", f, info)
  if (!file.exists(fffff)){
    next()
  }
  
  target<-sprintf("%s/dis.burn.in.1801.1.rda", f)
  if (file.exists(target)){
    next()
  }
  
  saveRDS(NULL, target)
  log<-fread(fffff)
  colnames(log)<-c("year", "global_id", "group_id", "n", "sp_id", "suitable")
  log<-log[suitable==1]
  
  log<-log[year==1801]
  saveRDS(log, target)
  
}


if (F){
  
  cells<-readRDS("../Data/Tables/cells.with.dist.rda")
  colnames(cells)[1]<-"global_id"
  cells$geometry<-NULL
  cells<-data.table(cells)
  folders<-list.dirs(target)
  folders<-folders[2:length(folders)]
  
  folders<-folders[sample(length(folders), length(folders))]
  f<-folders[1]
  range.list<-list()
  final.dis<-list()
  for (i in c(1:length(folders))){
    f<-folders[i]
    #f<-gsub("/Results/", "/Results.NULL/", f)
    info<-basename(f)
    infos<-strsplit(info, "\\.")[[1]]
    seed_id<-as.numeric(infos[1])
    
    target<-sprintf("%s/dis.burn.in.1801.1.rda", f)
    
    if (!file.exists(target)){
      next()
    }
    print(paste(f, i, length(folders)))
    dis.item<-readRDS(sprintf("%s/dis.burn.in.1801.1.rda", f))
    dis.item$seed_id<-seed_id
    dis.item$nb<-infos[2]
    dis.item$da<-infos[3]
    final.dis[[length(final.dis)+1]]<-dis.item
  }
  
  
  final.dis.df<-rbindlist(final.dis)
  saveRDS(final.dis.df, "../Data/Tables/Burn.in.Distribution.rda")
  #saveRDS(final.dis.df, "../Data/Tables/Burn.in.Distribution.NULL.rda")
  
}

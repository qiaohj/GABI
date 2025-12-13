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
  
  folders<-folders[grepl("BIG", folders) | grepl("MODERATE", folders)]
  saveRDS(folders, "../Data/LOG/all.sim.folders.rda")
}
cells<-readRDS("../Data/cells.with.dist.rda")
colnames(cells)[1]<-"global_id"
cells$geometry<-NULL
cells<-data.table(cells)
folders<-readRDS("../Data/LOG/all.sim.folders.rda")
folders<-folders[sample(length(folders), length(folders))]
f<-folders[1]
seeds.all<-readRDS("../Data/Tables/random.seeds.threshold.by.nb.distribution.rda")
seeds.all$rep<-NULL
seeds.all$N_SPECIES<-NULL
seeds.all<-unique(seeds.all$seed_id)

for (i in c(1:length(folders))){
  f<-folders[i]
  info<-basename(f)
  infos<-strsplit(info, "\\.")[[1]]
  seed_id<-as.numeric(infos[1])
  if (! seed_id %in% seeds.all){
    #print("seed id is not in the picked seed pool, skip")
    #next()
  }
  print(paste(f, i, length(folders)))
  target<-sprintf("%s/range.rda", f)
  if (file.exists(target)){
    next()
  }
  
  saveRDS(NULL, target)
  log<-fread(sprintf("%s/%s.log", f, info))
  colnames(log)<-c("year", "global_id", "group_id", "n", "sp_id", "suitable")
  log<-log[suitable==1]
  log<-log[year==0]
  saveRDS(log, sprintf("%s/final.dis.rda", f))
  if (nrow(log)==0){
    next()
  }
  
  log.full<-merge(log, cells, by=c("global_id"))
  range<-log.full[, .(N.Cells=length(unique(global_id)),
                      min.lon=min(lon),
                      min.lat=min(lat),
                      max.lon=max(lon),
                      max.lat=max(lat)),
                  by=list(sp_id)]
  
  range$seed_id<-infos[1]
  range$nb<-infos[2]
  range$da<-infos[3]
  saveRDS(range, target)
  
}



cells<-readRDS("../Data/cells.with.dist.rda")
colnames(cells)[1]<-"global_id"
cells$geometry<-NULL
cells<-data.table(cells)
folders<-readRDS("../Data/LOG/all.sim.folders.rda")
folders<-folders[sample(length(folders), length(folders))]
f<-folders[1]
seeds.all<-readRDS("../Data/Tables/random.seeds.threshold.by.nb.distribution.rda")
seeds.all$rep<-NULL
seeds.all$N_SPECIES<-NULL
seeds.all<-unique(seeds.all$seed_id)
range.list<-list()
final.dis<-list()
for (i in c(1:length(folders))){
  f<-folders[i]
  #f<-gsub("/Results/", "/Results.NULL/", f)
  info<-basename(f)
  infos<-strsplit(info, "\\.")[[1]]
  seed_id<-as.numeric(infos[1])
  
  target<-sprintf("%s/range.rda", f)
  if (! seed_id %in% seeds.all){
    print("seed id is not in the picked seed pool, skip")
    next()
  }
  if (!file.exists(target)){
    next()
  }
  print(paste(f, i, length(folders)))
  range.item<-readRDS(target)
  range.list[[length(range.list)+1]]<-range.item
  
  dis.item<-readRDS(sprintf("%s/final.dis.rda", f))
  dis.item$seed_id<-seed_id
  dis.item$nb<-infos[2]
  dis.item$da<-infos[3]
  final.dis[[length(final.dis)+1]]<-dis.item
}

range.df<-rbindlist(range.list)
saveRDS(range.df, "../Data/Tables/Species.Range.rda")
#saveRDS(range.df, "../Data/Tables/Species.Range.NULL.rda")
final.dis.df<-rbindlist(final.dis)
saveRDS(final.dis.df, "../Data/Tables/Final.Distribution.rda")
#saveRDS(final.dis.df, "../Data/Tables/Final.Distribution.NULL.rda")

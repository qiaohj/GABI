library(data.table)
library(sf)
library(RSQLite)
library(DBI)
library(ggplot2)
setwd("/media/huijieqiao/Butterfly/GABI/GABI")
target<-"/media/huijieqiao/Butterfly/GABI/Results_NULL"
#target<-"/media/huijieqiao/Butterfly/GABI/Results"

folders<-list.dirs(target, full.names=T)
length(folders)
folders<-folders[2:length(folders)]
length(folders)
ns<-read_sf("../Shape/isea3h8/N_S_America.shp")
ns<-data.table(ns)
ns$geometry<-NULL
f<-folders[1]
folders<-folders[sample(length(folders), length(folders))]
if (F){
  #seed_100<-readRDS("../Data/Tables/sp_full_continents.rda")
  ids<-unique(seed_100$seed_id)
  fids<-strsplit(gsub("/media/huijieqiao/Butterfly/GABI/Results/", "", folders), "\\.")
  first_items <- as.numeric(sapply(fids, "[[", 1))
  folders<-folders[first_items %in% ids]
}
if (F){
  for (i in c(1:length(folders))){
    f<-folders[i]
    target<-sprintf("%s/continent_n_cells.rda", f)
    if (!file.exists(target)){
      next()
    }
    if (file.size(target)<100){
      print(f)
      
      unlink(target)
      
    }
  }
}

for (i in c(1:length(folders))){
  f<-folders[i]
  files<-list.files(f)
  
  #print(length(files))
  if (length(files)>=4){
    print(paste(f, i, length(folders)))
    target<-sprintf("%s/continent_n_cells.rda", f)
    if (file.exists(target)){
      next()
    }
    ff<-files[grepl("sqlite", files)]
    ff<-gsub("\\.sqlite", "", ff)
    saveRDS(NULL, target)
    log<-fread(sprintf("%s/%s.log", f, ff))
    colnames(log)<-c("year", "global_id", "group_id", "n", "sp_id", "suitable")
    log<-log[suitable==1]
    if (nrow(log)==0){
      next()
    }
    
    log.full<-merge(log, ns, by.x="global_id", by.y="seqnum")
    ids<-strsplit(ff, "\\.")[[1]]
    log.se<-log.full[, .(N=.N, 
                         seed_id=ids[1], 
                         nb=ids[2], da=ids[3]), 
                     by=list(year, continent, sp_id)]
    setorder(log.se, "year")
    saveRDS(log.se, target)
    
    log.div<-log.full[, .(N_SP=length(unique(sp_id)), 
                          seed_id=ids[1], 
                          nb=ids[2], da=ids[3]), 
                      by=list(year, continent, global_id)]
    saveRDS(log.div, sprintf("%s/species.richness.rda", f))
  }
}

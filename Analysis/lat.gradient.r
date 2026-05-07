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
target<-"/media/huijieqiao/Butterfly/GABI/Results"
ll<-readRDS("../Data/Tables/cells.with.dist.rda")
local<-data.table(global_id=as.numeric(ll$seqnum), lon=ll$lon, lat=ll$lat)
local$lat_bin<-floor((local$lat+0.5)/1)*1
folders<-list.dirs(target, full.names=T)
folders<-folders[sample(length(folders), length(folders))]
f<-folders[1]
for (i in c(1:length(folders))){
  f<-folders[i]
  files<-list.files(f)
  #print(length(files))
  if (length(files)>=4){
    print(paste(f, i, length(folders)))
    target<-sprintf("%s/lat.N.1degree.rda", f)
    if (file.exists(target)){
      next()
    }
    saveRDS(NULL, target)
    if ("distribution.rda" %in% files){
      ff<-files[grepl("sqlite", files)]
      log<-readRDS(sprintf("%s/distribution.rda", f))
      log<-rbindlist(log, idcol="year")
    }else{
      ff<-files[grepl("sqlite", files)]
      ff<-gsub("\\.sqlite", "", ff)
      log<-fread(sprintf("%s/%s.log", f, ff))
      colnames(log)<-c("year", "global_id", "group_id", "n", "sp_id", "suitable")
      log<-log[suitable==1]
      
    }
    if (nrow(log)==0){
      next()
    }
    
    log.full<-merge(log, local, by.x="global_id", by.y="global_id")
    ids<-strsplit(gsub("/media/huijieqiao/Butterfly/GABI/Results/", "", f), "\\.")[[1]]
    log.div<-log.full[, .(N_SP=length(unique(sp_id)), 
                          seed_id=ids[1], 
                          nb=ids[2], da=ids[3]), 
                      by=list(year, lat_bin)]
    saveRDS(log.div, target)
  }
  if (F){
    all<-list()
    for (i in c(1:length(folders))){
      f<-folders[i]
      
      source<-sprintf("%s/lat.N.rda", f)
      if (file.exists(source)){
        print(paste(f, i, length(folders)))
        item<-readRDS(source)
        all[[length(all)+1]]<-item
      }
    }
    all_df<-rbindlist(all)
    
    saveRDS(all_df, "../Data/Tables/Lat.N.1defree.rda")
  }
  
}


if (F){
  base_db<-"../Configuration/conf.sqlite"
  mydb <- dbConnect(RSQLite::SQLite(), base_db)
  simulations<-dbReadTable(mydb, "simulations")
  dbDisconnect(mydb)
  simulations<-data.table(simulations)
  
  ll<-readRDS("../Data/Tables/cells.with.dist.rda")
  local<-data.table(global_id=as.numeric(ll$seqnum), lon=ll$lon, lat=ll$lat)
  local$lat_bin<-floor((local$lat+2.5)/5)*5
  
  yearlist<-list()
  yearsx<-c(seq(0, 1600, by=100), 1605)
  yearsx<-as.character(yearsx)
  for (year in yearsx){
    yearlist[[year]]<-list()
  }
  for (i in c(1:nrow(simulations))){
    
    item<-simulations[i]
    
    source<-sprintf("../Results/%d.%s.%s/species.richness.rda",
                    item$global_id, item$nb, item$da)
    print(paste(i, nrow(simulations), source))
    
    if (!file.exists(source)){
      next()
    }
    first<-data.table(year=1605, continent=item$continent, global_id=item$global_id,
                      N_SP=1, seed_id=item$global_id, nb=item$nb, da=item$da)
    richness<-readRDS(source)
    
    if (nrow(richness)==0){
      richness<-first
    }else{
      richness<-rbindlist(list(first, richness))
    }
    richness<-merge(richness, local, by="global_id")
    
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
  
  
}
if (F){
  yearsx<-c(seq(0, 1600, by=100), 1605)
  y=0
  all_df<-readRDS("../Data/Tables/Lat.N.1defree.rda")
  all_df$seed_id<-as.numeric(all_df$seed_id)
  all_df$label<-sprintf("%d.%s.%s", all_df$seed_id, all_df$nb, all_df$da)
  ll<-readRDS("../Data/Tables/cells.with.dist.rda")
  local<-data.table(seed_id=as.numeric(ll$seqnum), original_continent=ll$continent)
  seeds.all<-readRDS("../Data/Tables/random.seeds.threshold.by.nb.distance.rda")
  for (y in yearsx){
    df<-all_df[year==y]
    df_filtered<-df[label %in% seeds.all[rep==1]$label]
    df_filtered<-merge(df_filtered, local, by="seed_id")
    table(df_filtered$continent)
    richness<-df_filtered[, .(N=sum(N_SP)), by=list(lat_bin, original_continent, nb)]
    
    richness$continent<-ifelse(richness$lat_bin>10, "North America", "South America")
    richness$abs_lat_bin<-abs(richness$lat_bin)
    ggplot(richness)+geom_line(aes(y=N, x=lat_bin, color=original_continent))+
      facet_grid(nb~continent, scale="free")
    
  }
  
  
}

library(data.table)
library(sf)
library(RSQLite)
library(DBI)
library(ggplot2)
setwd("/media/huijieqiao/WD22T_11/GABI/Script")
rm(list=ls())
target<-"/media/huijieqiao/WD22T_11/GABI/Results_500k_NB_BROAD"

folders<-list.dirs(target, full.names=T)
length(folders)
ns<-read_sf("../Data/Shape/isea3h8/N_S_America.shp")
ns<-data.table(ns)
ns$geometry<-NULL
folders<-folders[sample(length(folders), length(folders))]
f<-folders[1]
nb_labels<-c("NARROW", "MODERATE", "BROAD")
da_labels<-c("GOOD", "POOR")
nb_list<-data.table(expand.grid(x=nb_labels, y=nb_labels, z=da_labels))

if (F){
  all<-list()
  for (i in c(0:1799)){
    allitem<-list()
    for (j in c(1:nrow(nb_list))){
      lab<-sprintf("%s-%s-%s", nb_list[j]$x, nb_list[j]$y, nb_list[j]$z)
      allitem[[lab]]<-list()
    }
    all[[as.character(i)]]<-allitem
  }
  allbak<-all
  
  all<-allbak
  for (i in c(1:length(folders))){
    f<-folders[i]
    if (!grepl("GOOD", f) & !grepl("POOR", f)){
      next()
    } 
    
    print(paste(i, length(folders), f))
    target<-sprintf("%s/species.richness.rda", f)
    if (!file.exists(target)){
      next()
    }
    df<-readRDS(target)
    if (is.null(df)){
      next()
    }
    if (nrow(df)==0){
      next()
    }
    lab<-sprintf("%s-%s", df[1]$nb, df[1]$da)
    seed_id<-as.character(df[1]$seed_id)
    for (y in unique(df$year)){
      item<-df[year==y]
      all[[as.character(y)]][[lab]][[seed_id]]<-item
    }
  }
  
  for (i in c(0:1799)){
    for (j in c(1:nrow(nb_list))){
      lab<-sprintf("%s-%s-%s", nb_list[j]$x, nb_list[j]$y, nb_list[j]$z)
      print(paste(i, lab))
      df<-rbindlist(all[[as.character(i)]][[lab]])
      folder<-sprintf("../Data/Tables/500k.NB_BROAD.real_distribution/%d", i)
      if (!dir.exists(folder)){
        dir.create(folder)
      }
      saveRDS(df, sprintf("%s/%s.rda", folder, lab))
    }
  }
  
}
i=0
j=1

for (i in c(0:1799)){
  for (j in c(1:nrow(nb_list))){
    lab<-sprintf("%s-%s-%s", nb_list[j]$x, nb_list[j]$y, nb_list[j]$z)
    print(paste(i, lab))
    folder<-sprintf("../Data/Tables/500k.NB_BROAD.real_distribution/%d", i)
    df<-readRDS(sprintf("%s/%s.rda", folder, lab))
    df[, c("nb.pr","nb.tas") := data.table(str_split_fixed(df$nb,"-",2))]
    df$seed_id<-as.numeric(df$seed_id)
    df<-merge(df, ns, by.x="seed_id", by.y="seqnum")
    colnames(df)[c(3, 10, 11, 12)]<-c("continent.global", "continent.seed", "lon.seed", "lat.seed")
    df<-merge(df, ns, by.x="global_id", by.y="seqnum")
    df$continent<-NULL
    colnames(df)[c(13, 14)]<-c("lon.global", "lat.global")
    df_se<-df[, .(N_SP=max(N_SP)), by=list(continent.global, continent.seed,
                                           nb.pr, nb.tas, da, seed_id)]
    
    
  }
}


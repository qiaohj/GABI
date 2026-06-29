library(data.table)
library(sf)
library(RSQLite)
library(DBI)
library(ggplot2)
setwd("/media/huijieqiao/Butterfly/GABI/GABI")
target<-"/media/huijieqiao/Butterfly/GABI/Results_NULL"
target<-"/media/huijieqiao/Butterfly/GABI/Results"

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
if (F){
  for (i in c(1:length(folders))){
    f<-folders[i]
    files<-list.files(f)
    if (length(files[grepl("too", files)])>0){
      next()
    }
    target<-sprintf("%s/continent_n_cells.rda", f)
    if (file.exists(target)){
      xx<-readRDS(target)
      xx.length<-length(unique(xx[year==min(xx$year)]$sp_id))
      if (xx.length>500){
        print(paste(i, length(folders)))
        saveRDS(NULL, sprintf("%s/too.many.species", f))
      }
    }
  }
}
if (F){
  ns<-read_sf("../Shape/isea3h8/N_S_America.shp")
  ids<-c()
  listxx<-list()
  for (i in c(1:length(folders))){
    f<-folders[i]
    files<-list.files(f)
    if (length(files[grepl("too", files)])>0){
      seed_id<-strsplit(gsub("/media/huijieqiao/Butterfly/GABI/Results/", "", f), 
                        "\\.")[[1]][1]
      nb<-strsplit(gsub("/media/huijieqiao/Butterfly/GABI/Results/", "", f), 
                        "\\.")[[1]][2]
      da<-strsplit(gsub("/media/huijieqiao/Butterfly/GABI/Results/", "", f), 
                        "\\.")[[1]][3]
      ids<-c(ids, seed_id)
      listxx[[length(listxx)+1]]<-data.table(seed_id=seed_id, nb=nb, da=da)
      
    }
  }
  ids<-unique(ids)
  listxx<-rbindlist(listxx)
  ns$color<-ns$seqnum %in% ids
  ggplot(ns)+geom_sf(aes(fill=color), color=NA)
  xxx<-data.table(ns)
  xxx[,.(N=.N), by=list(continent, color)]
}

if (F){
  library(data.table)
  library(sf)
  library(RSQLite)
  library(DBI)
  library(ggplot2)
  setwd("/media/huijieqiao/Butterfly/GABI/GABI")
  target<-"/media/huijieqiao/Butterfly/GABI/Results"
  #merge data table
  conn<-dbConnect(RSQLite::SQLite(), "../Configuration/conf.sqlite")
  simulations<-data.table(dbReadTable(conn, "simulations"))
  dbDisconnect(conn)
  
  
  #simulations_sub<-simulations[global_id %in% ids]
  simulations_sub<-simulations
  table(simulations_sub$nb)
  simulations_sub[, .(N=.N), by=list(nb, da, continent)]
  
  ns<-read_sf("../Shape/isea3h8/N_S_America.shp")
  ns<-data.table(ns)
  ns$geometry<-NULL
  all_df<-list()
  all_df_merge_isthmus<-list()
  div_df<-NULL
  div_df_nb<-NULL
  div_df_da<-NULL
  div_df_nb_da<-NULL
  i=1
  for (i in c(1:nrow(simulations_sub))){
    print(paste(i, nrow(simulations_sub)))
    item<-simulations_sub[i]
    target<-sprintf("/media/huijieqiao/Butterfly/GABI/Results/%s/continent_n_cells.rda", 
                    item$label)
    if (!file.exists(target)){
      next()
    }
    df<-readRDS(target)
    
    
    if (is.null(df)){
      
      next()
    }else{
      
      if (nrow(df)>0){
        df$label<-item$label
        df_merge_isthmus<-df
        df_merge_isthmus[continent=="bridge1", continent:="North America"]
        df_merge_isthmus<-df_merge_isthmus[,.(N=sum(N)), by=list(year, continent, sp_id, seed_id, nb, da, label)]
      }
    }
    
    all_df[[i]]<-df
    all_df_merge_isthmus[[i]]<-df_merge_isthmus
    
    
    rm(list=c("target", "df"))
    target_div<-sprintf("/media/huijieqiao/Butterfly/GABI/Results/%s/species.richness.rda", 
                        item$label)
    if (file.exists(target_div)){
      df_div<-readRDS(target_div)
      df_div$seed_id<-as.numeric(df_div$seed_id)
    }else{
      df_div<-NULL
    }
    
    
    if (is.null(df_div)){
      next()
    }else{
      if (nrow(df_div)>0){
        df_div$label<-item$label
      }
    }
    
    df_div<-df_div[, c("year", "continent", "global_id", "nb", "da", "N_SP")]
    df_div<-df_div[year %in% seq(0, 1900, by=100)]
    
    if (is.null(div_df)){
      temp_div_df<-df_div
      div_df_nb_da<-temp_div_df
      
      
      temp_div_df$nb<-NULL
      div_df_da<-temp_div_df
      
      temp_div_df<-df_div
      temp_div_df$da<-NULL
      div_df_nb<-temp_div_df
      
      temp_div_df$nb<-NULL
      div_df<-temp_div_df
      
    }else{
      
      temp_div_df<-df_div
      div_df_nb_da<-rbindlist(list(div_df_nb_da, temp_div_df))
      div_df_nb_da<-div_df_nb_da[, .(N_SP=sum(N_SP)), 
                                 by=list(year, continent, global_id, nb, da)]
      
      
      
      temp_div_df$nb<-NULL
      div_df_da<-rbindlist(list(div_df_da, temp_div_df))
      div_df_da<-div_df_da[, .(N_SP=sum(N_SP)), 
                           by=list(year, continent, global_id, da)]
      
      
      temp_div_df<-df_div
      temp_div_df$da<-NULL
      div_df_nb<-rbindlist(list(div_df_nb, temp_div_df))
      div_df_nb<-div_df_nb[, .(N_SP=sum(N_SP)), 
                           by=list(year, continent, global_id, nb)]
      
      temp_div_df$nb<-NULL
      
      div_df<-rbindlist(list(div_df, temp_div_df))
      div_df<-div_df[, .(N_SP=sum(N_SP)), by=list(year, continent, global_id)]
      
    }
    
    
    
  }
  all_dfx<-rbindlist(all_df)
  all_df_merge_isthmusx<-rbindlist(all_df_merge_isthmus)
  saveRDS(all_dfx, "../Data/Tables/virtual.species.rda")
  saveRDS(all_df_merge_isthmusx, "../Data/Tables/virtual.species.merge.isthmus.rda")
  saveRDS(div_df, "../Data/Tables/virtual.species.richness.all.rda")
  saveRDS(div_df_nb, "../Data/Tables/virtual.species.richness.nb.rda")
  saveRDS(div_df_da, "../Data/Tables/virtual.species.richness.da.rda")
  saveRDS(div_df_nb_da, "../Data/Tables/virtual.species.richness.nb_da.rda")
  
  
}
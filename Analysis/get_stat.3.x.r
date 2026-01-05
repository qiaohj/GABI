library(data.table)
library(sf)
library(RSQLite)
library(DBI)
library(ggplot2)
setwd("/media/huijieqiao/Butterfly/GABI/GABI")
target<-"/media/huijieqiao/Butterfly/GABI/Results"
folders<-list.dirs(target, full.names=T)
length(folders)
folders<-folders[2:length(folders)]
ns<-read_sf("../Shape/isea3h8/N_S_America.shp")
ns<-data.table(ns)
ns$geometry<-NULL
f<-folders[1]
folders<-folders[sample(length(folders), length(folders))]

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
  simulations_sub<-simulations[continent_id<=100]
  #simulations_sub<-simulations
  ns<-read_sf("../Shape/isea3h8/N_S_America.shp")
  ns<-data.table(ns)
  ns$geometry<-NULL
  all_df<-list()
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
        
      }
    }
    all_df[[i]]<-df
    
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
    df_div<-df_div[year %in% seq(0, 1800, by=100)]
    
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
  
  saveRDS(all_dfx, "../Data/Tables/virtual.species.rda")
  saveRDS(div_df, "../Data/Tables/virtual.species.richness.all.rda")
  saveRDS(div_df_nb, "../Data/Tables/virtual.species.richness.nb.rda")
  saveRDS(div_df_da, "../Data/Tables/virtual.species.richness.da.rda")
  saveRDS(div_df_nb_da, "../Data/Tables/virtual.species.richness.nb_da.rda")
  
  
}
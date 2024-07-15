library(data.table)
library(sf)
library(RSQLite)
library(DBI)
library(ggplot2)
setwd("/media/huijieqiao/WD22T_11/GABI/Script")
target<-"/media/huijieqiao/WD22T_11/GABI/Results"
folders<-list.dirs(target, full.names=T)
length(folders)
ns<-read_sf("../Data/Shape/isea3h8/N_S_America.shp")
ns<-data.table(ns)
ns$geometry<-NULL
folders<-folders[sample(length(folders), length(folders))]
f<-folders[1]
for (f in folders){
  if (!grepl("GOOD", f) & !grepl("POOR", f)){
    next()
  } 
  
  files<-list.files(f)
  #print(length(files))
  if (length(files)>=4){
    print(paste(f, length(folders)))
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
  setwd("/media/huijieqiao/WD22T_11/GABI/Script")
  target<-"/media/huijieqiao/WD22T_11/GABI/Results"
  #merge data table
  conn<-dbConnect(RSQLite::SQLite(), "../Configuration/conf.sqlite")
  simulations<-data.table(dbReadTable(conn, "simulations"))
  dbDisconnect(conn)
  simulations_sub<-simulations[continent_id<=100]
  ns<-read_sf("../Data/Shape/isea3h8/N_S_America.shp")
  ns<-data.table(ns)
  ns$geometry<-NULL
  all_df<-list()
  div_df<-list()
  i=1
  for (i in c(1:nrow(simulations_sub))){
    print(paste(i, nrow(simulations_sub)))
    item<-simulations_sub[i]
    target<-sprintf("/media/huijieqiao/WD22T_11/GABI/Results/%s/continent_n_cells.rda", 
                    item$label)
    df<-readRDS(target)
    
    init_df<-data.table(year=1800, 
                        continent=ns[seqnum==item$global_id]$continent, 
                        sp_id=item$global_id, 
                        N=0, 
                        seed_id=item$global_id, 
                        nb=item$nb,
                        da=item$da,
                        label=item$label)
    if (is.null(df)){
      df<-init_df
    }else{
      if (nrow(df)>0){
        df$label<-item$label
        df<-rbindlist(list(init_df, df))
      }else{
        df<-init_df
      }
    }
    all_df[[i]]<-df
    
    rm(list=c("target", "df", "init_df"))
    target_div<-sprintf("/media/huijieqiao/WD22T_11/GABI/Results/%s/species.richness.rda", 
                    item$label)
    if (file.exists(target_div)){
      df_div<-readRDS(target_div)
    }else{
      df_div<-NULL
    }
    
    init_df_div<-data.table(year=1800, 
                        continent=ns[seqnum==item$global_id]$continent, 
                        global_id=item$global_id, 
                        N_SP=1, 
                        seed_id=item$global_id, 
                        nb=item$nb,
                        da=item$da,
                        label=item$label)
    if (is.null(df_div)){
      
      df_div<-init_df_div
    }else{
      if (nrow(df_div)>0){
        df_div$label<-item$label
        df_div<-rbindlist(list(init_df_div, df_div))
      }else{
        df_div<-init_df_div
      }
    }
    div_df[[i]]<-df_div
    
  }
  all_dfx<-rbindlist(all_df)
  div_dfx<-rbindlist(div_df)
  if (F){
    div_dfx<-div_df[1:(length(div_df)/3)]
    div_dfxxx<-rbindlist(div_dfx)
    item<-div_dfxxx[,.(N_SP=sum(N_SP)), by=list(year, continent, global_id)]
    
    div_dfx<-div_df[(length(div_df)/3+1):(2 * length(div_df)/3)]
    div_dfxxx<-rbindlist(div_dfx)
    item2<-div_dfxxx[,.(N_SP=sum(N_SP)), by=list(year, continent, global_id)]
    
    div_dfx<-div_df[((2 * length(div_df)/3)+1):(length(div_df)-600)]
    div_dfxxx<-rbindlist(div_dfx)
    item3<-div_dfxxx[,.(N_SP=sum(N_SP)), by=list(year, continent, global_id)]
    
    div_dfx<-div_df[3001:3600]
    div_dfxxx<-rbindlist(div_dfx)
    item4<-div_dfxxx[,.(N_SP=sum(N_SP)), by=list(year, continent, global_id)]
    
    div<-rbindlist(list(item, item2, item3, item4))
    div<-div[,.(N_SP=sum(N_SP)), by=list(year, continent, global_id)]
    
    saveRDS(div, "../Data/Tables/500k.speciation.years/c100.virtual.species.richness.rda")
  }
  saveRDS(all_dfx, "../Data/Tables/500k.speciation.years/c100.virtual.species.rda")
  saveRDS(div_dfx, "../Data/Tables/500k.speciation.years/c100.virtual.species.richness.rda")
  
}
library(data.table)
library(sf)
library(RSQLite)
library(DBI)
library(ggplot2)
setwd("/media/huijieqiao/Butterfly/GABI/GABI")
target<-"/media/huijieqiao/Butterfly/GABI/Results.NULL"
folders<-list.dirs(target, full.names=T)
length(folders)
ns<-read_sf("../Shape/isea3h8/N_S_America.shp")
ns<-data.table(ns)
ns$geometry<-NULL
folders<-folders[sample(length(folders), length(folders))]
f<-folders[1]
cells<-readRDS("../Data/cells.with.dist.rda")
colnames(cells)[1]<-"global_id"
cells$geometry<-NULL
cells<-data.table(cells)

for (i in c(1:length(folders))){
  f<-folders[i]
  info<-basename(f)
  infos<-strsplit(info, "\\.")[[1]]
  seed_id<-as.numeric(infos[1])
  files<-list.files(f)
  #print(length(files))
  if (length(files)>=4){
    print(paste(f, i, length(folders)))
    target<-sprintf("%s/continent_n_cells.rda", f)
    if (file.exists(target)){
      print("Skip")
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
    target<-sprintf("%s/range.rda", f)
    saveRDS(range, target)
    
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
  #simulations_sub<-simulations[continent_id<=100]
  simulations_sub<-simulations
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
    df<-readRDS(target)
    
    init_df<-data.table(year=1800, 
                        continent=ns[seqnum==item$global_id]$continent, 
                        sp_id=item$global_id, 
                        N=1, 
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
    target_div<-sprintf("/media/huijieqiao/Butterfly/GABI/Results/%s/species.richness.rda", 
                    item$label)
    if (file.exists(target_div)){
      df_div<-readRDS(target_div)
      df_div$seed_id<-as.numeric(df_div$seed_id)
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
    
    df_div<-df_div[, c("year", "continent", "global_id", "nb", "da", "N_SP")]
    df_div<-df_div[year %in% seq(0, 1800, by=100)]
    print(nrow(df_div))
    
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
  
  saveRDS(all_dfx, "../Data/Tables/100k.speciation.years/virtual.species.rda")
  saveRDS(div_df, "../Data/Tables/100k.speciation.years/virtual.species.richness.all.rda")
  saveRDS(div_df_nb, "../Data/Tables/100k.speciation.years/virtual.species.richness.nb.rda")
  saveRDS(div_df_da, "../Data/Tables/100k.speciation.years/virtual.species.richness.da.rda")
  saveRDS(div_df_nb_da, "../Data/Tables/100k.speciation.years/virtual.species.richness.nb_da.rda")
  
  
}
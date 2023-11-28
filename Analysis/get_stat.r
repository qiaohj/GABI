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
for (f in folders){
  if (!grepl("GOOD", f) & !grepl("POOR", f)){
    next()
  }
  
  files<-list.files(f)
  #print(length(files))
  if (length(files)>=4){
    print(f)
    target<-sprintf("%s/stat.rda", f)
    if (file.exists(target)){
      next()
    }
    ff<-files[grepl("sqlite", files)]
    ff<-gsub("\\.sqlite", "", ff)
    saveRDS(NULL, target)
    log<-fread(sprintf("%s/%s.log", f, ff))
    colnames(log)<-c("year", "global_id", "group_id", "n", "sp_id", "suitable")
    log<-log[suitable==1]
    log.full<-merge(log, ns, by.x="global_id", by.y="seqnum")
    ids<-strsplit(ff, "_")[[1]]
    log.se<-log.full[, .(N=.N, global_id=ids[1], 
                         sp_id=ids[2], da=ids[3]), 
                     by=list(year, continent)]
    saveRDS(log.se, target)
  }
}

if (F){
  #merge data table
  conn<-dbConnect(RSQLite::SQLite(), "../Configuration/conf.sqlite")
  simulations<-data.table(dbReadTable(conn, "simulations"))
  dbDisconnect(conn)
  simulations_sub<-simulations[continent_id<=100 & species_id<=100]
  ns<-read_sf("../Data/Shape/isea3h8/N_S_America.shp")
  ns<-data.table(ns)
  ns$geometry<-NULL
  all_df<-list()
  for (i in c(1:nrow(simulations_sub))){
    print(paste(i, nrow(simulations_sub)))
    item<-simulations_sub[i]
    target<-sprintf("/media/huijieqiao/WD22T_11/GABI/Results/%d/%s/stat.rda", 
                    item$global_id, item$label)
    df<-readRDS(target)
    init_df<-data.table(year=1800, continent=ns[seqnum==item$global_id]$continent, N=0, 
                        global_id=item$global_id, sp_id=item$species_id, da=item$da,
                        label=item$label)
    if (nrow(df)>0){
      df$label<-item$label
      df<-rbindlist(list(init_df, df))
    }else{
      df<-init_df
    }
    all_df[[i]]<-df
  }
  all_dfx<-rbindlist(all_df)
  saveRDS(all_dfx, "../Data/Tables/100.100.rda")
}
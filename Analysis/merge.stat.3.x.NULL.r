library(data.table)
library(sf)
library(RSQLite)
library(DBI)
library(ggplot2)
setwd("/media/huijieqiao/Butterfly/GABI/GABI")
target<-"/media/huijieqiao/Butterfly/GABI/Results_NULL"
#merge data table
conn<-dbConnect(RSQLite::SQLite(), "../Configuration/conf.null.sqlite")
simulations<-data.table(dbReadTable(conn, "simulations"))
dbDisconnect(conn)
if (F){
  conn<-dbConnect(RSQLite::SQLite(), "../Configuration/conf.sqlite")
  ssss<-data.table(dbReadTable(conn, "simulations"))
  dbDisconnect(conn)
  ns<-read_sf("../Shape/isea3h8/N_S_America.shp")
  seeds<-ns[which(ns$seqnum %in% unique(ssss$global_id)),]
  write_sf(seeds, "../Shape/isea3h8/Seeds.shp")
}


#simulations_sub<-simulations[global_id %in% ids]
simulations_sub<-simulations
table(simulations_sub$nb)
simulations_sub[, .(N=.N), by=list(nb, da, continent)]

ns<-read_sf("../Shape/isea3h8/N_S_America.shp")
ns<-data.table(ns)
ns$geometry<-NULL
table(ns$continent)
all_df<-list()
all_df_merge_isthmus<-list()
div_df<-NULL
div_df_nb<-NULL
div_df_da<-NULL
div_df_nb_da<-NULL
i=1
simulations_sub[global_id==33105 & is_run==1]
for (i in c(1:nrow(simulations_sub))){
  print(paste(i, nrow(simulations_sub)))
  item<-simulations_sub[i]
  if (F){
    if (item$global_id==33105 & item$nb=="BROAD" & item$da=="GOOD"){
      adsf
    }
    next()
  }
  target<-sprintf("/media/huijieqiao/Butterfly/GABI/Results_NULL/%s/continent_n_cells.rda", 
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
      df_merge_isthmus<-copy(df)
      df_merge_isthmus[continent=="bridge1", continent:="North America"]
      df_merge_isthmus<-df_merge_isthmus[,.(N=sum(N)), by=list(year, continent, sp_id, seed_id, nb, da, label)]
    }else{
      next()
    }
  }
  
  all_df[[i]]<-df
  all_df_merge_isthmus[[i]]<-df_merge_isthmus
  
  
  rm(list=c("target", "df"))
  target_div<-sprintf("/media/huijieqiao/Butterfly/GABI/Results_NULL/%s/species.richness.rda", 
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
saveRDS(all_dfx, "../Data/Tables/virtual.species.NULL.rda")
saveRDS(all_df_merge_isthmusx, "../Data/Tables/virtual.species.merge.isthmus.NULL.rda")
saveRDS(div_df, "../Data/Tables/virtual.species.richness.all.NULL.rda")
saveRDS(div_df_nb, "../Data/Tables/virtual.species.richness.nb.NULL.rda")
saveRDS(div_df_da, "../Data/Tables/virtual.species.richness.da.NULL.rda")
saveRDS(div_df_nb_da, "../Data/Tables/virtual.species.richness.nb_da.NULL.rda")

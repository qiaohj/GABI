library(data.table)
library(sf)
library(RSQLite)
library(DBI)
library(ggplot2)
setwd("/media/huijieqiao/Butterfly/GABI/GABI")

conn<-dbConnect(RSQLite::SQLite(), "../Configuration/conf.sqlite")
simulations<-data.table(dbReadTable(conn, "simulations"))
dbDisconnect(conn)

result_folder<-"/media/huijieqiao/Butterfly/GABI/Results"
ns<-read_sf("../Shape/isea3h8/N_S_America.shp")
ns<-data.table(ns)
ns$geometry<-NULL
for (i in c(1:nrow(simulations))){
  item<-simulations[i]
  f<-sprintf("%s/%s", result_folder, item$label)
  log.f<-sprintf("%s/%s/distribution.rda", result_folder, item$label)
  if (!file.exists(log.f)){
    next()
  }
  files<-list.files(f)
  #print(length(files))
  if (length(files)>=4){
    print(paste(f, i, length(files)))
    
    target<-sprintf("%s/continent_n_cells.rda", f)
    if (file.exists(target)){
      next()
    }
    
    saveRDS(NULL, target)
    log.list<-readRDS(log.f)
    for (nm in names(log.list)) {
      log.list[[nm]][, year := nm]
    }
    log<-rbindlist(log.list)
    if (nrow(log)==0){
      next()
    }
    
    log.full<-merge(log, ns, by.x="global_id", by.y="seqnum")
    log.se<-log.full[, .(N=.N, 
                         seed_id=item$global_id, 
                         nb=item$nb, da=item$da), 
                     by=list(year, continent, sp_id)]
    setorder(log.se, "year")
    saveRDS(log.se, target)
    
    log.div<-log.full[, .(N_SP=length(unique(sp_id)), 
                          seed_id=item$global_id, 
                          nb=item$nb, da=item$da), 
                      by=list(year, continent, global_id)]
    saveRDS(log.div, sprintf("%s/species.richness.rda", f))
  }
}



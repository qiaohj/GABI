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
folders[which(grepl("40435", folders))]

conn<-dbConnect(RSQLite::SQLite(), "../Configuration/configuration.sqlite")
pr<-data.table(dbReadTable(conn, "pr"))
tasmax<-data.table(dbReadTable(conn, "tasmax"))
tasmin<-data.table(dbReadTable(conn, "tasmin"))
dist<-data.table(dbReadTable(conn, "distances"))
dbDisconnect(conn)

colnames(pr)[2]<-"pr"
colnames(tasmax)[2]<-"tasmax"
colnames(tasmin)[2]<-"tasmin"

envs<-merge(merge(pr, tasmax, by=c("global_id", "year")),
            tasmin, by=c("global_id", "year"))
i=36084
for (i in c(1:length(folders))){
  f<-folders[i]
  files<-list.files(f)
  #print(length(files))
  if (length(files)>=4){
    print(paste(f, i, length(folders)))
    target<-sprintf("%s/nb.rda", f)
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
    
    log.full<-merge(log, envs, by=c("year", "global_id"))
    nb<-log.full[, .(min_pr=min(pr),
                     max_pr=max(pr),
                     min_tas=min(tasmax, tasmin),
                     max_tas=max(tasmax, tasmin)),
                 by=list(year, sp_id)]
    
    nb$pr<-nb$max_pr - nb$min_pr
    nb$tas<-nb$max_tas - nb$min_tas
    if (F){
      nb<-readRDS("/media/huijieqiao/WD22T_11/GABI/Results/40435.MODERATE-MODERATE.GOOD/nb.rda")
      ggplot(nb)+geom_line(aes(x=year*-2, y=tas, color=sp_id))+
        theme(legend.position = "none")
      ggplot(nb[year %in% c(1800, 1000, 500, 0)])+geom_histogram(aes(x=tas))+
        facet_wrap(~year)+
        theme(legend.position = "none")
    }
    
    saveRDS(nb, target)
  }
}

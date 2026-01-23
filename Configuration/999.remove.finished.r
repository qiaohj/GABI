library(data.table)
library(sf)
library(RSQLite)
library(DBI)
library(ggplot2)
setwd("/media/huijieqiao/Butterfly/GABI/GABI")
conn<-dbConnect(RSQLite::SQLite(), "../Configuration/conf.sqlite")
simulations<-data.table(dbReadTable(conn, "simulations"))
simulations$is_run<-ifelse(file.exists(sprintf("/media/huijieqiao/Butterfly/GABI/Results/%s",
                                               simulations$label)),
                           0, 1)
if (F){
  #seed_100<-readRDS("../Data/Tables/sp_full_continents.rda")
  length(unique(seed_100$seed_id))
  simulations$is_run<-0
  simulations[global_id %in%unique(seed_100$seed_id) & nb=="TINY", is_run:=1]
}
#simulations[nb!="NARROW", is_run:=0]
dbWriteTable(conn, "simulations", simulations, overwrite=T)
dbDisconnect(conn)

table(simulations$is_run)
setorderv(simulations, c("nb", "da", "is_run"))
simulations[, .(N=.N), by=list(nb, is_run)]
all.df<-simulations
all.list<-list()
for (i in c(1:nrow(all.df))){
  f<-sprintf(sprintf("/media/huijieqiao/Butterfly/GABI/Results/%s",
                     simulations[i]$label))
  n<-length(list.files(f))
  
  if (n<=2){
    print(i)
    xx<-data.table(file.info(f))
    xx$f<-f
    all.list[[length(all.list)+1]]<-xx
  }
}
all.df<-rbindlist(all.list)
dim(all.df)

all.list<-list()
for (i in c(1:nrow(all.df))){
  f<-sprintf(all.df[i]$f)
  n<-length(list.files(f))
  
  if (n<=2){
    print(i)
    xx<-data.table(file.info(f))
    xx$f<-f
    all.list[[length(all.list)+1]]<-xx
  }
}
all.df<-rbindlist(all.list)
dim(all.df)
setorderv(all.df, "ctime")
View(all.df)

if (F){
  conn<-dbConnect(RSQLite::SQLite(), "../Configuration/conf.sqlite")
  s<-data.table(dbReadTable(conn, "simulations"))
  dbDisconnect(conn)
  
  s<-s[nb=="BROAD"]
  for (i in c(1:nrow(s))){
    print(i)
    folder<-sprintf("/media/huijieqiao/Butterfly/GABI/Results/%s", s[i]$label)
    if (!dir.exists(folder)){
      dir.create(folder)
    }
  }
}
library(data.table)
library(RSQLite)
library(DBI)
library(sf)
library(ggplot2)
setwd("/media/huijieqiao/Butterfly/GABI/GABI")
conn<-dbConnect(RSQLite::SQLite(), "../Configuration/conf.sqlite")
simulations<-data.table(dbReadTable(conn, "simulations"))
dbDisconnect(conn)

simulations<-simulations[continent_id<=100]
result<-list()
for (i in c(1:nrow(simulations))){
  item<-simulations[i]
  print(paste(i, nrow(simulations)))
  old<-sprintf("../Results/%s/%s.log", item$label, item$label)
  new<-sprintf("/media/huijieqiao/WD22T_50/ES.R/Results/%s/distribution.rda", item$label)
  if (file.exists(old) & file.exists(new)){
    log.old<-fread(old)
    colnames(log.old)<-c("year", "global_id", "group_id", "n", "sp_id", "suitable")
    log.old<-log.old[suitable==1]
    lod.new<-readRDS(new)
    log.old.last<-log.old[year==0]
    N.sp.old<-length(unique(log.old.last$sp_id))
    lod.new.last<-lod.new[["0"]]
    N.sp.new<-length(unique(lod.new.last$sp_id))
    
    id.old<-unique(log.old.last$global_id)
    id.new<-unique(lod.new.last$global_id)
    
    overlap<-id.old[id.old %in% id.new]
    id.old.no<-id.old[! id.old %in% overlap]
    id.new.no<-id.new[! id.new %in% overlap]
    j.sim<-length(overlap)/(length(overlap)+length(id.old.no)+length(id.new.no))
    j.sim.old<-length(id.old.no)/(length(overlap)+length(id.old.no)+length(id.new.no))
    j.sim.new<-length(id.new.no)/(length(overlap)+length(id.old.no)+length(id.new.no))
    r.item<-data.table(seed_id=item$global_id,
                       nb=item$nb,
                       da=item$da,
                       continent=item$continent,
                       N.sp.old=N.sp.old,
                       N.sp.new=N.sp.new,
                       N.overlap=length(overlap),
                       N.old=length(id.old.no),
                       N.new=length(id.new.no),
                       j.sim=j.sim)
    result[[length(result)+1]]<-r.item
    if (F){
      cells<-rbindlist(list(
        data.table(id=overlap, type="overlap"),
        data.table(id=id.old.no, type="old"),
        data.table(id=id.new.no, type="new")
      ))
      table(cells$type)
      continent<-read_sf("../Shape/isea3h8/N_S_America.shp")
      cells<-merge(continent, cells, by.x="seqnum", by.y="id")
      ggplot(continent)+geom_sf()+
        geom_sf(data=cells, aes(fill=type))
    }
  }
  
}
result.df<-rbindlist(result)
saveRDS(result.df, "../Data/Tables/3.5vs4.rda")
result.df$N.all<-result.df$N.overlap+result.df$N.sp.new+result.df$N.overlap

ggplot(result.df)+geom_point(aes(x=N.all, y=j.sim))

ggplot(result.df[between(N.sp.old, 2, 100)])+
  geom_point(aes(x=N.sp.old, y=N.sp.new))
cor(result.df[between(N.sp.old, 2, 150)]$N.sp.old,
    result.df[between(N.sp.old, 2, 150)]$N.sp.new)

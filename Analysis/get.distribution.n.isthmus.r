library(data.table)
library(ggplot2)
library(sf)
setDTthreads(30)
setwd("/media/huijieqiao/Butterfly/GABI/GABI")
target<-"/media/huijieqiao/Butterfly/GABI/Results"

cells<-readRDS("../Data/Tables/cells.with.dist.rda")
ggplot(cells)+geom_sf(aes(fill=continent))

cells<-data.table(global_id=cells$seqnum, continent=cells$continent)

folders<-list.dirs(target, full.names=T)
folders<-folders[2:length(folders)]
folders<-folders[sample(length(folders), length(folders))]

f<-folders[1]
richness.list<-list()
richness.merged.isthmus.list<-list()

for (i in c(1:length(folders))){
  print(paste(i, length(folder)))
  f<-folders[i]
  dis.f<-sprintf("%s/final.dis.rda", f)
  if (!file.exists(dis.f)){
    next()
  }
  ids<-strsplit(gsub("/media/huijieqiao/Butterfly/GABI/Results/", "", f), "\\.")[[1]]
  
  df<-readRDS(dis.f)
  df<-df[, c("global_id", "sp_id")]
  df<-merge(df, cells, by="global_id")
  df.N<-df[, .(N.SP=length(unique(sp_id)),
               NB=ids[2],
               DA=ids[3],
               seed_id=ids[1]),
           by=list(continent)
           ]
  richness.list[[length(richness.list)+1]]<-df.N
  df[continent=="bridge1", continent:="North America"]
  df.N.merged<-df[, .(N.SP=length(unique(sp_id)),
               NB=ids[2],
               DA=ids[3],
               seed_id=ids[1]),
           by=list(continent)
  ]
  richness.merged.isthmus.list[[length(richness.merged.isthmus.list)+1]]<-df.N.merged
  
}
richness<-rbindlist(richness.list)
richness.merged.isthmus.list<-rbindlist(richness.merged.isthmus.list)

saveRDS(richness, "../Data/Tables/Richness.with.isthmus.rda")
saveRDS(richness.merged.isthmus.list, "../Data/Tables/Richness.merged.isthmus.rda")

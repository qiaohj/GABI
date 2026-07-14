library(data.table)
library(ggplot2)
library(sf)
library(stringr)
library(flextable)
library(officer)
library(RSQLite)
library(DBI)
library(patchwork)
setDTthreads(30)
setwd("/media/huijieqiao/Butterfly/GABI/GABI")
#For simulations
distribution<-readRDS("../Data/Tables/Final.Distribution.Unique.rda")
seeds.all<-readRDS("../Data/Tables/random.seeds.threshold.by.nb.distance.rda")

range(distribution$year)
i=1
richness.list<-list()
for (i in c(1:max(seeds.all$rep))){
  print(i)
  item<-seeds.all[rep==i]
  dis.item<-distribution[label %in% item$label]
  richness.item<-dis.item[,.(N.sp=length(unique(sp_label))), by=list(global_id)]
  richness.item$rep<-i
  richness.list[[i]]<-richness.item
}
richness<-rbindlist(richness.list)
richness<-richness[, .(N.sp=mean(N.sp), sd=sd(N.sp)), by=list(global_id)]


saveRDS(richness, "../Data/Tables/Final.Richness.rda")
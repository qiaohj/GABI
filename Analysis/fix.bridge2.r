library(data.table)
library(ggplot2)
library(sf)
setwd("/media/huijieqiao/Butterfly/GABI/GABI")

folders<-readRDS("../Data/LOG/all.sim.folders.rda")

folders<-gsub("MODERATE-MODERATE", "NARROW-NARROW", folders)
folders<-folders[sample(length(folders), length(folders))]
cells<-readRDS("../Data/cells.with.dist.rda")
bridge2.id<-cells[which(cells$continent=="bridge2"), ]$seqnum
bridge2.id<-bridge2.id[!bridge2.id %in% c(9580, 9662,9744,9663,9745,9664)]
for (i in c(1:length(folders))){
  f<-folders[i]
  print(paste(i, length(folders), f))
  bname<-basename(f)
  target<-sprintf("%s/species.richness.fixed.rda", f)
  if (file.exists(target)){
    print("Skip 1")
    next()
  }
  #path<-sprintf("%s/species.richness.rda", gsub("/Results/", "/Results.NULL/", f))
  path<-sprintf("%s/species.richness.rda", f)
  if (!file.exists(path)){
    next()
  }
  if (!file.exists(sprintf("%s/species.richness.rda", f))){
    next()
  }
  saveRDS(NULL, target)
  richness.null<-readRDS(path)
  richness.null.bridge<-richness.null[continent=="bridge2"]
  richness.null.bridge<-richness.null.bridge[!global_id %in% 
                                               c(9580, 9662,9744,9663,9745,9664)]
  
  if (nrow(richness.null.bridge)>0){
    if (F){
      cells<-readRDS("../Data/cells.with.dist.rda")
      bridge<-merge(cells, richness.null.bridge, by.x="seqnum", by.y="global_id")
      ggplot(bridge)+geom_sf(aes(fill=N_SP))
    }
    
    file.copy(sprintf("%s/species.richness.rda", f), target, overwrite=T)
    richness<-readRDS(sprintf("%s/species.richness.rda", f))
    richness<-rbindlist(list(richness, richness.null.bridge))
    saveRDS(richness, sprintf("%s/species.richness.rda", f))
    
    file.copy(sprintf("%s/final.dis.rda", f), 
              sprintf("%s/final.dis.fixed.rda", f), overwrite=T)
    final.dis<-readRDS(sprintf("%s/final.dis.rda", f))
    final.dis.NULL<-readRDS(sprintf("%s/final.dis.rda", gsub("/Results/", "/Results.NULL/", f)))
    final.dis.NULL<-final.dis.NULL[global_id %in% bridge2.id]
    if (nrow(final.dis.NULL)>0){
      final.dis<-rbindlist(list(final.dis, final.dis.NULL))
      saveRDS(final.dis, sprintf("%s/final.dis.rda", f))
    }
  }
}

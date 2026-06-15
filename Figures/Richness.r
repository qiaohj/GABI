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
if (F){
  distribution<-readRDS("../Data/Tables/Final.Distribution.rda")
  distribution$label<-sprintf("%d.%s.%s", distribution$seed_id, distribution$nb, distribution$da)
  distribution$sp_label<-sprintf("%s.%s", distribution$sp_id, distribution$label)
  distribution$group_id<-NULL
  distribution$n<-NULL
  distribution$suitable<-NULL
  distribution<-unique(distribution)
  saveRDS(distribution, "../Data/Tables/Final.Distribution.Unique.rda")
  
  distribution<-readRDS("../Data/Tables/Final.Distribution.NULL.rda")
  distribution$label<-sprintf("%d.%s.%s", distribution$seed_id, distribution$nb, distribution$da)
  distribution$sp_label<-sprintf("%s.%s", distribution$sp_id, distribution$label)
  distribution$group_id<-NULL
  distribution$n<-NULL
  distribution$suitable<-NULL
  distribution<-unique(distribution)
  saveRDS(distribution, "../Data/Tables/Final.Distribution.NULL.Unique.rda")
}

if (F){
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
  
  #For NULL models
  distribution<-readRDS("../Data/Tables/Final.Distribution.NULL.Unique.rda")
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
  saveRDS(richness, "../Data/Tables/Final.Richness.NULL.rda")
  
  #For IUCN
  sf_use_s2(FALSE)
  species.america<-readRDS(sprintf("../Data/IUCN_NB/Mammals.%s.rda", "World"))
  species.america<-species.america[continent=="America"]
  mammals<-st_read("../Shape/IUCN/MAMMALS/MAMMALS_TERRESTRIAL_ONLY.shp")
  mammals<-mammals[which(mammals$binomial %in% species.america$species),]
  length(unique(mammals$binomial))
  continents<-read_sf("../Shape/isea3h8/N_S_America.shp")
  continents$N.sp <- sapply(st_intersects(continents, mammals), function(idx) {
    if (length(idx) == 0) {
      return(0) 
    } else {
      overlapping_binomials <- mammals$binomial[idx]
      return(length(unique(na.omit(overlapping_binomials))))
    }
  })
  saveRDS(continents, "../Data/Tables/IUCN.dis.rda")
  p1.2<-ggplot()+ 
    geom_sf(data=continents, fill=NA, color="lightgray")+
    geom_sf(data=continents,  aes(fill=N.sp),
            color=NA, linewidth=0.1) +
    scale_fill_gradient2(low="#2166AC",
                         mid="#F7F7F7",
                         high="#B2182B",
                         midpoint = median(continents$N.sp))+
    theme(
      axis.line = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      panel.grid.major = element_line(linetype = "dashed", linewidth = 0.5, color="#dab27f"),
      plot.background = element_rect(fill="#fde7c0"),
      panel.background = element_rect(fill="#fde7c0"),
      legend.background = element_rect(fill = "#fde7c0", color = NA),
      legend.title = element_blank(),
      legend.position="bottom",
      legend.key.width = unit(1, 'cm'),
      plot.title = element_text(hjust = 0.5)
    )
}


richness<-readRDS("../Data/Tables/Final.Richness.rda")
colnames(richness)<-c("seqnum", "N.sp.sim", "sd.sim")
continents<-read_sf("../Shape/isea3h8/N_S_America.shp")
continents.sim<-merge(continents, richness, by="seqnum", all=T)
continents.sim[is.na(continents.sim$N.sp.sim),]$N.sp.sim<-0

richness.null<-readRDS("../Data/Tables/Final.Richness.NULL.rda")
colnames(richness.null)<-c("seqnum", "N.sp.null", "sd.null")
continents.sim.null<-merge(continents.sim, richness.null, by="seqnum", all=T)
continents.sim.null[is.na(continents.sim.null$N.sp.null),]$N.sp.null<-0

iucn<-readRDS("../Data/Tables/IUCN.dis.rda")
iucn<-data.table(seqnum=iucn$seqnum, N.iucn=iucn$N.sp)

continents.full<-merge(continents.sim.null, iucn, by="seqnum", all=T)
#continents.full[is.na(continents.full$N.iucn),]$N.iucn<-0


p1<-ggplot()+ 
  geom_sf(data=continents.full, fill=NA, color="lightgray")+
  geom_sf(data=continents.full,  aes(fill=N.sp.sim),
          color=NA, linewidth=0.1) +
  scale_fill_gradient2(low="#2166AC",
                       mid="#F7F7F7",
                       high="#B2182B",
                       midpoint = median(continents.full$N.sp.sim))+
  labs(tag = "(a) Simulation")+
  theme(
    axis.line = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_line(linetype = "dashed", linewidth = 0.5, color="#dab27f"),
    plot.background = element_rect(fill="#fde7c0"),
    panel.background = element_rect(fill="#fde7c0"),
    legend.background = element_rect(fill = "#fde7c0", color = NA),
    legend.title = element_blank(),
    legend.position="bottom",
    legend.key.width = unit(1, 'cm'),
    plot.title = element_text(hjust = 0.5),
    plot.tag.position = c(0.01, 0.99), 
    plot.tag = element_text(hjust = 0, size = 12)
  )



p2<-ggplot()+ 
  geom_sf(data=continents.full, fill=NA, color="lightgray")+
  geom_sf(data=continents.full,  aes(fill=N.sp.null),
          color=NA, linewidth=0.1) +
  scale_fill_gradient2(low="#2166AC",
                       mid="#F7F7F7",
                       high="#B2182B",
                       midpoint = median(continents.full$N.sp.null))+
  labs(tag = "(b) Null model")+
  theme(
    axis.line = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_line(linetype = "dashed", linewidth = 0.5, color="#dab27f"),
    plot.background = element_rect(fill="#fde7c0"),
    panel.background = element_rect(fill="#fde7c0"),
    legend.background = element_rect(fill = "#fde7c0", color = NA),
    legend.title = element_blank(),
    legend.position="bottom",
    legend.key.width = unit(1, 'cm'),
    plot.title = element_text(hjust = 0.5),
    plot.tag.position = c(0.01, 0.99), 
    plot.tag = element_text(hjust = 0, size = 12)
  )

p3<-ggplot()+ 
  geom_sf(data=continents.full, fill=NA, color="lightgray")+
  geom_sf(data=continents.full,  aes(fill=N.iucn),
          color=NA, linewidth=0.1) +
  scale_fill_gradient2(low="#2166AC",
                       mid="#F7F7F7",
                       high="#B2182B",
                       midpoint = median(continents.full$N.iucn))+
  labs(tag = "(c) IUCN")+
  theme(
    axis.line = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_line(linetype = "dashed", linewidth = 0.5, color="#dab27f"),
    plot.background = element_rect(fill="#fde7c0"),
    panel.background = element_rect(fill="#fde7c0"),
    legend.background = element_rect(fill = "#fde7c0", color = NA),
    legend.title = element_blank(),
    legend.position="bottom",
    legend.key.width = unit(1, 'cm'),
    plot.title = element_text(hjust = 0.5),
    plot.tag.position = c(0.01, 0.99), 
    plot.tag = element_text(hjust = 0, size = 12)
  )

cor(continents.full$N.sp.null, continents.full$N.sp.sim)
cor(continents.full$N.iucn, continents.full$N.sp.sim)
cor(continents.full$N.iucn, continents.full$N.sp.null)
p<-p1+p2+p3
ggsave(p, filename="../Figures/Figure.Richness/Figure.Richness.pdf", width=15, height=5)
ggsave(p, filename="../Figures/Figure.Richness/Figure.Richness.png", width=15, height=5)

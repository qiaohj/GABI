library(data.table)
library(ggplot2)
library(sf)
library(stringr)
setDTthreads(30)

setwd("/media/huijieqiao/Butterfly/GABI/GABI")
source("Figures/common.r")
if (F){
  seeds.all<-readRDS("../Data/Tables/random.seeds.threshold.by.nb.distance.rda")
  species.dis<-readRDS("../Data/Tables/Final.Distribution.Unique.rda")
  
  cells<-read_sf("../Shape/isea3h8/N_S_America.shp")
  cells<-data.table(global_id=cells$seqnum, seed_continent=cells$continent)
  species.dis.seed<-merge(species.dis, cells, by.x="seed_id", by.y="global_id")
  cells<-read_sf("../Shape/isea3h8/N_S_America.shp")
  
  cells<-data.table(global_id=cells$seqnum, continent=cells$continent)
  
  species.dis.seed.continent<-merge(species.dis.seed, cells, 
                                    by.x="global_id", 
                                    by.y="global_id")
  head(species.dis.seed.continent)
  species.dis.seed.continent$type<-
    ifelse(species.dis.seed.continent$continent==species.dis.seed.continent$seed_continent, 
           "Aborigines", "Invader")
  rep.list<-list()
  rep.list.all<-list()
  for (rrrr in c(1:100)){
    print(rrrr)
    seeds<-seeds.all[rep==rrrr]
    item<-species.dis.seed.continent[seed_id %in% seeds$seed_id]
    type.N<-item[, .(N.species=length(unique(sp_id))),
                 by=list(type, nb, da, global_id, continent)]
    
    type.N$rep<-rrrr
    rep.list[[rrrr]]<-type.N
    type.N.sum<-type.N[, .(N.species=sum(N.species)),
                       by=list(type, global_id, continent, rep)]
    
    rep.list.all[[rrrr]]<-type.N.sum
    
  }
  rep.df<-rbindlist(rep.list)
  rep.df.all<-rbindlist(rep.list.all)
  saveRDS(rep.df, "../Data/Tables/biome.N.species.by.nb.cell.rda")
  saveRDS(rep.df.all, "../Data/Tables/biome.N.species.cell.rda")
  
  
}

rep.df.all<-readRDS("../Data/Tables/biome.N.species.cell.rda")
Aborigines<-rep.df.all[type=="Aborigines"]
colnames(Aborigines)[5]<-"N.Aborigines"
Aborigines$type<-NULL
Invader<-rep.df.all[type=="Invader"]
colnames(Invader)[5]<-"N.Invader"
Invader$type<-NULL
N.merge<-merge(Aborigines, Invader, 
               by=c("global_id", "rep", "continent"), all=T)

N.merge[is.na(N.Aborigines), N.Aborigines:=0]

N.merge[is.na(N.Invader), N.Invader:=0]

N.merge$Invader_per<-N.merge$N.Invader/(N.merge$N.Invader+N.merge$N.Aborigines)
range(N.merge$Invader_per)

N.merge.mean<-N.merge[, .(N.Aborigines=mean(N.Aborigines), sd.N.Aborigines=sd(N.Aborigines),
                          N.Invader=mean(N.Invader), sd.N.Invader=sd(N.Invader),
                          Invader_per=mean(Invader_per), sd.Invader_per=sd(Invader_per)),
                      by=list(global_id, continent)]
N.merge.mean[,c("global_id", "Invader_per", "sd.Invader_per")]

cells<-read_sf("../Shape/isea3h8/N_S_America.shp")
cells.invader.pre<-merge(cells, N.merge.mean, by.x="seqnum", by.y="global_id")
cells.invader.pre[which(is.na(cells.invader.pre$Invader_per)),]

write_sf(cells.invader.pre, "../Figures/Invader.Per/Invader.Per.shp")
p<-ggplot()+
  geom_sf(data=cells, fill=NA, color="lightgrey")+
  geom_sf(data=cells.invader.pre, aes(fill=Invader_per), color=NA)+
  scale_fill_gradient2(high=color_high, mid=color_mid2, low=color_low, midpoint = 0.5)+
  theme_bw()+
  labs(fill="Immigrant percentage")+
  theme(legend.position = "bottom")+
  guides(fill = guide_colorbar(barwidth = unit(6, "cm")))
p
ggsave(p, filename="../Figures/Invader.Per/Invader.Per.pdf", width=6, height=6)
ggsave(p, filename="../Figures/Invader.Per/Invader.Per.png", width=6, height=6, bg="white")

rep.df<-readRDS("../Data/Tables/biome.N.species.by.nb.cell.rda")


Aborigines<-rep.df[type=="Aborigines"]
colnames(Aborigines)[6]<-"N.Aborigines"
Aborigines$type<-NULL
Invader<-rep.df[type=="Invader"]
colnames(Invader)[6]<-"N.Invader"
Invader$type<-NULL
N.merge<-merge(Aborigines, Invader, 
               by=c("global_id", "rep", "continent", "nb", "da"), all=T)

N.merge[is.na(N.Aborigines), N.Aborigines:=0]

N.merge[is.na(N.Invader), N.Invader:=0]

N.merge$Invader_per<-N.merge$N.Invader/(N.merge$N.Invader+N.merge$N.Aborigines)


N.merge.mean<-N.merge[, .(N.Aborigines=mean(N.Aborigines), sd.N.Aborigines=sd(N.Aborigines),
                          N.Invader=mean(N.Invader), sd.N.Invader=sd(N.Invader),
                          Invader_per=mean(Invader_per), sd.Invader_per=sd(Invader_per)),
                      by=list(global_id, continent, nb, da)]

cells<-read_sf("../Shape/isea3h8/N_S_America.shp")
cells.invader.pre<-merge(cells, N.merge.mean, by.x="seqnum", by.y="global_id")
cells.invader.pre[which(is.na(cells.invader.pre$Invader_per)),]

write_sf(cells.invader.pre, "../Figures/Invader.Per/Invader.Per.nb.da.shp")

cells.invader.pre$nb<-factor(cells.invader.pre$nb, 
                     levels = c("BROAD", "BIG", "MODERATE", "NARROW"), 
                     labels = c("BROAD", "MODERATE", "NARROW", "TINY"))
unique(cells.invader.pre$nb)
cells.invader.pre[is.na(cells.invader.pre$nb),]
p<-ggplot()+
  geom_sf(data=cells, fill=NA, color="lightgrey")+
  geom_sf(data=cells.invader.pre, aes(fill=Invader_per), color=NA)+
  scale_fill_gradient2(high=color_high, mid=color_mid2, low=color_low, midpoint = 0.5)+
  scale_x_continuous(guide = guide_axis(check.overlap = TRUE)) +
  theme_bw()+
  facet_grid(da~nb)+
  labs(fill="Immigrant percentage")+
  theme(legend.position = "bottom")+
  guides(fill = guide_colorbar(barwidth = unit(10, "cm")))
p
ggsave(p, filename="../Figures/Invader.Per/Invader.Per.NB.DA.pdf", width=15, height=6)
ggsave(p, filename="../Figures/Invader.Per/Invader.Per.NB.DA.png", width=15, height=6, bg="white")

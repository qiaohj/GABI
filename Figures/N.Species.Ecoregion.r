library(data.table)
library(ggplot2)
library(sf)
library(dplyr)
sf_use_s2(FALSE)
setwd("/media/huijieqiao/Butterfly/GABI/GABI")

if (F){
  ecoregion<-read_sf("../Shape/Ecoregions2017/Ecoregions2017.shp")
  ecoregion_meter <- st_transform(ecoregion, crs = 3035)
  ecoregion.sim <- st_simplify(ecoregion_meter, preserveTopology = TRUE, dTolerance = 10000)
  ecoregion.sim.lat <- st_transform(ecoregion.sim, crs = st_crs(ecoregion))
  write_sf(ecoregion.sim.lat, "../Shape/Ecoregions2017/Ecoregions2017.simpify.shp")
  ecoregion.sim.lat<-read_sf("../Shape/Ecoregions2017/Ecoregions2017.simpify.shp")
  ecoregion_group <- ecoregion_meter %>%
    group_by(ECO_NAME, REALM) %>%
    summarise(
      geometry = st_union(geometry) 
    ) %>%
    ungroup()
  
  
  ecoregion.lat <- st_transform(ecoregion_group, crs = st_crs(ecoregion))
  write_sf(ecoregion.lat, "../Shape/Ecoregions2017/ecoregion.shp")
  ecoregion_group_sim<-st_simplify(ecoregion_group, preserveTopology = TRUE, dTolerance = 10000)
  ecoregion.lat <- st_transform(ecoregion_group_sim, crs = st_crs(ecoregion))
  
  write_sf(ecoregion.lat, "../Shape/Ecoregions2017/ecoregion.simplify.shp")
  
  
  hexagon<-readRDS("../Data/cells.with.dist.rda")
  cells<-data.table(global_id=as.numeric(hexagon$seqnum), continent=hexagon$continent,
                    lon=hexagon$lon, lat=hexagon$lat)
  ecoregion<-read_sf("../Shape/Ecoregions2017/ecoregion.simplify.shp")
  
  cells <- st_as_sf(
    cells, 
    coords = c("lon", "lat"), 
    crs = 4326 
  )
  
  cells.ecoregion <- st_join(
    cells, 
    ecoregion, 
    join = st_intersects, 
    left = F
  )
  
  
  species.dis<-readRDS("../Data/Tables/Final.Distribution.rda")
  species.dis.geo<-merge(species.dis, cells.ecoregion, by="global_id")
  
  species.dis.geo$geometry<-NULL
  
  seeds<-data.table(seed_id=as.numeric(hexagon$seqnum), seed_continent=hexagon$continent)
  species.dis.geo<-merge(species.dis.geo, seeds, by="seed_id")
  
  
  species.dis.geo$type<-ifelse(species.dis.geo$continent==species.dis.geo$seed_continent, 
                               "Aborigines", "Invader")
  
  saveRDS(species.dis.geo, "../Data/Tables/species.dis.ecoregion.rda")
  
}
species.dis.geo<-readRDS("../Data/Tables/species.dis.ecoregion.rda")
#species.dis.geo[continent %in% c("bridge1", "bridge2"), 
#                continent:="North America"]
#species.dis.geo[global_id %in% c(9580, 9662,9744,9663,9745,9664), continent:="South America"]


#species.dis.geo[global_id %in% sa.bridge2, continent:="South America"]
seeds.all<-readRDS("../Data/Tables/random.seeds.threshold.by.nb.distribution.rda")

rep.list<-list()
rep.list.all<-list()
for (rrrr in c(1:100)){
  print(rrrr)
  seeds<-seeds.all[rep==rrrr]
  item<-species.dis.geo[seed_id %in% seeds$seed_id]
  type.N<-item[, .(N.species=length(unique(sp_id))),
                          by=list(type, nb, da, ECO_NAME, continent)]
  
  type.N<-type.N[!is.na(ECO_NAME)]
  type.N<-type.N[ECO_NAME!="N/A"]
  type.N$rep<-rrrr
  rep.list[[rrrr]]<-type.N
  type.N.sum<-type.N[, .(N.species=sum(N.species)),
                     by=list(type, ECO_NAME, continent, rep)]
  
  rep.list.all[[rrrr]]<-type.N.sum
    
}
rep.df<-rbindlist(rep.list)
rep.list.all<-rbindlist(rep.list.all)

Aborigines<-rep.df[type=="Aborigines"]
colnames(Aborigines)[6]<-"N.Aborigines"
Aborigines$type<-NULL
Invader<-rep.df[type=="Invader"]
colnames(Invader)[6]<-"N.Invader"
Invader$type<-NULL

N.merge<-merge(Aborigines, Invader, 
               by=c("nb", "da", "ECO_NAME", "rep", "continent"), all=T)

N.merge[is.na(N.Aborigines), N.Aborigines:=0]

N.merge[is.na(N.Invader), N.Invader:=0]

N.merge$Invader_per<-N.merge$N.Invader/(N.merge$N.Invader+N.merge$N.Aborigines)
N.merge.mean<-N.merge[, .(N.Aborigines=mean(N.Aborigines), sd.N.Aborigines=sd(N.Aborigines),
                          N.Invader=mean(N.Invader), sd.N.Invader=sd(N.Invader),
                          Invader_per=mean(Invader_per), sd.Invader_per=sd(Invader_per)),
                      by=list(nb, da, ECO_NAME, continent)]

#N.merge.mean<-N.merge.mean[N.Invader>10]
p<-ggplot(N.merge.mean)+geom_point(aes(x=ECO_NAME, y=Invader_per))+
  geom_errorbar(aes(x=ECO_NAME, 
                    ymin = Invader_per-sd.Invader_per, 
                    ymax=Invader_per+sd.Invader_per), width=0.5)+
  geom_hline(yintercept = 0.5, linetype=2)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  facet_grid(nb+da~continent)
p
ggsave(p, filename="../Figures/ecoregion_invader_da_nb.pdf", width=8, height=10)

Aborigines<-rep.list.all[type=="Aborigines"]
colnames(Aborigines)[5]<-"N.Aborigines"
Aborigines$type<-NULL
Invader<-rep.list.all[type=="Invader"]
colnames(Invader)[5]<-"N.Invader"
Invader$type<-NULL
N.merge<-merge(Aborigines, Invader, 
               by=c("ECO_NAME", "rep", "continent"), all=T)

N.merge[is.na(N.Aborigines), N.Aborigines:=0]

N.merge[is.na(N.Invader), N.Invader:=0]

N.merge$Invader_per<-N.merge$N.Invader/(N.merge$N.Invader+N.merge$N.Aborigines)
N.merge.mean<-N.merge[, .(N.Aborigines=mean(N.Aborigines), sd.N.Aborigines=sd(N.Aborigines),
                          N.Invader=mean(N.Invader), sd.N.Invader=sd(N.Invader),
                          Invader_per=mean(Invader_per), sd.Invader_per=sd(Invader_per)),
                      by=list(ECO_NAME, continent)]

#N.merge.mean<-N.merge.mean[N.Invader>10]

ecoregion<-read_sf("../Shape/Ecoregions2017/ecoregion.simplify.shp")
ecoregion<-ecoregion[which(ecoregion$REALM %in% c("Nearctic", "Neotropic")),]
ecoregion$area<-st_area(ecoregion)

ecoregion.n<-merge(ecoregion, N.merge.mean, by="ECO_NAME")
source("Figures/common.r")
p<-ggplot(ecoregion)+
  geom_sf(fill="lightgrey", color="grey", linewidth=0.5)+
  geom_sf(data=ecoregion.n[which(ecoregion.n$REALM %in% c("Nearctic", "Neotropic") &
                                   ecoregion.n$continent %in% c("North America", "South America")),], 
          aes(fill=Invader_per))+
  scale_fill_gradient2(low=color_low,
                       mid=color_mid,
                       high=color_high,
                       midpoint = 0.5,
                       limits=c(0,1))+
  coord_sf(crs=map_crs)+
  map_theme
p
ggsave(p, filename="../Figures/ecoregion_invader_map.pdf", width=10, height=8)

p<-ggplot(N.merge.mean)+geom_point(aes(x=ECO_NAME, y=Invader_per))+
  geom_errorbar(aes(x=ECO_NAME, 
                    ymin = Invader_per-sd.Invader_per, 
                    ymax=Invader_per+sd.Invader_per), width=0.5)+
  geom_hline(yintercept = 0.5, linetype=2)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  facet_wrap(~continent)
p
ggsave(p, filename="../Figures/ecoregion_invader.pdf", width=8, height=6)

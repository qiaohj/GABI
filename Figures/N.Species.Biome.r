library(data.table)
library(ggplot2)
library(sf)
library(dplyr)
sf_use_s2(FALSE)
setwd("/media/huijieqiao/Butterfly/GABI/GABI")

if (F){
  biome<-read_sf("../Shape/Ecoregions2017/Ecoregions2017.shp")
  biome_meter <- st_transform(biome, crs = 3035)
  biome.sim <- st_simplify(biome_meter, preserveTopology = TRUE, dTolerance = 10000)
  biome.sim.lat <- st_transform(biome.sim, crs = st_crs(biome))
  write_sf(biome.sim.lat, "../Shape/Ecoregions2017/Ecoregions2017.simpify.shp")
  biome_group <- biome_meter %>%
    group_by(BIOME_NAME) %>%
    summarise(
      geometry = st_union(geometry) 
    ) %>%
    ungroup()
  
  
  biome.lat <- st_transform(biome_group, crs = st_crs(biome))
  write_sf(biome.lat, "../Shape/Ecoregions2017/biome.shp")
  biome_group_sim<-st_simplify(biome_group, preserveTopology = TRUE, dTolerance = 10000)
  biome.lat <- st_transform(biome_group_sim, crs = st_crs(biome))
  
  write_sf(biome.lat, "../Shape/Ecoregions2017/biome.simplify.shp")
  
  
  hexagon<-readRDS("../Data/cells.with.dist.rda")
  cells<-data.table(global_id=as.numeric(hexagon$seqnum), continent=hexagon$continent,
                    lon=hexagon$lon, lat=hexagon$lat)
  biome<-read_sf("../Shape/Ecoregions2017/biome.simplify.shp")
  
  cells <- st_as_sf(
    cells, 
    coords = c("lon", "lat"), 
    crs = 4326 
  )
  
  cells.biome <- st_join(
    cells, 
    biome, 
    join = st_intersects, 
    left = F
  )
  
  
  species.dis<-readRDS("../Data/Tables/Final.Distribution.rda")
  species.dis.geo<-merge(species.dis, cells.biome, by="global_id")
  
  species.dis.geo$geometry<-NULL
  
  seeds<-data.table(seed_id=as.numeric(hexagon$seqnum), seed_continent=hexagon$continent)
  species.dis.geo<-merge(species.dis.geo, seeds, by="seed_id")
  
  
  species.dis.geo$type<-ifelse(species.dis.geo$continent==species.dis.geo$seed_continent, "Aborigines", "Invader")
  
  saveRDS(species.dis.geo, "../Data/Tables/species.dis.biome.rda")
  
}
species.dis.geo<-readRDS("../Data/Tables/species.dis.biome.rda")
species.dis.geo[continent %in% c("bridge1", "bridge2"), 
                continent:="North America"]

seeds.all<-readRDS("../Data/Tables/random.seeds.rda")

rep.list<-list()
rep.list.all<-list()
for (rrrr in c(1:10)){
  print(rrrr)
  seeds<-seeds.all[rep==rrrr]
  item<-species.dis.geo[seed_id %in% seeds$seed_id]
  type.N<-item[, .(N.species=length(unique(sp_id))),
                          by=list(type, nb, da, BIOME_NAME, continent)]
  
  type.N<-type.N[!is.na(BIOME_NAME)]
  type.N<-type.N[BIOME_NAME!="N/A"]
  type.N$rep<-rrrr
  rep.list[[rrrr]]<-type.N
  type.N.sum<-type.N[, .(N.species=sum(N.species)),
                     by=list(type, BIOME_NAME, continent, rep)]
  
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
               by=c("nb", "da", "BIOME_NAME", "rep", "continent"), all=T)

N.merge[is.na(N.Aborigines), N.Aborigines:=0]

N.merge[is.na(N.Invader), N.Invader:=0]

N.merge$Invader_per<-N.merge$N.Invader/(N.merge$N.Invader+N.merge$N.Aborigines)
N.merge.mean<-N.merge[, .(N.Aborigines=mean(N.Aborigines), sd.N.Aborigines=sd(N.Aborigines),
                          N.Invader=mean(N.Invader), sd.N.Invader=sd(N.Invader),
                          Invader_per=mean(Invader_per), sd.Invader_per=sd(Invader_per)),
                      by=list(nb, da, BIOME_NAME, continent)]

N.merge.mean<-N.merge.mean[N.Invader>10]
ggplot(N.merge.mean)+geom_point(aes(x=BIOME_NAME, y=Invader_per))+
  geom_errorbar(aes(x=BIOME_NAME, ymin = Invader_per-sd.Invader_per, ymax=Invader_per+sd.Invader_per))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  facet_grid(nb+da~continent)



Aborigines<-rep.list.all[type=="Aborigines"]
colnames(Aborigines)[5]<-"N.Aborigines"
Aborigines$type<-NULL
Invader<-rep.list.all[type=="Invader"]
colnames(Invader)[5]<-"N.Invader"
Invader$type<-NULL
N.merge<-merge(Aborigines, Invader, 
               by=c("BIOME_NAME", "rep", "continent"), all=T)

N.merge[is.na(N.Aborigines), N.Aborigines:=0]

N.merge[is.na(N.Invader), N.Invader:=0]

N.merge$Invader_per<-N.merge$N.Invader/(N.merge$N.Invader+N.merge$N.Aborigines)
N.merge.mean<-N.merge[, .(N.Aborigines=mean(N.Aborigines), sd.N.Aborigines=sd(N.Aborigines),
                          N.Invader=mean(N.Invader), sd.N.Invader=sd(N.Invader),
                          Invader_per=mean(Invader_per), sd.Invader_per=sd(Invader_per)),
                      by=list(BIOME_NAME, continent)]

N.merge.mean<-N.merge.mean[N.Invader>10]
ggplot(N.merge.mean)+geom_point(aes(x=BIOME_NAME, y=Invader_per))+
  geom_errorbar(aes(x=BIOME_NAME, ymin = Invader_per-sd.Invader_per, ymax=Invader_per+sd.Invader_per))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  facet_wrap(~continent)

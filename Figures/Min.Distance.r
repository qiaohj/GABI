library(data.table)
library(ggplot2)
library(ggrepel)
library(ggh4x)
library(sf)
setwd("/media/huijieqiao/Butterfly/GABI/GABI")
source("Figures/common.r")
seed.dist<-readRDS("../Data/cells.with.dist.rda")

seed.dist[which(seed.dist$min.dist==0), "min.dist"]<-NA
range.v<-range(seed.dist$min.dist, na.rm=T)
p<-ggplot(seed.dist)+
  geom_sf(data=seed.dist, fill=NA, color="lightgrey")+
  geom_sf(data=seed.dist[which(!is.na(seed.dist$min.dist)),],
          aes(fill=min.dist), color="grey")+
  scale_fill_gradient2(low=color_low,
                       mid=color_mid,
                       high=color_high,
                       midpoint = mean(seed.dist$min.dist, na.rm=T),
                       limits=range.v)+
  coord_sf(crs = map_crs)+
  map_theme
  
p

ggsave(p, filename="../Figures/min.dist.to.other.continent.pdf", width=5, height=7)

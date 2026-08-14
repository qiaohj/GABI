library(data.table)
library(ggplot2)
library(sf)
library(dplyr)
library(ggrepel)
library(ggforce)
library(viridis)
library(scales)

sf_use_s2(FALSE)
setwd("/media/huijieqiao/Butterfly/GABI/GABI")
species.dis<-readRDS("../Data/Tables/Final.Distribution.NULL.Unique.rda")

FULL.N_SP<-species.dis[,.(N_SP=length(unique(sp_label))), by=list(seed_id, label, nb, da)]

FULL.N_SP$bins<-FULL.N_SP$N_SP
result <- table(cut(FULL.N_SP$N_SP, 
                    breaks = c(0,1, 10, 100, 1000, 10000, Inf), 
                    labels = c("1", "2-10", "11-100", "101-1000", "1001-10000", ">10000")))

sum(FULL.N_SP[N_SP>1000]$N_SP)
sum(FULL.N_SP[N_SP<1000]$N_SP)
# 查看结果
print(result)

setorderv(FULL.N_SP, "N_SP", -1)
FULL.N_SP
ggplot(FULL.N_SP)+geom_histogram(aes(x=N_SP))+
  scale_x_log10()
seeds.all<-readRDS("../Data/Tables/random.seeds.threshold.by.nb.distance.99.rda")
species.dis.df<-species.dis[label %in% seeds.all$seed_label]
species.dis.geo<-readRDS("../Data/Tables/species.dis.biome.rda")
species.dis.geo$label<-sprintf("%d.%s.%s", species.dis.geo$seed_id, species.dis.geo$nb, species.dis.geo$da)

cells<-read_sf("../Shape/isea3h8/N_S_America.shp")
ll<-data.table(global_id=cells$seqnum, continent=cells$continent, lon=cells$lon, lat=cells$lat)

species.dis.df.ll<-merge(species.dis.df, ll, by="global_id")
ll<-data.table(seed_id=cells$seqnum, seed_continent=cells$continent, seed.lon=cells$lon, seed.lat=cells$lat)
species.dis.df.ll.seeds<-merge(species.dis.df.ll, ll, by="seed_id")
species.dis.df.ll.seeds$lat_bin<-floor((species.dis.df.ll.seeds$lat+0.5)/1)*1

xxx<-species.dis.df.ll.seeds[seed_id==11030 & nb=="MODERATE" & da=="GOOD"]

range(xxx$lat_bin)

high_lat<-species.dis.df.ll.seeds[lat_bin<=-50]
N_SP<-high_lat[,.(N_SP=length(unique(sp_label))), 
               by=list(seed_id, nb, da, label, continent, seed_continent, lat_bin)]

N_Simulation<-high_lat[,.(N_Simulation=length(unique(label))),
                       by=list(seed_id, nb, da, label, continent, seed_continent)]


N_SP[N_SP>100]
N_LAT_SP<-high_lat[,.(N_SP=length(unique(sp_label))), by=list(lat_bin, continent, seed_continent)]

N_LAT_Simulation<-high_lat[,.(N_Simulation=length(unique(label))), 
                           by=list(lat_bin, continent, seed_continent)]

N_SP_SE<-N_SP[,.(N_SP=mean(N_SP), N_SP_SD=sd(N_SP)), by=list(lat_bin, seed_continent)]
ggplot(N_SP_SE)+geom_point(aes(x=factor(lat_bin), y=N_SP, color=seed_continent))+
  geom_errorbar(aes(x=factor(lat_bin), ymin=N_SP-N_SP_SD, ymax=N_SP+N_SP_SD, color=seed_continent))

library(data.table)
library(ggplot2)
library(sf)
library(ggh4x)
library(dplyr)
setDTthreads(30)
setwd("/media/huijieqiao/Butterfly/GABI/GABI")
source("Figures/common.r")
rep.df.all<-readRDS("../Data/Tables/Lat.low.high.without.bridges.rda")
rep.df<-readRDS("../Data/Tables/Lat.low.high.without.bridges.nb.da.rda")

rep.df.all$Per<-rep.df.all$N_SP/rep.df.all$N_SP_ALL

rep.df.all.mean<-rep.df.all[,.(Per=mean(Per), sd=sd(Per)), by=list(lat_threshold, type, continent)]

rep.df.all2<-readRDS("../Data/Tables/Lat.low.mid.high.without.bridges.rda")

rep.df.all2$Per<-rep.df.all2$N_SP/rep.df.all2$N_SP_ALL

rep.df.all2.mean<-rep.df.all2[,.(Per=mean(Per), sd=sd(Per)), by=list(lat_threshold2, type, continent)]


5781.12/(5781.12+56200.06)
2009.29/(2009.29+83199.57)

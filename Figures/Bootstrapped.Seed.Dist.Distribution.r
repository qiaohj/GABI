library(data.table)
library(ggplot2)
library(ggrepel)
library(ggh4x)
library(sf)
setwd("/media/huijieqiao/Butterfly/GABI/GABI")
seeds.all<-readRDS("../Data/Tables/random.seeds.threshold.by.nb.distribution.rda")

seeds.r<-seeds.all[rep==1]
ggplot(seeds.all)+
  geom_histogram(aes(x=min.dist, fill=continent), binwidth = 1, position = "identity", 
                 alpha = 0.5, color="white")+
  geom_vline(xintercept = 70, linetype=2)+
  theme_bw()
seeds.all2<-readRDS("../Data/Tables/random.seeds.threshold.by.nb.distribution.threshold.rda")

seeds.r2<-seeds.all2[rep==1]
ggplot(seeds.all2)+
  geom_histogram(aes(x=min.dist, fill=continent), binwidth = 1, position = "identity", 
                 alpha = 0.5, color="white")+
  geom_vline(xintercept = 70, linetype=2)+
  theme_bw()

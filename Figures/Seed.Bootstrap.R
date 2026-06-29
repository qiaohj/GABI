library(data.table)
library(ggplot2)
library(sf)
library(stringr)
library(RSQLite)
library(DBI)
library(ggnewscale)

setDTthreads(30)

setwd("/media/huijieqiao/Butterfly/GABI/GABI")
source("Figures/common.r")
cell.dist<-readRDS("../Data/Tables/cells.with.dist.rda")
seeds<-readRDS("../Data/Tables/random.seeds.threshold.by.nb.distance.rda")
seeds.unique<-unique(seeds[,c("seed_id", "min.dist", "continent")])

seeds$label<-NULL
seeds$label2<-NULL
seeds$N_SPECIES<-NULL
seeds<-unique(seeds)
seeds.N<-seeds[,.(N=.N), by=list(min.dist, continent, nb, da, rep)]
seeds.N.se<-seeds.N[,.(N=mean(N), sd=sd(N)), by=list(min.dist, continent, nb)]

seeds.N.se$nb<-factor(seeds.N.se$nb, 
                                    levels = c("BROAD", "BIG", "MODERATE", "NARROW"), 
                                    labels = c("BROAD", "MODERATE", "NARROW", "TINY"))
p1<-ggplot(seeds.unique)+
  geom_histogram(aes(x=min.dist, fill=continent), position = "identity", binwidth=1,
                                 alpha = 0.3, color="white")+
  geom_line(data=seeds.N.se, aes(x = min.dist, y=N, color=nb), 
               linetype = "solid", position = "identity") +
  labs(x="Minimal distance to the other continent", fill="Continent", color="Niche Breadth",
       y="Number of seeds")+
  scale_fill_manual(values=c("South America"=color_low, "North America"=color_high))+
  scale_color_manual(values=c("BROAD"="#D55E00", "MODERATE"="#44AA99",
                             "NARROW"="#AA4499", "TINY"="#F0E442"))+
  
  theme_bw()
p1

seeds.N.full<-seeds[,.(N=.N), by=list(seed_id)]
seeds.N.full<-merge(cell.dist, seeds.N.full, by.y="seed_id", by.x="seqnum")
p2<-ggplot()+
  geom_sf(data=cell.dist, fill=NA, color="lightgrey", alpha=0.3)+
  geom_sf(data=cell.dist[which(cell.dist$min.dist>0),], aes(fill=min.dist), color=NA)+
  scale_fill_gradient(
    low = "lightgrey",
    high = color_high,
    na.value = "transparent",
    name = "Distance to the other continent"
  ) + 
  new_scale_fill() +
  geom_sf(data=seeds.N.full, aes(fill=N), color=NA)+
  scale_fill_gradient(
    low = "lightgrey",
    high = color_low,
    na.value = "transparent",
    name = "Number of seeds"
  ) +
  scale_x_continuous(guide = guide_axis(check.overlap = TRUE)) +
  theme_bw() +
  theme(legend.position = "right")
p2

p<-p1+p2
ggsave(p, filename="../Figures/Seed.Bootstrap/Seed.Bootstrap.pdf", width=10, height=5)
ggsave(p, filename="../Figures/Seed.Bootstrap/Seed.Bootstrap.png", width=10, height=5, bg="white")

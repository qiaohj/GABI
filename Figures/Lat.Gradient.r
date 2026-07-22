library(data.table)
library(ggplot2)
library(sf)
library(ggh4x)
library(dplyr)
setDTthreads(30)
setwd("/media/huijieqiao/Butterfly/GABI/GABI")
source("Figures/common.r")
if (F){
  df<-readRDS("../Data/Tables/Lat.N.1defree.without.bridges.rda")
  df<-df[year==0]
  df$seed_id<-as.numeric(df$seed_id)
  dim(df)
  table(df$lat_bin)
  
  seeds.all<-readRDS("../Data/Tables/random.seeds.threshold.by.nb.distance.rda")
  cells<-readRDS("../Data/seeds.rda")
  df<-merge(df, cells, by.x="seed_id", by.y="global_id")
  colnames(df)[7]<-"seed_continent"
  threshold<-13.5
  df$continent<-ifelse(df$lat_bin>threshold, "North America", "South America")
  df[continent!=seed_continent]
  df$label<-sprintf("%d.%s.%s", df$seed_id, df$nb, df$da)
  rep.list<-list()
  rep.list.all<-list()
  rep.list.full<-list()
  
  for (rrrr in c(1:100)){
    print(rrrr)
    seeds<-seeds.all[rep==rrrr]
    
    item<-df[label %in% seeds$label]
    item<-item[,.(N_SP=sum(N_SP), rep=rrrr), by=list(lat_bin)]
    rep.list.full[[rrrr]]<-item
    
    item<-df[label %in% seeds$label]
    item<-item[,.(N_SP=sum(N_SP), rep=rrrr), by=list(seed_continent, continent, lat_bin, nb, da)]
    item$type<-"Native"
    item[seed_continent!=continent, type:="Immigrant"]
    rep.list[[rrrr]]<-item
    
    item<-df[label %in% seeds$label]
    item<-item[,.(N_SP=sum(N_SP), rep=rrrr), by=list(seed_continent, continent, lat_bin)]
    item$type<-"Native"
    item[seed_continent!=continent, type:="Immigrant"]
    rep.list.all[[rrrr]]<-item
    

  }
  rep.df<-rbindlist(rep.list)
  rep.df.all<-rbindlist(rep.list.all)
  rep.df.full<-rbindlist(rep.list.full)
  saveRDS(rep.df.full, "../Data/Tables/N.Sp.Lat.full.rep.rda")
  saveRDS(rep.df, "../Data/Tables/N.Sp.Lat.rep.rda")
  saveRDS(rep.df.all, "../Data/Tables/N.Sp.Lat.all.rep.rda")
}

rep.df<-readRDS("../Data/Tables/N.Sp.Lat.rep.rda")
rep.df.all<-readRDS("../Data/Tables/N.Sp.Lat.all.rep.rda")

lat.all.N.Native<-rep.df.all[type=="Native"]
lat.all.N.Immigrant<-rep.df.all[type=="Immigrant"]
lat.all.N.Per<-merge(lat.all.N.Native, lat.all.N.Immigrant, 
                     by=c("continent", "lat_bin", "rep"),
                     all=T)
colnames(lat.all.N.Per)[c(5, 8)]<-c("N_SP_Native", "N_SP_Immigrant")
lat.all.N.Per[is.na(N_SP_Native), N_SP_Native:=0]
lat.all.N.Per[is.na(N_SP_Immigrant), N_SP_Immigrant:=0]

lat.all.N.Per$N_SP<-lat.all.N.Per$N_SP_Native+lat.all.N.Per$N_SP_Immigrant
lat.all.N.Per$Per_Immigrant<-lat.all.N.Per$N_SP_Immigrant/lat.all.N.Per$N_SP
lat.all.N.Per.SE<-lat.all.N.Per[, .(Per_Immigrant=mean(Per_Immigrant),
                                    Per_Immigrant_SD=sd(Per_Immigrant)),
                                by=c("continent", "lat_bin")]
p1 <- ggplot(lat.all.N.Per.SE, aes(y = lat_bin, x = Per_Immigrant, color=continent)) +
  
  geom_ribbon(aes(xmin = pmax(0, Per_Immigrant - Per_Immigrant_SD), 
                  xmax = Per_Immigrant + Per_Immigrant_SD), 
              alpha = 0.3,
              color = NA) +
  
  geom_line(orientation = "y", linewidth = 1) +
  scale_color_manual(values=c("North America"=color_na,
                              "South America"=color_sa))+
  facet_wrap(~continent, scales="free")+
  facetted_pos_scales(
    y = list(
      continent == "South America" ~ scale_y_reverse()
    )
  ) +
  
  labs(
    x = "Immigrant proportion",
    y = "Latitude"
  ) +
  theme_bw() +
  theme(
    legend.position = "none"
  )

p1  
ggsave(p1, filename="../Figures/Lat.Gradient/Lat.Gradient.Immigrant.proportion.pdf", width=6, height=6)
ggsave(p1, filename="../Figures/Lat.Gradient/Lat.Gradient.Immigrant.proportion.png", width=6, height=6, bg="white")



lat.all.N<-rep.df.all[,.(N_SP=mean(N_SP), sd=sd(N_SP)), 
                      by=list(seed_continent, continent, lat_bin, type)]
lat.all.N$type<-factor(lat.all.N$type, 
                     levels = c("Native", "Immigrant"), 
                     labels = c("Native", "Immigrant"))
p1 <- ggplot(lat.all.N, aes(y = lat_bin, x = N_SP, color = type, fill = type)) +
  
  geom_ribbon(aes(xmin = pmax(0, N_SP - sd), xmax = N_SP + sd), 
              alpha = 0.3,
              color = NA) +
  
  geom_line(orientation = "y", linewidth = 1) +
  
  facet_wrap(~ continent, ncol = 2, scale="free_y") +
  facetted_pos_scales(
    y = list(
      continent == "South America" ~ scale_y_reverse()
    )
  ) +
  scale_color_manual(values=c("Native"=color_native, "Immigrant"=color_immigrant))+
  scale_fill_manual(values=c("Native"=color_native, "Immigrant"=color_immigrant))+
  
  labs(
    x = "Number of species",
    y = "Latitude",
    color = "Species type",
    fill  = "Species type"
  ) +
  theme_bw() +
  theme(
    legend.position = "bottom"
  )
p1
ggsave(p1, filename="../Figures/Lat.Gradient/Lat.Gradient.pdf", width=6, height=6)
ggsave(p1, filename="../Figures/Lat.Gradient/Lat.Gradient.png", width=6, height=6, bg="white")



setorderv(lat.all.N, c("seed_continent", "continent", "lat_bin"))
fwrite(lat.all.N, "../Figures/Lat.Gradient/Lat.Gradient.csv")
to.doc(lat.all.N, "Latitudinal gradient", "../Figures/Lat.Gradient/Lat.Gradient.docx", digits=2)



lat.all.N<-rep.df[,.(N_SP=mean(N_SP), sd=sd(N_SP)), 
                      by=list(seed_continent, continent, lat_bin, type, nb, da)]


lat.all.N$nb<-factor(lat.all.N$nb, 
                     levels = c("BROAD", "BIG", "MODERATE", "NARROW"), 
                     labels = c("BROAD", "MODERATE", "NARROW", "TINY"))
lat.all.N$type<-factor(lat.all.N$type, 
                       levels = c("Native", "Immigrant"), 
                       labels = c("Native", "Immigrant"))
p <- ggplot(lat.all.N, aes(y = lat_bin, x = N_SP, color = type, fill = type)) +
  
  geom_ribbon(aes(xmin = pmax(0, N_SP - sd), xmax = N_SP + sd), 
              alpha = 0.3,
              color = NA) +
  
  geom_line(orientation = "y", linewidth = 1) +
  
  facet_grid(continent~nb+da, scale="free") +
  facetted_pos_scales(
    y = list(
      continent == "South America" ~ scale_y_reverse()
    )
  ) +
  scale_color_manual(values=c("Native"=color_native, "Immigrant"=color_immigrant))+
  scale_fill_manual(values=c("Native"=color_native, "Immigrant"=color_immigrant))+
  
  labs(
    x = "Number of species",
    y = "Latitude",
    color = "Species type",
    fill  = "Species type"
  ) +
  scale_x_continuous(guide = guide_axis(check.overlap = TRUE)) +
  
  
  theme_bw() +
  theme(
    legend.position = "bottom"
  )
p

ggsave(p, filename="../Figures/Lat.Gradient/Lat.Gradient.NB.DA.pdf", width=10, height=6)
ggsave(p, filename="../Figures/Lat.Gradient/Lat.Gradient.NB.DA.png", width=10, height=6, bg="white")
setorderv(lat.all.N, c("nb", "da", "seed_continent", "continent", "lat_bin"))
fwrite(lat.all.N, "../Figures/Lat.Gradient/Lat.Gradient.NB.DA.csv")
to.doc(lat.all.N, "Latitudinal gradient", "../Figures/Lat.Gradient/Lat.Gradient.NB.DA.docx", digits=2)

dis<-readRDS("../Data/Tables/Final.Distribution.Unique.rda")
seeds.all<-readRDS("../Data/Tables/random.seeds.threshold.by.nb.distance.rda")
dis.boot<-dis[seed_id %in% unique(seeds.all$seed_id)]
ll<-readRDS("../Data/Tables/cells.with.dist.rda")
seed.continent<-unique(data.table(seed_id=seeds.all$seed_id, seed_continent=seeds.all$continent))
dis.boot.ll<-merge(dis.boot, ll, by.x="global_id", by.y="seqnum")
dis.boot.ll.seed<-merge(dis.boot.ll, seed.continent, by="seed_id")
dis.boot.ll.seed[,.(max_ll=max(lat), min_ll=min(lat)), 
                 by=c("seed_continent")]

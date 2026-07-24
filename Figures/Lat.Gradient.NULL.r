library(data.table)
library(ggplot2)
library(sf)
library(ggh4x)
library(dplyr)
setDTthreads(30)
setwd("/media/huijieqiao/Butterfly/GABI/GABI")
source("Figures/common.r")
if (F){
  df<-readRDS("../Data/Tables/Lat.N.1defree.without.bridges.NULL.rda")
  df<-df[year==0]
  df$seed_id<-as.numeric(df$seed_id)
  dim(df)
  table(df$lat_bin)
  
  
  seeds.all[seed_id==41164]
  cells<-readRDS("../Data/seeds.rda")
  df<-merge(df, cells, by.x="seed_id", by.y="global_id")
  colnames(df)[7]<-"seed_continent"
  threshold<-13.5
  df$continent<-ifelse(df$lat_bin>threshold, "North America", "South America")
  df[continent!=seed_continent]
  df$label<-sprintf("%d.%s.%s", df$seed_id, df$nb, df$da)
  
  #seeds.all<-readRDS("../Data/Tables/random.seeds.threshold.by.nb.distance.99.rda")
  seeds.all<-readRDS("../Data/Tables/random.seeds.99.rda")
  rep.list<-list()
  rep.list.all<-list()
  rep.list.full<-list()
  
  for (rrrr in c(1:100)){
    print(rrrr)
    seeds<-seeds.all[rep==rrrr]
    
    item<-df[label %in% seeds$label]
    item<-item[,.(N_SP=sum(N_SP), N_Simulation=length(unique(label)), rep=rrrr), by=list(lat_bin)]
    rep.list.full[[rrrr]]<-item
    
    item<-df[label %in% seeds$label]
    item<-item[,.(N_SP=sum(N_SP), N_Simulation=length(unique(label)), rep=rrrr), 
               by=list(seed_continent, continent, lat_bin, nb, da)]
    item$type<-"Native"
    item[seed_continent!=continent, type:="Immigrant"]
    rep.list[[rrrr]]<-item
    
    item<-df[label %in% seeds$label]
    item<-item[,.(N_SP=sum(N_SP), N_Simulation=length(unique(label)), rep=rrrr), by=list(seed_continent, continent, lat_bin)]
    item$type<-"Native"
    item[seed_continent!=continent, type:="Immigrant"]
    rep.list.all[[rrrr]]<-item
  }
  rep.df<-rbindlist(rep.list)
  rep.df.all<-rbindlist(rep.list.all)
  rep.df.full<-rbindlist(rep.list.full)
  saveRDS(rep.df.full, "../Data/Tables/N.Sp.Lat.full.rep.NULL.random.seeds.99.rda")
  saveRDS(rep.df, "../Data/Tables/N.Sp.Lat.rep.NULL.random.seeds.99.rda")
  saveRDS(rep.df.all, "../Data/Tables/N.Sp.Lat.all.rep.NULL.random.seeds.99.rda")
}

rep.df<-readRDS("../Data/Tables/N.Sp.Lat.rep.NULL.random.seeds.99.rda")
rep.df.all<-readRDS("../Data/Tables/N.Sp.Lat.all.rep.NULL.random.seeds.99.rda")

lat.all.N.Native<-rep.df.all[type=="Native"]
lat.all.N.Immigrant<-rep.df.all[type=="Immigrant"]
lat.all.N.Per<-merge(lat.all.N.Native, lat.all.N.Immigrant, 
                     by=c("continent", "lat_bin", "rep"),
                     all=T)
colnames(lat.all.N.Per)[c(5, 6, 9, 10)]<-c("N_SP_Native", "N_Simulation_Native",
                                           "N_SP_Immigrant", "N_Simulation_Immigrant")
lat.all.N.Per[is.na(N_SP_Native), N_SP_Native:=0]
lat.all.N.Per[is.na(N_SP_Immigrant), N_SP_Immigrant:=0]

lat.all.N.Per[is.na(N_Simulation_Native), N_Simulation_Native:=0]
lat.all.N.Per[is.na(N_Simulation_Immigrant), N_Simulation_Immigrant:=0]


lat.all.N.Per$N_SP<-lat.all.N.Per$N_SP_Native+lat.all.N.Per$N_SP_Immigrant
lat.all.N.Per$N_Simulation<-lat.all.N.Per$N_Simulation_Native+lat.all.N.Per$N_Simulation_Immigrant

lat.all.N.Per$Per_Immigrant<-lat.all.N.Per$N_SP_Immigrant/lat.all.N.Per$N_SP
lat.all.N.Per$Per_Immigrant_Simulation<-lat.all.N.Per$N_Simulation_Immigrant/lat.all.N.Per$N_Simulation
lat.all.N.Per.SE<-lat.all.N.Per[, .(Per_Immigrant=mean(Per_Immigrant),
                                    Per_Immigrant_SD=sd(Per_Immigrant),
                                    Per_Immigrant_Simulation=mean(Per_Immigrant_Simulation),
                                    Per_Immigrant_Simulation_SD=sd(Per_Immigrant_Simulation)),
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

p2 <- ggplot(lat.all.N.Per.SE, aes(y = lat_bin, x = Per_Immigrant_Simulation, color=continent)) +
  
  geom_ribbon(aes(xmin = pmax(0, Per_Immigrant_Simulation - Per_Immigrant_Simulation_SD), 
                  xmax = Per_Immigrant_Simulation + Per_Immigrant_Simulation_SD), 
              alpha = 0.3,
              color = NA) +
  
  geom_line(orientation = "y", linewidth = 1) +
  geom_vline(xintercept=0.5, linetype=2)+
  scale_color_manual(values=c("North America"=color_na,
                              "South America"=color_sa))+
  facet_wrap(~continent, scales="free")+
  facetted_pos_scales(
    y = list(
      continent == "South America" ~ scale_y_reverse()
    )
  ) +
  
  labs(
    x = "Immigrant simulation proportion",
    y = "Latitude"
  ) +
  theme_bw() +
  theme(
    legend.position = "none"
  )

p2

ggsave(p1, filename="../Figures/Lat.Gradient/Lat.Gradient.Immigrant.proportion.NULL.pdf", width=6, height=6)
ggsave(p1, filename="../Figures/Lat.Gradient/Lat.Gradient.Immigrant.proportion.NULL.png", width=6, height=6, bg="white")



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
ggsave(p1, filename="../Figures/Lat.Gradient/Lat.Gradient.NULL.pdf", width=6, height=6)
ggsave(p1, filename="../Figures/Lat.Gradient/Lat.Gradient.NULL.png", width=6, height=6, bg="white")



setorderv(lat.all.N, c("seed_continent", "continent", "lat_bin"))
fwrite(lat.all.N, "../Figures/Lat.Gradient/Lat.Gradient.NULL.csv")
to.doc(lat.all.N, "Latitudinal gradient", "../Figures/Lat.Gradient/Lat.Gradient.NULL.docx", digits=2)



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

ggsave(p, filename="../Figures/Lat.Gradient/Lat.Gradient.NB.DA.NULL.pdf", width=10, height=6)
ggsave(p, filename="../Figures/Lat.Gradient/Lat.Gradient.NB.DA.NULL.png", width=10, height=6, bg="white")
setorderv(lat.all.N, c("nb", "da", "seed_continent", "continent", "lat_bin"))
fwrite(lat.all.N, "../Figures/Lat.Gradient/Lat.Gradient.NB.DA.NULL.csv")
to.doc(lat.all.N, "Latitudinal gradient", "../Figures/Lat.Gradient/Lat.Gradient.NB.DA.NULL.docx", digits=2)

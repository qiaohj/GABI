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
    x = "Number of Species",
    y = "Latitude",
    color = "Species Type",
    fill  = "Species Type"
  ) +
  theme_bw() +
  theme(
    legend.position = "bottom",
    strip.text = element_text(size = 12, face = "bold"),
    panel.spacing = unit(1, "lines")
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
    x = "Number of Species",
    y = "Latitude",
    color = "Species Type",
    fill  = "Species Type"
  ) +
  scale_x_continuous(guide = guide_axis(check.overlap = TRUE)) +
  
  
  theme_bw() +
  theme(
    legend.position = "bottom",
    strip.text = element_text(size = 12, face = "bold"),
    panel.spacing = unit(1, "lines")
  )
p

ggsave(p, filename="../Figures/Lat.Gradient/Lat.Gradient.NB.DA.pdf", width=10, height=6)
ggsave(p, filename="../Figures/Lat.Gradient/Lat.Gradient.NB.DA.png", width=10, height=6, bg="white")
setorderv(lat.all.N, c("nb", "da", "seed_continent", "continent", "lat_bin"))
fwrite(lat.all.N, "../Figures/Lat.Gradient/Lat.Gradient.NB.DA.csv")
to.doc(lat.all.N, "Latitudinal gradient", "../Figures/Lat.Gradient/Lat.Gradient.NB.DA.docx", digits=2)


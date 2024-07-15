library(data.table)
library(sf)
library(ggplot2)
library(heatmaply)
setwd("/media/huijieqiao/WD22T_11/GABI/Script")
t_folder<-"500k.speciation.years"
if (F){
  div<-readRDS(sprintf("../Data/Tables/%s/c100.virtual.species.richness.rda", t_folder))
  species.richness<-div[,.(N_SP=sum(N_SP)), by=list(year, continent, global_id)]
  saveRDS(species.richness, sprintf("../Data/Tables/%s/c100.virtual.species.richness.sum.rda",
                                    t_folder))
  
  species<-readRDS(sprintf("../Data/Tables/%s/c100.virtual.species.rda", t_folder))
  N_species<-species[N>0,.(N_Species=length(unique(sp_id))),
                     by=list(year, nb, da, seed_id)]
  saveRDS(N_species, sprintf("../Data/Tables/%s/c100.virtual.N_species.rda",
                                    t_folder))
  
  species_last<-N_species[year==0]
  ggplot(species_last)+geom_histogram(aes(x=N_Species), bins = 30)+
    scale_x_log10()
  
  
  quantile(species_last$N_Species, c(0.9, 0.95, 0.99, 1))
  
  threshold<-1000
  outlier_seed_id<-unique(species_last[N_Species>threshold]$seed_id)
  
  div_sub<-div[!(seed_id %in% outlier_seed_id)]
  species.richness_sub<-div_sub[,.(N_SP=sum(N_SP)), by=list(year, continent, global_id)]
  saveRDS(species.richness_sub, sprintf("../Data/Tables/%s/c100.virtual.species.richness.sum.100_threshold.rda",
                                    t_folder))
  
  dispersal_result<-readRDS(sprintf("../Data/Tables/%s/dispersal_result.rda", t_folder))
  
  dispersal_result$Label<-sprintf("%s.%d.%d.%d.%d", dispersal_result$status,
                                  dispersal_result$init_n, dispersal_result$init_s,
                                  dispersal_result$final_n, dispersal_result$final_s)
  table(dispersal_result$Label)
  
  xx<-dispersal_result[, .(N=.N), by=list(Label, status, nb)]
  xx[nb=="BROAD-BROAD"]
  
  sub_dispersal_result<-dispersal_result[nb=="BROAD-BROAD" & da=="GOOD"]
  
  ns<-read_sf("../Data/Shape/isea3h8/N_S_America.shp")
  
  ns_first<-merge(ns, sub_dispersal_result, by.x="seqnum", by.y="seed_id", all.x=T)
  
  types<-unique(ns_first$Label)
  table(ns_first$Label)
  colors<-c("lightgrey", "yellow", "red", "blue", "yellow", "blue")
  ggplot()+ 
    #geom_sf(data=ns_first[which(ns_first$Label=="Extinct before isthmus.1.0.0.0" | is.na(ns_first$Label)),],  aes(fill=Label),
    geom_sf(data=ns_first,  aes(fill=Label),
            color=NA, linewidth=0.1) +
    scale_fill_manual(values=colors, breaks = types)+
    #scale_fill_viridis_d()+
    theme(
      axis.line = element_blank(),
      #axis.text.x = element_blank(),
      #axis.text.y = element_blank(),
      #axis.ticks = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      panel.grid.major = element_line(linetype = "dashed", linewidth = 0.5, color="#dab27f"),
      #panel.grid.minor = element_line(color="black"),
      plot.background = element_rect(fill="#fde7c0"),
      panel.background = element_rect(fill="#fde7c0"),
      legend.background = element_rect(fill = "#fde7c0", color = NA),
      legend.title = element_blank(),
      #panel.border = element_blank(),
      legend.position="bottom",
      #legend.key.width=unit(0.8,"in"),
      #strip.background.x = element_blank(),
      #strip.text.x = element_blank(),
      plot.title = element_text(hjust = 0.5)
    )
  
}
species.richness<-readRDS(sprintf("../Data/Tables/%s/c100.virtual.species.richness.sum.rda",
                                  t_folder))
ns<-read_sf("../Data/Shape/isea3h8/N_S_America.shp")
first<-species.richness[year==1800]

ns_first<-merge(ns, first, by.x="seqnum", by.y="global_id", all.x=T)
ns_first[is.na(ns_first$N_SP),]$N_SP<-0

last<-species.richness[year==0]
ns_last<-merge(ns, last, by.x="seqnum", by.y="global_id", all.x=T)
ns_last[is.na(ns_last$N_SP),]$N_SP<-0
hist(ns_last$N_SP)
threshold<-round(mean(ns_last$N_SP)+3*sd(ns_last$N_SP))
mycol <- cool_warm(threshold + 1)

max_n_sp<-max(ns_last$N_SP)
min_n_sp<-min(ns_last$N_SP)
if (threshold>max_n_sp){
  midpoint<-round(max_n_sp/2)
  breakss<-c(min_n_sp, midpoint, max_n_sp)
  labelss<-c("0", "", max_n_sp)
}else{
  midpoint<-round(threshold/2)
  breakss<-c(min_n_sp, midpoint, threshold)
  labelss<-c("0", "", sprintf(">%d, up to %d", threshold, max_n_sp))
}


ggplot()+ 
  geom_sf(data=ns_first,  aes(fill=N_SP),
          color=NA, linewidth=0.1) +
  theme(
    axis.line = element_blank(),
    #axis.text.x = element_blank(),
    #axis.text.y = element_blank(),
    #axis.ticks = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_line(linetype = "dashed", linewidth = 0.5, color="#dab27f"),
    #panel.grid.minor = element_line(color="black"),
    plot.background = element_rect(fill="#fde7c0"),
    panel.background = element_rect(fill="#fde7c0"),
    legend.background = element_rect(fill = "#fde7c0", color = NA),
    legend.title = element_blank(),
    #panel.border = element_blank(),
    legend.position="bottom",
    #legend.key.width=unit(0.8,"in"),
    #strip.background.x = element_blank(),
    #strip.text.x = element_blank(),
    plot.title = element_text(hjust = 0.5)
  )

ggplot()+ 
  geom_sf(data=ns_last,  aes(fill=N_SP),
          color=NA, linewidth=0.1) +
  scale_fill_gradient2(low  = mycol[1], high=mycol[length(mycol)],
                        mid = "#DDDDDD", midpoint=midpoint,
                        breaks=breakss, 
                        labels=labelss)+
  
  theme(
    axis.line = element_blank(),
    #axis.text.x = element_blank(),
    #axis.text.y = element_blank(),
    #axis.ticks = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_line(linetype = "dashed", linewidth = 0.5, color="#dab27f"),
    #panel.grid.minor = element_line(color="black"),
    plot.background = element_rect(fill="#fde7c0"),
    panel.background = element_rect(fill="#fde7c0"),
    legend.background = element_rect(fill = "#fde7c0", color = NA),
    legend.title = element_blank(),
    #panel.border = element_blank(),
    legend.position="bottom",
    #legend.key.width=unit(0.8,"in"),
    #strip.background.x = element_blank(),
    #strip.text.x = element_blank(),
    plot.title = element_text(hjust = 0.5)
  )


library(data.table)
library(ggplot2)
library(sf)
library(stringr)
setDTthreads(30)

setwd("/media/huijieqiao/Butterfly/GABI/GABI")
source("Figures/common.r")
if (F){
  sp.with.bridge<-readRDS("../Data/Tables/sp_full_continents.rda")
  N<-sp.with.bridge[,.(N_SP=length(unique(species.label))), 
        by=list(seed_id, seed_continent, current_continent, NB, DA)]
  N$label<-sprintf("%d.%s.%s", N$seed_id, N$NB, N$DA)
  
  seeds.all<-readRDS("../Data/Tables/random.seeds.threshold.by.nb.distance.rda")
  rep.list<-list()
  
  for (rrrr in c(1:100)){
    print(rrrr)
    seeds<-seeds.all[rep==rrrr]
    item<-N[label %in% seeds$label]
    item$rep<-rrrr
    rep.list[[rrrr]]<-item
  }
  rep.df<-rbindlist(rep.list)
  saveRDS(rep.df, "../Data/Tables/bridge.usage.rda")
  
  sp.ids<-unique(sp.with.bridge$species.label)
  sp.id<-sp.ids[1]
  list_of_sp <- split(sp.with.bridge, by = "species.label")
  length(list_of_sp)
  final.list<-list()
  for (i in c(1:length(list_of_sp))){
    
    print(paste(i, length(list_of_sp)))
    item<-list_of_sp[[i]]
    is.two.continents<-F
    if (nrow(item[current_continent %in% c("Two continents")])>0){
      is.two.continents<-T
    }
    item<-item[current_continent %in% c("North America", 
                                        "South America", 
                                        "bridge1", 
                                        "bridge2")]
    continents<-unique(item$current_continent)
    if (is.two.continents==T){
      continents<-unique(c(continents, 
                           "North America", 
                           "South America"))
    }
    #print(continents)
    if (length(continents)>=2 | length(coutinents[coutinents %in% c("bridge1", "bridge2")])>0){
      item.info<-item[1]
      if (length(continents)==1){
        dt.item<-data.table(seed_id=item.info$seed_id,
                            sp_id=item.info$sp_id,
                            NB=item.info$NB,
                            DA=item.info$DA,
                            species.label=item.info$species.label,
                            seed_continent=item.info$seed_continent,
                            bridge_continent=continents[continents %in% c("bridge1", "bridge2")],
                            across_bridge=F)
      }
      if (length(continents)==2){
        dt.item<-data.table(seed_id=item.info$seed_id,
                            sp_id=item.info$sp_id,
                            NB=item.info$NB,
                            DA=item.info$DA,
                            species.label=item.info$species.label,
                            seed_continent=item.info$seed_continent,
                            bridge_continent=continents[continents %in% c("bridge1", "bridge2")],
                            across_bridge=F)
      }
      if (length(continents)==3 & length(continents[continents %in% c("bridge1", "bridge2")])==2){
        dt.item<-data.table(seed_id=item.info$seed_id,
                            sp_id=item.info$sp_id,
                            NB=item.info$NB,
                            DA=item.info$DA,
                            species.label=item.info$species.label,
                            seed_continent=item.info$seed_continent,
                            bridge_continent=continents[continents %in% c("bridge1", "bridge2")],
                            across_bridge=F)
      }
      if (length(continents)==3 & length(continents[continents %in% c("bridge1", "bridge2")])==1){
        dt.item<-data.table(seed_id=item.info$seed_id,
                            sp_id=item.info$sp_id,
                            NB=item.info$NB,
                            DA=item.info$DA,
                            species.label=item.info$species.label,
                            seed_continent=item.info$seed_continent,
                            bridge_continent=continents[continents %in% c("bridge1", "bridge2")],
                            across_bridge=T)
      }
      if (length(continents)==4){
        
        dt.item<-data.table(seed_id=item.info$seed_id,
                            sp_id=item.info$sp_id,
                            NB=item.info$NB,
                            DA=item.info$DA,
                            species.label=item.info$species.label,
                            seed_continent=item.info$seed_continent,
                            bridge_continent=continents[continents %in% c("bridge1", "bridge2")],
                            across_bridge=T)
      }
      final.list[[length(final.list)+1]]<-dt.item
    }
  }
  final.df<-rbindlist(final.list)
  saveRDS(final.df, "../Data/Tables/bridge.usage.details.rda")
  
  final.df$label<-sprintf("%d.%s.%s", final.df$seed_id, final.df$NB, final.df$DA)
  seeds.all<-readRDS("../Data/Tables/random.seeds.threshold.by.nb.distance.rda")
  
  rep.list<-list()
  
  for (rrrr in c(1:100)){
    print(rrrr)
    seeds<-seeds.all[rep==rrrr]
    item<-final.df[label %in% seeds$label]
    item$rep<-rrrr
    rep.list[[rrrr]]<-item
  }
  rep.df<-rbindlist(rep.list)
  saveRDS(rep.df, "../Data/Tables/bridge.usage.details.rep.rda")
  
}

final.df<-readRDS("../Data/Tables/bridge.usage.details.rep.rda")
final.df$species.label<-sprintf("%s.%s.%s", final.df$sp_id, final.df$NB, final.df$DA)
rep.df.all.details<-final.df[,.(N_SP=length(unique(species.label))), 
                     by=list(seed_continent, bridge_continent, across_bridge, rep, NB, DA)]
rep.df.all.details[bridge_continent=="bridge2", .(N=mean(N_SP)), by=list(across_bridge, DA)]

rep.df.all<-final.df[,.(N_SP=length(unique(species.label))), 
                     by=list(seed_continent, bridge_continent, across_bridge, rep)]
rep.df.all.se<-rep.df.all[,.(N_SP=mean(N_SP), sd=sd(N_SP)), 
                          by=list(seed_continent, bridge_continent, across_bridge)]


pd <- position_dodge(width = 0.4)
rep.df.all.se$current_continent<-factor(rep.df.all.se$current_continent,
                                        levels=c("bridge1", "bridge2"),
                                        labels=c("Isthmus", "Caribbean"))

rep.df.all.se$bridge_continent<-factor(rep.df.all.se$bridge_continent,
                                        levels=c("bridge1", "bridge2"),
                                        labels=c("Isthmus", "Caribbean"))
rep.df.all.se$across_bridge_label<-ifelse(rep.df.all.se$across_bridge, 
                                          "Crossed over successfully",
                                          "Crossed over unsuccessfully")
p<-ggplot(rep.df.all.se[across_bridge_label=="Crossed over successfully"],
          aes(x = bridge_continent, y = N_SP, color = seed_continent)) +
  
  geom_errorbar(aes(ymin = N_SP - sd, ymax = N_SP + sd), 
                width = 0.2, 
                position = pd, 
                linewidth = 0.8) +
  geom_point(position = pd, size = 3) +
  labs(
    x = "Bridge",
    y = "Number of species",
    color = "Original continent"
  ) +
  scale_color_manual(values=c("North America"=color_na, "South America"=color_sa))+
  #facet_wrap(~across_bridge_label, scale="free")+
  theme_bw() +
  theme(
    axis.title.x = element_blank(),
    legend.position = "bottom"
  )
p
ggsave(p, filename="../Figures/Bridge.Usage/Bridge.Usage.pdf", width=5, height=4)
ggsave(p, filename="../Figures/Bridge.Usage/Bridge.Usage.png", width=5, height=4, bg="white")
fwrite(rep.df.all.se, "../Figures/Bridge.Usage/Bridge.Usage.csv")

to.doc(rep.df.all.se, "Bridge usage", "../Figures/Bridge.Usage/Bridge.Usage.docx",
       digits = 2)

rep.df.all<-final.df[,.(N_SP=length(unique(species.label))), 
                     by=list(seed_continent, bridge_continent, NB, DA, across_bridge, rep)]

rep.df.sum<-rep.df.all[,.(N_SP=sum(N_SP)), 
                   by=list(seed_continent, bridge_continent, NB, DA, across_bridge, rep)]
rep.df.se<-rep.df.sum[,.(N_SP=mean(N_SP), sd=sd(N_SP)), 
                  by=list(seed_continent, bridge_continent, NB, DA, across_bridge)]

rep.df.se$bridge_continent<-factor(rep.df.se$bridge_continent,
                                        levels=c("bridge1", "bridge2"),
                                        labels=c("Isthmus", "Caribbean"))
rep.df.se$NB<-factor(rep.df.se$NB, 
                     levels = c("BROAD", "BIG", "MODERATE", "NARROW"), 
                     labels = c("BROAD", "MODERATE", "NARROW", "TINY"))
p<-ggplot(rep.df.se[across_bridge==T], 
          aes(x = bridge_continent, y = N_SP, color = seed_continent)) +
  
  geom_errorbar(aes(ymin = N_SP - sd, ymax = N_SP + sd), 
                width = 0.2, 
                position = pd, 
                linewidth = 0.8) +
  geom_point(position = pd, size = 3) +
  labs(
    x = "Bridge",
    y = "Number of species",
    color = "Original continent"
  ) +
  scale_color_manual(values=c("North America"=color_na, "South America"=color_sa))+
  theme_bw() +
  theme(
    legend.position = "bottom",
    axis.title.x = element_blank()
  )+
  #facet_grid(across_bridge~NB+DA, scale="free")
  facet_grid(DA~NB, scale="free")
p
ggsave(p, filename="../Figures/Bridge.Usage/Bridge.Usage.NB.DA.pdf", width=6, height=4)
ggsave(p, filename="../Figures/Bridge.Usage/Bridge.Usage.NB.DA.png", width=6, height=4, bg="white")
to.doc(rep.df.se, "Bridge usage", "../Figures/Bridge.Usage/Bridge.Usage.NB.DA.docx",
       digits = 2)
fwrite(rep.df.se, "../Figures/Bridge.Usage/Bridge.Usage.NB.DA.csv")


rep.df.se[bridge_continent=="Caribbean" & across_bridge==T]

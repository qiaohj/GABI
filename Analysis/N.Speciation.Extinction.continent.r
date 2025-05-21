library(data.table)
library(sf)
library(RSQLite)
library(DBI)
library(ggplot2)
library(ggh4x)
library(ape)
library(phytools)
library(ggtree)
library(phangorn)
setwd("/media/huijieqiao/WD22T_11/GABI/GABI")
if (F){
  sp<-readRDS("../Data/Tables/100k.speciation.years/virtual.species.rda")
  sp$Parent<-sub("-[^-]*$", "", sp$sp_id)
  species.type.N<-readRDS("../Data/Tables/100k.speciation.years/species.type.N.rda")
  seeds.all<-readRDS("../Data/Tables/100k.speciation.years/random.seeds.rda")
  seeds.all[, .(N=.N), by=list(rep, nb, da, continent)]
  colnames(seeds.all)[1]<-"origin_continent"
  colnames(seeds.all)[6]<-"label"
  seeds.all$label<-sprintf("%d.%s.%s", seeds.all$seed_id, seeds.all$nb, seeds.all$da)
  
  N.all<-list()
  for (rrrr in c(1:100)){
    seeds<-seeds.all[rep==rrrr]
    
    
    sp_filter<-sp[label %in% seeds$label]
    sp_filter<-sp_filter[continent %in% c("South America", "North America")]
    
    sp_filter$year<-sp_filter$year * -1
    sp_filter$seed_id<-as.numeric(sp_filter$seed_id)
    
    
    sp_filter_origin<-merge(sp_filter, seeds, by=c("seed_id", "nb", "da", "label"))
    colnames(sp_filter_origin)[2:3]<-c("NB", "DA")
    
    for (y in c(-1800:0)){
      print(paste(rrrr, y))
      item.species.type.N<-species.type.N[to==y]
      N_Event<-item.species.type.N[type!="Species", 
                                   .(N=.N, year=y, rep=rrrr), 
                                   by=list(type, continent, origin_continent, NB, DA)]
      item.sp_filter<-sp_filter_origin[year==y]
      item.sp.N<-item.sp_filter[, .(N=.N, type="Species", year=y, rep=rrrr), 
                                by=c("continent", "origin_continent", "NB", "DA")]
      item.N<-rbindlist(list(N_Event, item.sp.N), use.names=T)
      N.all[[length(N.all)+1]]<-item.N
    }
  }
  N.all.df<-rbindlist(N.all)
  saveRDS(N.all.df, "../Data/Tables/100k.speciation.years/species.speciation.extinction.N.with.continent.rda")
  N.all.df<-readRDS("../Data/Tables/100k.speciation.years/species.speciation.extinction.N.with.continent.rda")
  N.all.df[type=="Species" & year==0 & NB=="MODERATE-MODERATE" & DA=="GOOD" & continent=="South America" & origin_continent=="North America"]
  
  
  N.all<-N.all.df[,.(N=mean(N), N_sd=sd(N)), 
                  by=list(type, continent, origin_continent,
                          NB, year)]
  
  N.all[type=="Species" & year==0 & NB=="MODERATE-MODERATE" & DA=="GOOD" & continent=="South America" & origin_continent=="North America"]
  
  saveRDS(N.all, "../Data/Tables/100k.speciation.years/sum.species.speciation.extinction.N.with.continent.rda")
}

N.all<-readRDS("../Data/Tables/100k.speciation.years/sum.species.speciation.extinction.N.with.continent.rda")
t<-"Species"
for (t in unique(N.all$type)){
  p<-ggplot(N.all[type==t & year>-1000 & (!NB %in% c("HUGE-HUGE")) &
                    continent %in% c("North America", "South America")])+
    #geom_ribbon(aes(x=year, ymin=N-N_sd, ymax=N+N_sd, 
    #                fill=origin_continent), alpha=0.3)+
    geom_line(aes(x=year, y=N, color=origin_continent))+
    ggtitle(t)+
    facet_grid(NB~continent, scale="free")+
    theme_bw()
  
  ggsave(p, filename=sprintf("../Figures/N.Events/N.%s.png", t))
  
  p<-ggplot(N.all[type==t & year>-1000 & (!NB %in% c("HUGE-HUGE")) &
                    continent != origin_continent &
                    continent %in% c("North America", "South America")])+
    #geom_ribbon(aes(x=year, ymin=N-N_sd, ymax=N+N_sd, 
    #                fill=origin_continent), alpha=0.3)+
    geom_line(aes(x=year, y=N, color=origin_continent))+
    ggtitle(t)+
    facet_grid(NB~continent, scale="free")+
    theme_bw()
  
  ggsave(p, filename=sprintf("../Figures/N.Events/N.Different.continent.%s.png", t))
  
}

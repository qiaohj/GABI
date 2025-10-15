library(data.table)
library(ggplot2)
setDTthreads(30)
setwd("/media/huijieqiao/Butterfly/GABI/GABI")
if (F){
  sp.without.bridge<-readRDS("../Data/Tables/100k.speciation.years/sp_full_continents.without.bridge.rda")
  sp.with.bridge<-readRDS("../Data/Tables/100k.speciation.years/sp_full_continents.rda")
  seeds.all<-readRDS("../Data/Tables/100k.speciation.years/random.seeds.rda")
  all.with.bridge.item<-list()
  all.with.bridge.seed<-list()
  all.without.bridge.item<-list()
  all.without.bridge.seed<-list()
  
  for (rrrr in c(1:10)){
    print(rrrr)
    seeds<-seeds.all[rep==rrrr]
    sp_filter.without.bridge<-sp.without.bridge[label %in% seeds$label]
    sp_filter.with.bridge<-sp.with.bridge[label %in% seeds$label]
    
    #without.bridge
    sp_item.without.bridge<-sp_filter.without.bridge
    sp_item.without.bridge<-sp_item.without.bridge[, c("year", "sp_id", "NB", "DA", "seed_id", "Parent", 
                         "origin_continent", "seed_continent",
                         "current_continent")]
    sp_item.without.bridge<-unique(sp_item.without.bridge)
    sp_item.N.without.bridge<-sp_item.without.bridge[, .(N=length(unique(sp_id))), 
                                  by=list(year, 
                                          seed_id,
                                          NB,
                                          origin_continent, current_continent)]
    sp_item.N.without.bridge$rep<-rrrr
    all.without.bridge.item[[rrrr]]<-sp_item.N.without.bridge
    
    sp_seed.without.bridge<-sp_item.without.bridge
    sp_seed.without.bridge<-sp_seed.without.bridge[, c("year", "NB", "DA", "seed_id", "seed_continent", "current_continent")]
    sp_seed.without.bridge<-unique(sp_seed.without.bridge)
    
    sp_seed.N.without.bridge<-sp_item.without.bridge[, .(N=length(unique(seed_id))), 
                                by=list(year, 
                                        seed_id,
                                        NB,
                                        seed_continent, current_continent)]
    sp_seed.N.without.bridge$rep<-rrrr
    all.without.bridge.seed[[rrrr]]<-sp_seed.N.without.bridge
    
    #with.bridge
    sp_item.with.bridge<-sp_filter.with.bridge
    sp_item.with.bridge<-sp_item.with.bridge[, c("year", "sp_id", "NB", "DA", "seed_id", "Parent", 
                                                       "origin_continent", "seed_continent",
                                                       "current_continent")]
    sp_item.with.bridge<-unique(sp_item.with.bridge)
    sp_item.N.with.bridge<-sp_item.with.bridge[, .(N=length(unique(sp_id))), 
                                                     by=list(year, 
                                                             seed_id,
                                                             NB,
                                                             origin_continent, current_continent)]
    
    sp_item.N.with.bridge$rep<-rrrr
    all.with.bridge.item[[rrrr]]<-sp_item.N.with.bridge
    
    sp_seed.with.bridge<-sp_item.with.bridge
    sp_seed.with.bridge<-sp_seed.with.bridge[, c("year", "NB", "DA", "seed_id", "seed_continent", "current_continent")]
    sp_seed.with.bridge<-unique(sp_seed.with.bridge)
    
    sp_seed.N.with.bridge<-sp_item.with.bridge[, .(N=length(unique(seed_id))), 
                                                     by=list(year,
                                                             seed_id,
                                                             NB,
                                                             seed_continent, 
                                                             current_continent)]
    sp_seed.N.with.bridge$rep<-rrrr
    all.with.bridge.seed[[rrrr]]<-sp_seed.N.with.bridge
  }
  
  all.with.bridge.item.df<-rbindlist(all.with.bridge.item)
  all.with.bridge.seed.df<-rbindlist(all.with.bridge.seed)
  all.without.bridge.item.df<-rbindlist(all.without.bridge.item)
  all.without.bridge.seed.df<-rbindlist(all.without.bridge.seed)
  saveRDS(all.with.bridge.item.df, "../Data/Tables/100k.speciation.years/N.with.bridge.continent.rda")
  saveRDS(all.with.bridge.seed.df, "../Data/Tables/100k.speciation.years/N.with.bridge.seed.rda")
  saveRDS(all.without.bridge.item.df, "../Data/Tables/100k.speciation.years/N.without.bridge.continent.rda")
  saveRDS(all.without.bridge.seed.df, "../Data/Tables/100k.speciation.years/N.without.bridge.seed.rda")
  
  
}
if (F){
  seeds<-sp.with.bridge[,.(N=.N), by=list(NB, DA, seed_id)]
  final<-list()
  for (i in c(1:nrow(seeds))){
    print(paste(i, nrow(seeds)))
    item<-seeds[i]
    sp.items<-sp.with.bridge[NB==item$NB & DA==item$DA & seed_id==item$seed_id]
    seed_continent<-sp.items[year== -1800]$seed_continent
    target_continent<-ifelse(seed_continent=="South America", "North America", "South America")
    target_continent<-c(target_continent, "Two continents")
    target_item<-sp.items[current_continent %in% target_continent]
    to_target_continent<-(nrow(target_item)>0)
    if (to_target_continent==T){
      to_year<-min(target_item$year)
    }else{
      to_year<-Inf
    }
    final_continent<-target_item[year==0]
    to_target_continent_final<-(nrow(final_continent)>0)
    item$seed_continent<-seed_continent
    item$to_target_continent<-to_target_continent
    item$to_target_continent_final<-to_target_continent_final
    item$to_year<-to_year
    final[[i]]<-item
  }
  final.df<-rbindlist(final)
  saveRDS(final.df, "../Data/Tables/100k.speciation.years/N.with.bridge.simulation.rda")
}

df<-readRDS("../Data/Tables/N.with.bridge.simulation.rda")
df<-df[NB %in% c("BIG-BIG", "MODERATE-MODERATE")]
df$label<-sprintf("%d.%s.%s", df$seed_id, df$NB, df$DA)
table(df$seed_continent)

seeds.all<-readRDS("../Data/Tables/random.seeds.rda")

rep.list<-list()
for (rrrr in c(1:10)){
  print(rrrr)
  seeds<-seeds.all[rep==rrrr]
  item<-df[label %in% seeds$label]
  rep.to_target_continent<-item[to_target_continent==T, .(N.to_target_continent=.N), 
                                by=list(NB, DA, seed_continent)]
  rep.to_target_continent_final<-item[to_target_continent_final==T, .(N.to_target_continent_final=.N), 
                                      by=list(NB, DA, seed_continent)]
  rep<-merge(rep.to_target_continent_final, rep.to_target_continent, by=c("NB", "DA", "seed_continent"), all=T)
  rep$rep<-rrrr
  rep.list[[rrrr]]<-rep
}
rep.df.seed<-rbindlist(rep.list)
rep.df.seed$NB.label<-factor(rep.df.seed$NB, 
                        levels=c("BIG-BIG", "MODERATE-MODERATE"),
                        labels=c("BROAD", "NARROW"))

saveRDS(rep.df.seed, "../Data/Tables/N.Seed.Dispersal.rep.rda")
rep.df.seed$label<-sprintf("%s.%s", rep.df.seed$NB.label, rep.df.seed$DA)
ggplot(rep.df.seed[NB.label %in% c("BROAD", "NARROW")], 
       aes(x=label, y=N.to_target_continent, color=seed_continent))+geom_point()+
  geom_boxplot()

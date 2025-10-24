library(data.table)
library(ggplot2)
setDTthreads(30)
setwd("/media/huijieqiao/Butterfly/GABI/GABI")

if (F){
  sp.with.bridge<-readRDS("../Data/Tables/sp_full_continents.rda")
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
  saveRDS(final.df, "../Data/Tables/N.with.bridge.simulation.rda")
}

df<-readRDS("../Data/Tables/N.with.bridge.simulation.rda")
df<-df[NB %in% c("BIG-BIG", "MODERATE-MODERATE")]
df$label<-sprintf("%d.%s.%s", df$seed_id, df$NB, df$DA)
table(df$seed_continent)

seeds.all<-readRDS("../Data/Tables/random.seeds.rda")
seeds.all[seed_id=="5412"]

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


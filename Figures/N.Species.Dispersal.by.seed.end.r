library(data.table)
library(ggplot2)
library(sf)
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
    source_continent<-c(seed_continent, "Two continents")
    target_item<-sp.items[current_continent %in% target_continent & year==0]
    source_item<-sp.items[current_continent %in% source_continent & year==0]
    to_target_continent<-nrow(unique(target_item[,c("sp_id", "seed_id", "NB", "DA")]))
    in_source_continent<-nrow(unique(source_item[,c("sp_id", "seed_id", "NB", "DA")]))
    item$seed_continent<-seed_continent
    item$to_target_continent<-to_target_continent
    item$in_source_continent<-in_source_continent
    
    final[[i]]<-item
  }
  final.df<-rbindlist(final)
  saveRDS(final.df, "../Data/Tables/100k.speciation.years/N.with.bridge.seed.continent.rda")
}



df<-readRDS("../Data/Tables/N.with.bridge.seed.continent.rda")
df<-df[NB %in% c("BIG-BIG", "MODERATE-MODERATE")]
df$label<-sprintf("%d.%s.%s", df$seed_id, df$NB, df$DA)
table(df$seed_continent)

seeds.all<-readRDS("../Data/Tables/random.seeds.rda")
if (F){
  cell.ll<-readRDS("../Data/cells.with.dist.rda")
  seeds.xy<-unique(seeds.all[, c("continent", "seed_id")])
  seeds.xy<-merge(seeds.xy, cell.ll, by.x="seed_id", by.y="seqnum")
  ggplot(seeds.xy)+geom_point(aes(x=lon, y=lat, fill=continent.x))
}
rep.list<-list()
for (rrrr in c(1:10)){
  print(rrrr)
  seeds<-seeds.all[rep==rrrr]
  item<-df[label %in% seeds$label]
  rep.to_target_continent<-item[to_target_continent>0, 
                                .(N.to_target_continent=sum(to_target_continent),
                                  N.in_source_continent=sum(in_source_continent)), 
                                by=list(NB, DA, seed_continent)]
  rep.to_target_continent$rep<-rrrr
  rep.list[[rrrr]]<-rep.to_target_continent
}
rep.df<-rbindlist(rep.list)

rep.df$NB.label<-factor(rep.df$NB, 
                        levels=c("BIG-BIG", "MODERATE-MODERATE"),
                        labels=c("BROAD", "NARROW"))

saveRDS(rep.df, "../Data/Tables/N.Species.Dispersal.by.seed.end.rep.rda")
ggplot(rep.df, 
       aes(x=NB.label, y=N.to_target_continent, color=seed_continent))+
  labs(y="Number of species to the other continent")+
  geom_boxplot()+
  facet_wrap(~DA)

rep.df.all<-rep.df[NB.label %in% c("BROAD", "NARROW"),
                   .(N.to_target_continent=sum(N.to_target_continent),
                      N.in_source_continent=sum(N.in_source_continent)),
                   by=list(seed_continent, rep)]

item1<-rep.df.all[,c("seed_continent",   "rep", "N.to_target_continent")]
colnames(item1)[3]<-"N"
item1$type<-"to_target_continent"

item2<-rep.df.all[,c("seed_continent",   "rep", "N.in_source_continent")]
colnames(item2)[3]<-"N"
item2$type<-"in_source_continent"

item.final<-rbindlist(list(item1, item2))

item.final$final.continent<-ifelse(item.final$seed_continent=="North America", "South America", "North America")
item.final[type=="in_source_continent", final.continent:=seed_continent]

ggplot(item.final, 
       aes(x=final.continent, y=N, color=seed_continent))+
  geom_boxplot()

summary_dt<-item.final[, .(mean=mean(N), sd=sd(N)),
                       by=list(seed_continent, type)]
colnames(summary_dt)<-c("Original continent", "Type", "mean", "sd")
summary_dt$Value<-sprintf("%.2f±%.2f", summary_dt$mean, summary_dt$sd)
summary_dt$mean<-NULL
summary_dt$sd<-NULL
to.doc(summary_dt, 
       "Number of species stay in the original continent and dispersal to the other continent", 
       "../Table.Doc/species.2.other.continent.full.docx")

item1<-rep.df[,c("seed_continent",   "rep", "N.to_target_continent", "NB.label", "DA")]
colnames(item1)[3]<-"N"
item1$type<-"to_target_continent"

item2<-rep.df[,c("seed_continent",   "rep", "N.in_source_continent", "NB.label", "DA")]
colnames(item2)[3]<-"N"
item2$type<-"in_source_continent"

item.final<-rbindlist(list(item1, item2))

item.final$final.continent<-ifelse(item.final$seed_continent=="North America", "South America", "North America")
item.final[type=="in_source_continent", final.continent:=seed_continent]

ggplot(item.final, 
       aes(x=final.continent, y=N, color=seed_continent))+
  geom_boxplot()+facet_grid(NB.label~DA, scale="free")


summary_dt<-item.final[, .(mean=mean(N), sd=sd(N)),
                       by=list(seed_continent, type, NB.label, DA)]
colnames(summary_dt)<-c("Original continent", "Type", "Niche Breadth", "Dispersal Ability", "mean", "sd")
summary_dt$Value<-sprintf("%.2f±%.2f", summary_dt$mean, summary_dt$sd)
summary_dt$mean<-NULL
summary_dt$sd<-NULL
to.doc(summary_dt, 
       "Number of species stay in the original continent and dispersal to the other continent", 
       "../Table.Doc/species.2.other.continent.detail.docx")

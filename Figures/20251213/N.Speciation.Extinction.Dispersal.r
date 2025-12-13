library(data.table)
library(ggplot2)
library(sf)
setDTthreads(30)
setwd("/media/huijieqiao/Butterfly/GABI/GABI")
if (F){
  sp.with.bridge<-readRDS("../Data/Tables/sp_full_continents.rda")
  sp.with.bridge.N<-sp.with.bridge[, .(N=.N), 
                                   by=list(type, gain.continent, loss.continent, 
                                           seed_id, NB, DA, seed_continent)]
  sp.with.bridge.N<-sp.with.bridge.N[!is.na(type)]
  unique(sp.with.bridge.N$type)
  sp.with.bridge.N<-sp.with.bridge.N[type %in% c("New.Immigrants",   "Local.Extinction", "Speciation" , "Extinction")]
  sp.with.bridge.N[type=="New.Immigrants" & gain.continent==seed_continent, type:="Secondary Invader"]
  sp.with.bridge.N[type=="New.Immigrants" & gain.continent!=seed_continent, type:="Primary Invader"]
  sp.with.bridge.N[type=="Speciation" & gain.continent!=seed_continent, type:="Non-local Speciation"]
  sp.with.bridge.N[type=="Speciation" & gain.continent==seed_continent, type:="Local Speciation"]
  sp.with.bridge.N[type=="Extinction" & loss.continent!=seed_continent, type:="Failed Invader"]
  sp.with.bridge.N[type=="Extinction" & loss.continent==seed_continent, type:="Extinction"]
  sp.with.bridge.N[type=="Local.Extinction" & loss.continent==seed_continent, type:="Local Extinction"]
  sp.with.bridge.N[type=="Local.Extinction" & loss.continent!=seed_continent, type:="Non-local Extinction"]
  saveRDS(sp.with.bridge.N, "../Data/Tables/N.Speciation.Extinction.Dispersal.rda")
}

if (F){
  sp.with.bridge<-readRDS("../Data/Tables/sp_full_continents.NULL.rda")
  sp.with.bridge.N<-sp.with.bridge[, .(N=.N), 
                                   by=list(type, gain.continent, loss.continent, 
                                           seed_id, NB, DA, seed_continent)]
  sp.with.bridge.N<-sp.with.bridge.N[!is.na(type)]
  unique(sp.with.bridge.N$type)
  sp.with.bridge.N<-sp.with.bridge.N[type %in% c("New.Immigrants",   "Local.Extinction", "Speciation" , "Extinction")]
  sp.with.bridge.N[type=="New.Immigrants" & gain.continent==seed_continent, type:="Secondary Invader"]
  sp.with.bridge.N[type=="New.Immigrants" & gain.continent!=seed_continent, type:="Primary Invader"]
  sp.with.bridge.N[type=="Speciation" & gain.continent!=seed_continent, type:="Non-local Speciation"]
  sp.with.bridge.N[type=="Speciation" & gain.continent==seed_continent, type:="Local Speciation"]
  sp.with.bridge.N[type=="Extinction" & loss.continent!=seed_continent, type:="Failed Invader"]
  sp.with.bridge.N[type=="Extinction" & loss.continent==seed_continent, type:="Extinction"]
  sp.with.bridge.N[type=="Local.Extinction" & loss.continent==seed_continent, type:="Local Extinction"]
  sp.with.bridge.N[type=="Local.Extinction" & loss.continent!=seed_continent, type:="Non-local Extinction"]
  saveRDS(sp.with.bridge.N, "../Data/Tables/N.Speciation.Extinction.Dispersal.NULL.rda")
}

df<-readRDS("../Data/Tables/N.Speciation.Extinction.Dispersal.rda")
table(df$NB)
df<-df[NB %in% c("BIG-BIG", "MODERATE-MODERATE")]
df$label<-sprintf("%d.%s.%s", df$seed_id, df$NB, df$DA)
table(df$type)

seeds.all<-readRDS("../Data/Tables/random.seeds.threshold.by.nb.distribution.rda")
if (F){
  seeds.allx<-seeds.all
  seeds.allx[nb=="BIG-BIG", nb:="NARROW-NARROW"]
  seeds.allx[nb=="MODERATE-MODERATE", nb:="BROAD-BROAD"]
  seeds.allx$label<-sprintf("%d.%s.%s", seeds.allx$seed_id, seeds.allx$nb, seeds.allx$da)
  seeds.all<-rbindlist(list(seeds.allx, seeds.all))
}
rep.list<-list()
rep.list.all<-list()
for (rrrr in c(1:100)){
  print(rrrr)
  seeds<-seeds.all[rep==rrrr]
  item<-df[label %in% seeds$label]
  item.rep<-item[, .(N=sum(N)), 
                 by=list(NB, DA, seed_continent, type)]
  item.rep$rep<-rrrr
  rep.list[[rrrr]]<-item.rep
  item.rep<-item[, .(N=sum(N)), 
                 by=list(seed_continent, type)]
  item.rep$rep<-rrrr
  rep.list.all[[rrrr]]<-item.rep
}
rep.df<-rbindlist(rep.list)
rep.df.all<-rbindlist(rep.list.all)

#rep.df$NB.label<-factor(rep.df$NB, 
#                        levels=c("BROAD-BROAD", "BIG-BIG", "MODERATE-MODERATE", "NARROW-NARROW"),
#                        labels=c( "BROAD", "BIG", "MODERATE", "NARROW"))


saveRDS(rep.df, "../Data/Tables/N.Speciation.Extinction.Dispersal.rep.rda")
saveRDS(rep.df.all, "../Data/Tables/N.Speciation.Extinction.Dispersal.all.rep.rda")

extinction<-rep.df.all[type %in% c("Failed Invader", "Extinction")]
extinction$continent<-extinction$seed_continent
extinction$other.continent<-ifelse(extinction$seed_continent=="South America",
                                   "North America", "South America")

extinction[type=="Failed Invader", continent:=other.continent]

ggplot(extinction)+geom_boxplot(aes(x=continent, y=N, color=seed_continent))+
  labs(title="Extinction")

extinction<-rep.df[type %in% c("Failed Invader", "Extinction")]
extinction$continent<-extinction$seed_continent
extinction$other.continent<-ifelse(extinction$seed_continent=="South America",
                                   "North America", "South America")

extinction[type=="Failed Invader", continent:=other.continent]

ggplot(extinction)+geom_boxplot(aes(x=continent, y=N, color=seed_continent))+
  facet_grid(NB~DA, scale="free")+
  labs(title="Extinction")

ggplot(extinction)+geom_boxplot(aes(x=continent, y=N, color=seed_continent))+
  facet_wrap(~NB, scale="free")+
  labs(title="Extinction")



speciation<-rep.df.all[type %in% c("Local Speciation", "Non-local Speciation")]
speciation$continent<-speciation$seed_continent
speciation$other.continent<-ifelse(speciation$seed_continent=="South America",
                                   "North America", "South America")

speciation[type=="Non-local Speciation", continent:=other.continent]

ggplot(speciation)+geom_boxplot(aes(x=continent, y=N, color=seed_continent))+
  labs(title="Speciation")

speciation<-rep.df[type %in% c("Local Speciation", "Non-local Speciation")]
speciation$continent<-speciation$seed_continent
speciation$other.continent<-ifelse(speciation$seed_continent=="South America",
                                   "North America", "South America")

speciation[type=="Non-local Speciation", continent:=other.continent]

ggplot(speciation)+geom_boxplot(aes(x=continent, y=N, color=seed_continent))+
  facet_grid(NB~DA, scale="free")+
  labs(title="Speciation")

ggplot(speciation)+geom_boxplot(aes(x=continent, y=N, color=seed_continent))+
  facet_wrap(~NB, scale="free")+
  labs(title="Speciation")


dispersal<-rep.df.all[type %in% c("Secondary Invader", "Primary Invader")]
dispersal$continent<-dispersal$seed_continent
dispersal$other.continent<-ifelse(dispersal$seed_continent=="South America",
                                   "North America", "South America")

dispersal[type=="Primary Invader", continent:=other.continent]

ggplot(dispersal)+geom_boxplot(aes(x=continent, y=N, color=seed_continent))+
  labs(title="Dispersal")

dispersal<-rep.df[type %in% c("Secondary Invader", "Primary Invader")]
dispersal$continent<-dispersal$seed_continent
dispersal$other.continent<-ifelse(dispersal$seed_continent=="South America",
                                   "North America", "South America")

dispersal[type=="Primary Invader", continent:=other.continent]

ggplot(dispersal)+geom_boxplot(aes(x=continent, y=N, color=seed_continent))+
  facet_grid(NB~DA, scale="free")+
  labs(title="Dispersal")

ggplot(dispersal)+geom_boxplot(aes(x=continent, y=N, color=seed_continent))+
  facet_wrap(~NB, scale="free")+
  labs(title="Dispersal")


local_extinction<-rep.df.all[type %in% c("Local Extinction", "Non-local Extinction")]
local_extinction$continent<-local_extinction$seed_continent
local_extinction$other.continent<-ifelse(local_extinction$seed_continent=="South America",
                                  "North America", "South America")

local_extinction[type=="Non-local Extinction", continent:=other.continent]

ggplot(local_extinction)+geom_boxplot(aes(x=continent, y=N, color=seed_continent))

local_extinction<-rep.df[type %in% c("Local Extinction", "Non-local Extinction")]
local_extinction$continent<-local_extinction$seed_continent
local_extinction$other.continent<-ifelse(local_extinction$seed_continent=="South America",
                                  "North America", "South America")

local_extinction[type=="Non-local Extinction", continent:=other.continent]

ggplot(local_extinction)+geom_boxplot(aes(x=continent, y=N, color=seed_continent))+
  facet_grid(NB~DA, scale="free")



rep.df.sd<-rep.df.all[, .(N=mean(N), sd=sd(N)),
                  by=list(seed_continent, type)]
pd <- position_dodge(width = 0.9)

p<-ggplot(rep.df.sd)+
  geom_bar(aes(x=type, y=N, fill=seed_continent), 
           stat = "identity", position="dodge")+
  geom_errorbar(aes(x=type, ymin=N-sd, ymax=N+sd, group=seed_continent), 
                position=pd, width=0.2)+
  theme_bw()
p
ggsave(p, filename="../Figures/event.type.pdf", width=12, height=6)
rep.df.sd<-rep.df[, .(N=mean(N), sd=sd(N)),
                  by=list(seed_continent, type, NB, DA)]

if (F){
  rep.df.sd[type %in% c("Local Speciation", "Non-local Speciation"), type:="Speciation"]
  rep.df.sd<-rep.df.sd[, .(N=sum(N), sd=mean(sd)), by=list(NB.label, DA, seed_continent, type)]
}
p<-ggplot(rep.df.sd[type %in% c("Extinction", "Local Speciation", "Non-local Speciation")])+
  geom_bar(aes(x=type, y=N, fill=seed_continent), stat = "identity", position="dodge")+
  geom_errorbar(aes(x=type, ymin=N-sd, ymax=N+sd, group=seed_continent), 
                position=pd, width=0.2)+
  facet_grid(NB~DA, scale="free")+
  theme_bw()+
  theme(axis.text.x = element_text(
    angle = 45,
    hjust = 1,
    vjust = 1 
  ))
p

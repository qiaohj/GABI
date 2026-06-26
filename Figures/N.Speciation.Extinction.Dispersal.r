library(data.table)
library(ggplot2)
library(sf)
setDTthreads(30)
setwd("/media/huijieqiao/Butterfly/GABI/GABI")
source("Figures/common.r")
if (F){
  sp.with.bridge<-readRDS("../Data/Tables/sp_full_continents.rda")
  View(sp.with.bridge[seed_id==33595 & NB=="NARROW" & DA=="POOR" & type!="Still.There"])
  
  sp.with.bridge[previous_continent %in% c("bridge1", "bridge2") & type=="New.Immigrants" & current_continent==seed_continent,
                 type:="Still.There"]
  
  sp.with.bridge.N<-sp.with.bridge[, .(N=.N), 
                                   by=list(type, gain.continent, loss.continent, 
                                           seed_id, NB, DA, seed_continent)]
  sp.with.bridge.N<-sp.with.bridge.N[!is.na(type)]
  unique(sp.with.bridge.N$type)
  
  sp.with.bridge[seed_id==33595 & NB=="NARROW" & DA=="POOR" & type=="New.Immigrants"]
  
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
  
  sp.with.bridge.final<-sp.with.bridge[year==0]
  sp.with.bridge.final<-sp.with.bridge.final[N>0]
  sp.with.bridge.final.2continents<-sp.with.bridge.final[current_continent=="Two continents"]
  sp.with.bridge.final.2continents1<-sp.with.bridge.final.2continents
  sp.with.bridge.final.2continents1$current_continent<-"South America"
  sp.with.bridge.final.2continents2<-sp.with.bridge.final.2continents
  sp.with.bridge.final.2continents2$current_continent<-"North America"
  sp.with.bridge.final<-sp.with.bridge.final[current_continent %in% c("South America", "North America")]
  sp.with.bridge.final<-rbindlist(list(sp.with.bridge.final, 
                                       sp.with.bridge.final.2continents1, 
                                       sp.with.bridge.final.2continents2))
  sp.with.bridge.final.N<-sp.with.bridge.final[,.(N.SP=length(unique(species.label))),
                                               by=list(seed_id, NB, DA, seed_continent, current_continent)]
  
  sp.with.bridge.final.N$type<-ifelse(sp.with.bridge.final.N$seed_continent==sp.with.bridge.final.N$current_continent,
                                      "Native", "Immigrant")
  table(sp.with.bridge.final.N$type)
  
  saveRDS(sp.with.bridge.final.N, "../Data/Tables/N.Richness.rda")
  
  
  sp.with.bridge[DA=="GOOD" & NB=="MODERATE" & seed_id=="11871"]
  table(sp.with.bridge$type)
  Extinction<-sp.with.bridge[type %in% c("Extinction", "Local.Extinction")]
  Extinction.N<-Extinction[, .(N=.N), by=list(year, seed_id, NB, DA, seed_continent, current_continent, previous_continent,
                                              type, gain.continent, loss.continent)]
  
  saveRDS(Extinction.N, "../Data/Tables/N.Extinction.rda")
  
  
  Speciation<-sp.with.bridge[type %in% c("Speciation")]
  Speciation.N<-Speciation[, .(N=.N), by=list(year, seed_id, NB, DA, seed_continent, current_continent, previous_continent,
                                              type, gain.continent, loss.continent)]
  if (F){
    Speciation.N1<-Speciation.N[current_continent=="Two continents"]
    Speciation.SA<-Speciation.N1
    Speciation.SA$current_continent<-"South America"
    Speciation.SA$gain.continent<-"South America"
    Speciation.NA<-Speciation.N1
    Speciation.NA$current_continent<-"North America"
    Speciation.NA$gain.continent<-"North America"
    
    Speciation.N2<-Speciation.N[current_continent!="Two continents"]
    Speciation.N<-rbindlist(list(Speciation.NA, Speciation.SA, Speciation.N2))
    Speciation.N<-Speciation.N[, .(N=sum(N)), by=list(year, seed_id, NB, DA, seed_continent, current_continent, previous_continent,
                                                      type, gain.continent, loss.continent)]
  }
  saveRDS(Speciation.N, "../Data/Tables/N.Speciation.rda")
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
richness.df<-readRDS("../Data/Tables/N.with.bridge.seed.continent.rda")
richness.df$label<-sprintf("%d.%s.%s", richness.df$seed_id, richness.df$NB, richness.df$DA)


table(df$NB)
df$label<-sprintf("%d.%s.%s", df$seed_id, df$NB, df$DA)
table(df$type)

seeds.all<-readRDS("../Data/Tables/random.seeds.threshold.by.nb.distance.rda")
rep.list<-list()
rep.list.all<-list()

rep.richness.list<-list()
rep.richness.list.all<-list()

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
  
  item<-richness.df[label %in% seeds$label]
  rep.to_target_continent<-item[, 
                                .(N.to_target_continent=sum(to_target_continent),
                                  N.in_source_continent=sum(in_source_continent)), 
                                by=list(NB, DA, seed_continent)]
  rep.to_target_continent$rep<-rrrr
  rep.richness.list[[rrrr]]<-rep.to_target_continent
  
  rep.to_target_continent_all<-item[, 
                                .(N.to_target_continent=sum(to_target_continent),
                                  N.in_source_continent=sum(in_source_continent)), 
                                by=list(seed_continent)]
  rep.to_target_continent_all$rep<-rrrr
  rep.richness.list.all[[rrrr]]<-rep.to_target_continent_all
}
rep.df<-rbindlist(rep.list)
rep.df.all<-rbindlist(rep.list.all)

rep.richness.df<-rbindlist(rep.richness.list)
rep.richness.df.all<-rbindlist(rep.richness.list.all)

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

extinction.all<-extinction
extinction.all$event<-"Extinction"
extinction.all$type<-ifelse(extinction.all$seed_continent==extinction.all$continent, 
                            "Native", "Immigrant")
extinction.all$other.continent<-NULL

ggplot(extinction.all)+geom_boxplot(aes(x=continent, y=N, color=type))

speciation<-rep.df.all[type %in% c("Local Speciation", "Non-local Speciation")]
speciation$continent<-speciation$seed_continent
speciation$other.continent<-ifelse(speciation$seed_continent=="South America",
                                   "North America", "South America")

speciation[type=="Non-local Speciation", continent:=other.continent]
speciation.all<-speciation
speciation.all$event<-"Speciation"
speciation.all$type<-ifelse(speciation.all$seed_continent==speciation.all$continent, 
                            "Native", "Immigrant")
speciation.all$other.continent<-NULL
ggplot(speciation.all)+geom_boxplot(aes(x=continent, y=N, color=type))

dispersal<-rep.df.all[type %in% c("Secondary Invader", "Primary Invader")]
#dispersal$type2<-dispersal$type
dispersal$continent<-dispersal$seed_continent
dispersal$other.continent<-ifelse(dispersal$seed_continent=="South America",
                                  "North America", "South America")

dispersal[type=="Primary Invader", continent:=other.continent]

dispersal.all<-dispersal
dispersal.all$event<-"Dispersal"
#dispersal.all$type<-ifelse(dispersal.all$seed_continent==dispersal.all$continent, 
#                            "Native", "Immigrant")
dispersal.all$other.continent<-NULL
dispersal.all$disp.type<-"N to S"
dispersal.all[seed_continent=="North America" & type=="Primary Invader", disp.type:="N to S"]
dispersal.all[seed_continent=="North America" & type=="Secondary Invader", disp.type:="S to N"]
dispersal.all[seed_continent=="South America" & type=="Primary Invader", disp.type:="S to N"]
dispersal.all[seed_continent=="South America" & type=="Secondary Invader", disp.type:="N to S"]

p<-ggplot(dispersal.all)+geom_boxplot(aes(x=disp.type, y=N))+
  facet_wrap(~type)+
  labs(y="Number of Species")+
  theme_bw()+
  theme(axis.title.x = element_blank())
p
dispersal.all.se<-dispersal.all[,.(mean=mean(N), sd=sd(N)),
                                by=list(type, disp.type)]
setorderv(dispersal.all.se, c("type", "disp.type"))
to.doc(dispersal.all.se, "Number of dispersal events", 
       "../Figures/N.Speciation.Extinction.Dispersal/N.Dispersal.docx",
       digits=2)
ggsave(p, filename="../Figures/N.Speciation.Extinction.Dispersal/N.Dispersal.pdf", width=5, height=3)
ggsave(p, filename="../Figures/N.Speciation.Extinction.Dispersal/N.Dispersal.png", width=5, height=3, bg="white")
item1<-rep.richness.df.all[,c("seed_continent",   "rep", "N.to_target_continent")]
colnames(item1)[3]<-"N"
item1$type<-"to_target_continent"

item2<-rep.richness.df.all[,c("seed_continent",   "rep", "N.in_source_continent")]
colnames(item2)[3]<-"N"
item2$type<-"in_source_continent"

item.final<-rbindlist(list(item1, item2))

item.final$final.continent<-ifelse(item.final$seed_continent=="North America", "South America", "North America")
item.final[type=="in_source_continent", final.continent:=seed_continent]

richness<-item.final
richness$continent<-richness$final.continent
richness$type<-ifelse(richness$type=="in_source_continent", "Native", "Immigrant")
ggplot(richness)+geom_boxplot(aes(x=continent, y=N, color=type))

richness.all<-richness[, c("seed_continent", "type", "N", "rep", "continent")]
richness.all$event<-"Richness"
all.df<-rbindlist(list(richness.all, speciation.all, extinction.all))
table(all.df$type)

all.df$event<-factor(all.df$event, levels=c("Speciation", "Extinction", "Richness"))
all.df$type<-factor(all.df$type, levels=c("Native", "Immigrant"))

p<-ggplot(all.df)+geom_boxplot(aes(x=continent, y=N, color=type))+
  facet_wrap(~event, nrow=1, scale="free")+
  labs(y="Number of events/species", color="Species type")+
  scale_color_manual(values=c("Native"=color_low, "Immigrant"=color_high))+
  theme_bw()+
  theme(legend.position = "bottom",
        axis.title.x = element_blank())
p

all.df.se<-all.df[,.(mean=mean(N), sd=sd(N)),
                                by=list(event, continent, type)]
setorderv(all.df.se, c("event", "continent", "type"))
to.doc(all.df.se, "Number of speciation and extinction events, and species richness", 
       "../Figures/N.Speciation.Extinction.Dispersal/N.Speciation.Extinction.Richness.docx",
       digits=2)
ggsave(p, filename="../Figures/N.Speciation.Extinction.Dispersal/N.Speciation.Extinction.Richness.pdf", 
       width=10, height=3)
ggsave(p, filename="../Figures/N.Speciation.Extinction.Dispersal/N.Speciation.Extinction.Richness.png", 
       width=10, height=3, bg="white")


#By NB & DA

extinction<-rep.df[type %in% c("Failed Invader", "Extinction")]
extinction$continent<-extinction$seed_continent
extinction$other.continent<-ifelse(extinction$seed_continent=="South America",
                                   "North America", "South America")
extinction[type=="Failed Invader", continent:=other.continent]

extinction.all<-extinction
extinction.all$event<-"Extinction"
extinction.all$type<-ifelse(extinction.all$seed_continent==extinction.all$continent, 
                            "Native", "Immigrant")
extinction.all$other.continent<-NULL

ggplot(extinction.all)+geom_boxplot(aes(x=continent, y=N, color=type))+
  facet_grid(NB~DA)

speciation<-rep.df[type %in% c("Local Speciation", "Non-local Speciation")]
speciation$continent<-speciation$seed_continent
speciation$other.continent<-ifelse(speciation$seed_continent=="South America",
                                   "North America", "South America")

speciation[type=="Non-local Speciation", continent:=other.continent]
speciation.all<-speciation
speciation.all$event<-"Speciation"
speciation.all$type<-ifelse(speciation.all$seed_continent==speciation.all$continent, 
                            "Native", "Immigrant")
speciation.all$other.continent<-NULL
ggplot(speciation.all)+geom_boxplot(aes(x=continent, y=N, color=type))+
  facet_grid(NB~DA)

dispersal<-rep.df[type %in% c("Secondary Invader", "Primary Invader")]
#dispersal$type2<-dispersal$type
dispersal$continent<-dispersal$seed_continent
dispersal$other.continent<-ifelse(dispersal$seed_continent=="South America",
                                  "North America", "South America")

dispersal[type=="Primary Invader", continent:=other.continent]

dispersal.all<-dispersal
dispersal.all$event<-"Dispersal"
#dispersal.all$type<-ifelse(dispersal.all$seed_continent==dispersal.all$continent, 
#                            "Native", "Immigrant")
dispersal.all$other.continent<-NULL

dispersal.all$disp.type<-"N to S"
dispersal.all[seed_continent=="North America" & type=="Primary Invader", disp.type:="N to S"]
dispersal.all[seed_continent=="North America" & type=="Secondary Invader", disp.type:="S to N"]
dispersal.all[seed_continent=="South America" & type=="Primary Invader", disp.type:="S to N"]
dispersal.all[seed_continent=="South America" & type=="Secondary Invader", disp.type:="N to S"]
dispersal.all$NB<-factor(dispersal.all$NB, 
                     levels = c("BROAD", "BIG", "MODERATE", "NARROW"), 
                     labels = c("BROAD", "MODERATE", "NARROW", "TINY"))
p<-ggplot(dispersal.all)+
  geom_boxplot(aes(x=disp.type, y=N, color=type))+
  facet_grid(DA~NB, scale="free")+
  scale_color_manual(values=c("Primary Invader"=color_low, "Secondary Invader"=color_high))+
  labs(y="Number of Species", color="Type of invader")+
  theme_bw()+
  theme(axis.title.x = element_blank(),
        legend.position = "bottom")
p

dispersal.all.se<-dispersal.all[,.(mean=mean(N), sd=sd(N)),
                                by=list(type, disp.type, NB, DA)]
setorderv(dispersal.all.se, c("type", "disp.type", "NB", "DA"))
to.doc(dispersal.all.se, "Number of dispersal events", 
       "../Figures/N.Speciation.Extinction.Dispersal/N.Dispersal.details.docx",
       digits=2)
ggsave(p, filename="../Figures/N.Speciation.Extinction.Dispersal/N.Dispersal.details.pdf", width=10, height=5)
ggsave(p, filename="../Figures/N.Speciation.Extinction.Dispersal/N.Dispersal.details.png", width=10, height=5, bg="white")



item1<-rep.richness.df[,c("seed_continent",   "rep", "N.to_target_continent", "NB", "DA")]
colnames(item1)[3]<-"N"
item1$type<-"to_target_continent"

item2<-rep.richness.df[,c("seed_continent",   "rep", "N.in_source_continent", "NB", "DA")]
colnames(item2)[3]<-"N"
item2$type<-"in_source_continent"

item.final<-rbindlist(list(item1, item2))

item.final$final.continent<-ifelse(item.final$seed_continent=="North America", "South America", "North America")
item.final[type=="in_source_continent", final.continent:=seed_continent]

richness<-item.final
richness$continent<-richness$final.continent
richness$type<-ifelse(richness$type=="in_source_continent", "Native", "Immigrant")
ggplot(richness)+geom_boxplot(aes(x=continent, y=N, color=type))+
  facet_grid(NB~DA)

richness.all<-richness[, c("seed_continent", "type", "N", "rep", "continent", "NB", "DA")]
richness.all$event<-"Richness"
all.df<-rbindlist(list(richness.all, speciation.all, extinction.all), use.names=T)
table(all.df$type)

all.df$event<-factor(all.df$event, levels=c("Speciation", "Extinction", "Richness"))
all.df$type<-factor(all.df$type, levels=c("Native", "Immigrant"))
all.df$NB<-factor(all.df$NB, 
                     levels = c("BROAD", "BIG", "MODERATE", "NARROW"), 
                     labels = c("BROAD", "MODERATE", "NARROW", "TINY"))
all.df$continent<-ifelse(all.df$continent=="North America", "N", "S")
p<-ggplot(all.df)+geom_boxplot(aes(x=continent, y=N, color=type))+
  facet_grid(event~NB+DA, scale="free")+
  labs(y="Number of events/species", color="Species type")+
  scale_color_manual(values=c("Native"=color_low, "Immigrant"=color_high))+
  theme_bw()+
  theme(legend.position = "bottom",
        axis.title.x = element_blank())
p

all.df.se<-all.df[,.(mean=mean(N), sd=sd(N)),
                  by=list(event, continent, type, NB, DA)]
setorderv(all.df.se, c("event", "continent", "type", "NB", "DA"))
to.doc(all.df.se, "Number of speciation and extinction events, and species richness", 
       "../Figures/N.Speciation.Extinction.Dispersal/N.Speciation.Extinction.Richness.details.docx",
       digits=2)
ggsave(p, filename="../Figures/N.Speciation.Extinction.Dispersal/N.Speciation.Extinction.Richness.details.pdf", 
       width=15, height=5)
ggsave(p, filename="../Figures/N.Speciation.Extinction.Dispersal/N.Speciation.Extinction.Richness.details.png", 
       width=15, height=5, bg="white")



###For analysis only
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

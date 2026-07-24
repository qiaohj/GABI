library(data.table)
library(ggplot2)
library(sf)
library(patchwork)
library(dplyr)
setDTthreads(30)
setwd("/media/huijieqiao/Butterfly/GABI/GABI")
source("Figures/common.r")
if (F){
  sp.with.bridge_bak<-readRDS("../Data/Tables/sp_full_continents.NULL.rda")
  sp.with.bridge<-copy(sp.with.bridge_bak)
  #View(sp.with.bridge[seed_id==33595 & NB=="NARROW" & DA=="POOR" & type!="Still.There"])
  
  sp.with.bridge[previous_continent %in% c("bridge1", "bridge2") & 
                   type=="New.Immigrants" & 
                   current_continent==seed_continent,
                 type:="Still.There"]
  
  sp.with.bridge<-sp.with.bridge[!(type=="New.Immigrants" & gain.continent %in% c("", "Two continents"))]
  sp.with.bridge[type=="New.Immigrants" & gain.continent=="Two continents"]
  sp.with.bridge<-sp.with.bridge[!(type=="Speciation" & gain.continent=="")]
  sp.with.bridge<-sp.with.bridge[!(type=="Extinction" & loss.continent=="")]
  sp.with.bridge<-sp.with.bridge[!(type=="Local.Extinction" & loss.continent=="")]
  
  
  sp.with.bridge<-sp.with.bridge[!(current_continent=="Two continents" & type=="Speciation")]
  sp.with.bridge.N<-sp.with.bridge[, .(N=.N), 
                                   by=list(type, gain.continent, loss.continent, 
                                           seed_id, NB, DA, seed_continent)]
  sp.with.bridge.N<-sp.with.bridge.N[!is.na(type)]
  table(sp.with.bridge.N$type)
  
  sp.with.bridge[seed_id==33595 & NB=="NARROW" & DA=="POOR" & type=="New.Immigrants"]
  
  sp.with.bridge.N<-sp.with.bridge.N[type %in% c("New.Immigrants",
                                                 "Local.Extinction", 
                                                 "Speciation" , 
                                                 "Extinction")]
  sp.with.bridge.N[type=="New.Immigrants" & gain.continent==""]
  table(sp.with.bridge.N[type=="New.Immigrants"]$gain.continent)
  
  sp.with.bridge.N[type=="New.Immigrants" & gain.continent==seed_continent, type:="Secondary Invader"]
  sp.with.bridge.N[type=="New.Immigrants" & gain.continent!=seed_continent, type:="Primary Invader"]
  
  table(sp.with.bridge.N[type=="Speciation"]$gain.continent)
  sp.with.bridge.N[type=="Speciation" & gain.continent!=seed_continent, type:="Non-local Speciation"]
  sp.with.bridge.N[type=="Speciation" & gain.continent==seed_continent, type:="Local Speciation"]
  
  table(sp.with.bridge.N[type=="Extinction"]$loss.continent)
  
  sp.with.bridge.N[type=="Extinction" & loss.continent!=seed_continent, type:="Failed Invader"]
  sp.with.bridge.N[type=="Extinction" & loss.continent==seed_continent, type:="Extinction"]
  
  table(sp.with.bridge.N[type=="Local.Extinction"]$loss.continent)
  
  sp.with.bridge.N[type=="Local.Extinction" & loss.continent==seed_continent, type:="Local Extinction"]
  sp.with.bridge.N[type=="Local.Extinction" & loss.continent!=seed_continent, type:="Non-local Extinction"]
  saveRDS(sp.with.bridge.N, "../Data/Tables/N.Speciation.Extinction.Dispersal.NULL.rda")
  
  sp.with.bridge.final<-sp.with.bridge[year==0]
  sp.with.bridge.final<-sp.with.bridge.final[N>0]
  table(sp.with.bridge.final$current_continent)
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
  
  saveRDS(sp.with.bridge.final.N, "../Data/Tables/N.Richness.NULL.rda")
  
  
  sp.with.bridge[DA=="GOOD" & NB=="MODERATE" & seed_id=="11871"]
  table(sp.with.bridge$type)
  Extinction<-sp.with.bridge[type %in% c("Extinction", "Local.Extinction")]
  table(Extinction$loss.continent)
  
  Extinction.N<-Extinction[, .(N=.N), by=list(year, seed_id, NB, DA, seed_continent, current_continent, previous_continent,
                                              type, gain.continent, loss.continent)]
  
  saveRDS(Extinction.N, "../Data/Tables/N.Extinction.NULL.rda")
  
  Dispersal<-sp.with.bridge[type %in% c("New.Immigrants")]
  table(Dispersal$gain.continent)
  Dispersal[type=="New.Immigrants" & gain.continent==seed_continent, type:="Secondary Invader"]
  Dispersal[type=="New.Immigrants" & gain.continent!=seed_continent, type:="Primary Invader"]
  
  Dispersal.N<-Dispersal[, .(N=.N), by=list(year, seed_id, NB, DA, seed_continent, current_continent, previous_continent,
                                            type, gain.continent, loss.continent)]
  
  saveRDS(Dispersal.N, "../Data/Tables/N.Dispersal.NULL.rda")
  
  Speciation<-sp.with.bridge[type %in% c("Speciation")]
  table(Speciation$gain.continent)
  
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
  saveRDS(Speciation.N, "../Data/Tables/N.Speciation.NULL.rda")

}
df<-readRDS("../Data/Tables/N.Speciation.Extinction.Dispersal.NULL.rda")

df[,.(N=sum(N)), by=list(type)]
table(df$type)
richness.df<-readRDS("../Data/Tables/N.with.bridge.seed.continent.NULL.rda")
if (F){
  richness.sim<-readRDS("../Data/Tables/N.with.bridge.seed.continent.rda")
  richness.null<-readRDS("../Data/Tables/N.with.bridge.seed.continent.NULL.rda")
  
  colnames(richness.sim)[c(6, 7)]<-c("to_target_continent_sim", "in_source_continent_sim")
  colnames(richness.null)[c(6, 7)]<-c("to_target_continent_null", "in_source_continent_null")
  richness<-merge(richness.sim, richness.null, by=c("NB", "DA", "seed_id", "seed_continent"))

  ggplot(richness)+geom_point(aes(x=in_source_continent_sim, y=in_source_continent_null, color=seed_continent))+
    geom_abline()+
    scale_x_log10()+
    scale_y_log10()
  
  richness.sim<-readRDS("../Data/Tables/N.with.bridge.seed.continent.rda")
  
  richness.null<-readRDS("../Data/Tables/N.with.bridge.seed.continent.NULL.rda")
  
  
  richness.sim$type<-"Simulation"
  richness.null$type<-"NULL"
  richness.item<-rbindlist(list(richness.sim, richness.null))
  ggplot(richness.item)+
    geom_histogram(aes(x=in_source_continent, fill=type), bins=20, position = "dodge")+
    scale_x_log10()  +
    scale_y_log10()  
}
richness.df$label<-sprintf("%d.%s.%s", richness.df$seed_id, richness.df$NB, richness.df$DA)


hist(richness.df$N)
outliers<-richness.df[in_source_continent>1e4]$label
saveRDS(outliers, "../Data/Tables/outliers.null.rda")
table(df$NB)
df$label<-sprintf("%d.%s.%s", df$seed_id, df$NB, df$DA)
table(df$type)
remove.outliers<-F
seeds.all<-readRDS("../Data/Tables/random.seeds.threshold.by.nb.distance.99.rda")
if (remove.outliers==T){
  seeds.all<-seeds.all[!label %in% outliers]
}
rep.list<-list()
rep.list.all<-list()

rep.richness.list<-list()
rep.richness.list.all<-list()

for (rrrr in c(1:100)){
  print(rrrr)
  seeds<-seeds.all[rep==rrrr]
  item<-df[label %in% seeds$label]
  item.rep<-item[, .(N=sum(N),
                     N_Seed=length(unique(label))), 
                 by=list(NB, DA, seed_continent, type)]
  item.rep$rep<-rrrr
  rep.list[[rrrr]]<-item.rep
  item.rep<-item[, .(N=sum(N),
                     N_Seed=length(unique(label))), 
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

ls()
if (remove.outliers==F){
  saveRDS(rep.df, "../Data/Tables/N.Speciation.Extinction.Dispersal.rep.NULL.rda")
  saveRDS(rep.df.all, "../Data/Tables/N.Speciation.Extinction.Dispersal.all.rep.NULL.rda")

}else{
  saveRDS(rep.df, "../Data/Tables/N.Speciation.Extinction.Dispersal.rep.NULL.no.outliers.rda")
  saveRDS(rep.df.all, "../Data/Tables/N.Speciation.Extinction.Dispersal.all.rep.NULL.no.outliers.rda")

}

extinction<-rep.df.all[type %in% c("Failed Invader", "Extinction")]
extinction$continent<-extinction$seed_continent
extinction$other.continent<-ifelse(extinction$seed_continent=="South America",
                                   "North America", "South America")
extinction[type=="Failed Invader", continent:=other.continent]
if (F){
  extinction.all<-extinction
  extinction.all$event<-"Extinction"
  extinction.all$type<-ifelse(extinction.all$seed_continent==extinction.all$continent, 
                              "Native", "Immigrant")
  extinction.all$other.continent<-NULL
  
  ggplot(extinction.all)+geom_boxplot(aes(x=continent, y=N, color=type))
  
  local.extinction<-rep.df.all[type %in% c("Local Extinction", "Non-local Extinction")]
  local.extinction$continent<-local.extinction$seed_continent
  local.extinction$other.continent<-ifelse(local.extinction$seed_continent=="South America",
                                           "North America", "South America")
  local.extinction[type=="Non-local Extinction", continent:=other.continent]
  
  local.extinction.all<-local.extinction
  local.extinction.all$event<-"Local Extinction"
  local.extinction.all$type<-ifelse(local.extinction.all$seed_continent==local.extinction.all$continent, 
                                    "Native", "Immigrant")
  local.extinction.all$other.continent<-NULL
  
  ggplot(local.extinction.all)+geom_boxplot(aes(x=continent, y=N, color=type))
}
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
custom_colors <- c(
  "N to S" = color_n2s,
  "S to N" = color_s2n
)
dispersal.all$type<-factor(dispersal.all$type, levels=c("Primary Invader",
                                                        "Secondary Invader"),
                           labels=c("Primary invader",
                                    "Secondary invader"))
p.disp<-ggplot(dispersal.all)+geom_boxplot(aes(x=disp.type, y=N, color=disp.type))+
  facet_wrap(~type)+
  scale_color_manual(values=custom_colors)+
  labs(y="Number of species")+
  theme_bw()+
  theme(axis.title.x = element_blank(),
        legend.position = "none",
        strip.background = element_blank())
p.disp
dispersal.all.se<-dispersal.all[,.(mean=mean(N), sd=sd(N)),
                                by=list(type, disp.type)]
setorderv(dispersal.all.se, c("type", "disp.type"))
if (remove.outliers==F){
  to.doc(dispersal.all.se, "Number of dispersal events", 
         "../Figures/N.Speciation.Extinction.Dispersal/NULL.MODEL/N.Dispersal.NULL.docx",
         digits=2)
  
  
  ggsave(p.disp, filename="../Figures/N.Speciation.Extinction.Dispersal/NULL.MODEL/N.Dispersal.NULL.pdf", width=5, height=3)
  ggsave(p.disp, filename="../Figures/N.Speciation.Extinction.Dispersal/NULL.MODEL/N.Dispersal.NULL.png", width=5, height=3, bg="white")
  
}else{
  to.doc(dispersal.all.se, "Number of dispersal events", 
        "../Figures/N.Speciation.Extinction.Dispersal/NULL.MODEL.remove.outliers/N.Dispersal.NULL.no.outliers.docx",
         digits=2)
  ggsave(p.disp, filename="../Figures/N.Speciation.Extinction.Dispersal/NULL.MODEL.remove.outliers/N.Dispersal.NULL.no.outliers.pdf", width=5, height=3)
  ggsave(p.disp, filename="../Figures/N.Speciation.Extinction.Dispersal/NULL.MODEL.remove.outliers/N.Dispersal.NULL.no.outliers.png", width=5, height=3, bg="white")
  
}


#Dispersal by seeds

p.disp.seed<-ggplot(dispersal.all[type=="Primary invader"])+
  geom_boxplot(aes(x=disp.type, y=N_Seed, color=disp.type))+
  #facet_wrap(~type)+
  scale_color_manual(values=custom_colors)+
  labs(y="Number of seeds")+
  theme_bw()+
  theme(axis.title.x = element_blank(),
        legend.position = "none")
p.disp.seed
dispersal.all.se<-dispersal.all[,.(mean=mean(N_Seed), sd=sd(N_Seed)),
                                by=list(type, disp.type)]
setorderv(dispersal.all.se, c("type", "disp.type"))
if (remove.outliers==F){
  to.doc(dispersal.all.se, "Number of dispersal events by seed", 
         "../Figures/N.Speciation.Extinction.Dispersal/NULL.MODEL/N.Dispersal.Seed.NULL.docx",
         digits=2)
  ggsave(p.disp.seed, filename="../Figures/N.Speciation.Extinction.Dispersal/NULL.MODEL/N.Dispersal.Seed.NULL.pdf", width=3, height=3)
  ggsave(p.disp.seed, filename="../Figures/N.Speciation.Extinction.Dispersal/NULL.MODEL/N.Dispersal.Seed.NULL.png", width=3, height=3, bg="white")
}else{
  to.doc(dispersal.all.se, "Number of dispersal events by seed", 
         "../Figures/N.Speciation.Extinction.Dispersal/NULL.MODEL.remove.outliers/N.Dispersal.Seed.NULL.no.outliers.docx",
         digits=2)
  ggsave(p.disp.seed, filename="../Figures/N.Speciation.Extinction.Dispersal/NULL.MODEL.remove.outliers/N.Dispersal.Seed.NULL.no.outliers.pdf", width=3, height=3)
  ggsave(p.disp.seed, filename="../Figures/N.Speciation.Extinction.Dispersal/NULL.MODEL.remove.outliers/N.Dispersal.Seed.NULL.no.outliers.png", width=3, height=3, bg="white")
}
p<-p.disp.seed+p.disp+plot_layout(guides = "collect", widths = c(1, 2))
p
if (remove.outliers==F){
  ggsave(p, filename="../Figures/N.Speciation.Extinction.Dispersal/NULL.MODEL/N.Dispersal.Seed.and.Species.NULL.pdf",
         width=6, height=3)
  ggsave(p, filename="../Figures/N.Speciation.Extinction.Dispersal/NULL.MODEL/N.Dispersal.Seed.and.Species.NULL.png",
         width=6, height=3)
}else{
  ggsave(p, filename="../Figures/N.Speciation.Extinction.Dispersal/NULL.MODEL.remove.outliers/N.Dispersal.Seed.and.Species.NULL.no.outliers.pdf",
         width=6, height=3)
  ggsave(p, filename="../Figures/N.Speciation.Extinction.Dispersal/NULL.MODEL.remove.outliers/N.Dispersal.Seed.and.Species.NULL.no.outliers.png",
         width=6, height=3)
}
#Richness
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
all.df<-rbindlist(list(richness.all, speciation.all), fill = T)
table(all.df$type)
table(all.df$event)

all.df$event<-factor(all.df$event, 
                     levels=c("Speciation", "Extinction", "Local Extinction", "Richness"),
                     labels=c("Speciation", "Extinction", "Extirpation", "Richness"))

all.df$type<-factor(all.df$type, levels=c("Native", "Immigrant"))

p1<-ggplot(all.df[event %in% c("Speciation", "Extinction", "Extirpation")])+
  geom_boxplot(aes(x=continent, y=N, color=type))+
  facet_wrap(~event, nrow=1, scale="free")+
  labs(y="Number of events", color="Species type")+
  scale_color_manual(values=c("Native"=color_native, "Immigrant"=color_immigrant))+
  theme_bw()+
  theme(legend.position = "none",
        axis.title.x = element_blank())
p1
p2<-ggplot(all.df[event %in% c("Richness")])+
  geom_boxplot(aes(x=continent, y=N, color=type))+
  facet_wrap(~event, nrow=1, scale="free")+
  labs(y="Number of species", color="Species type")+
  scale_color_manual(values=c("Native"=color_native, "Immigrant"=color_immigrant))+
  theme_bw()+
  theme(legend.position = "bottom",
        axis.title.x = element_blank())
p2

#p<-p1/(p.disp+p2+plot_layout(guides = "collect", widths = c(2, 1))) & 
#  theme(legend.position = "bottom")
#p
p<-p1+p2+plot_layout(guides = "collect", widths = c(1, 1)) & 
  theme(legend.position = "bottom")
p
all.df.all<-all.df

all.df.se<-all.df[,.(mean=mean(N), sd=sd(N)),
                  by=list(event, continent, type)]
setorderv(all.df.se, c("event", "continent", "type"))
if (remove.outliers==F){
  to.doc(all.df.se, "Number of speciation and extinction events, and species richness for null model", 
         "../Figures/N.Speciation.Extinction.Dispersal/NULL.MODEL/N.Speciation.Extinction.Richness.NULL.docx",
         digits=2)
  ggsave(p, filename="../Figures/N.Speciation.Extinction.Dispersal/NULL.MODEL/N.Speciation.Extinction.Richness.NULL.pdf", 
         width=6, height=5)
  ggsave(p, filename="../Figures/N.Speciation.Extinction.Dispersal/NULL.MODEL/N.Speciation.Extinction.Richness.NULL.png", 
         width=6, height=5, bg="white")
}else{
  to.doc(all.df.se, "Number of speciation and extinction events, and species richness for null model", 
         "../Figures/N.Speciation.Extinction.Dispersal/NULL.MODEL.remove.outliers/N.Speciation.Extinction.Richness.NULL.no.outliers.docx",
         digits=2)
  ggsave(p, filename="../Figures/N.Speciation.Extinction.Dispersal/NULL.MODEL.remove.outliers/N.Speciation.Extinction.Richness.NULL.no.outliers.pdf", 
         width=6, height=5)
  ggsave(p, filename="../Figures/N.Speciation.Extinction.Dispersal/NULL.MODEL.remove.outliers/N.Speciation.Extinction.Richness.NULL.no.outliers.png", 
         width=6, height=5, bg="white")
}


p
#By NB & DA
if (F){
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
  
  
  local.extinction<-rep.df[type %in% c("Local Extinction", "Non-local Extinction")]
  local.extinction$continent<-local.extinction$seed_continent
  local.extinction$other.continent<-ifelse(local.extinction$seed_continent=="South America",
                                           "North America", "South America")
  local.extinction[type=="Non-local Extinction", continent:=other.continent]
  
  local.extinction.all<-local.extinction
  local.extinction.all$event<-"Local Extinction"
  local.extinction.all$type<-ifelse(local.extinction.all$seed_continent==local.extinction.all$continent, 
                                    "Native", "Immigrant")
  local.extinction.all$other.continent<-NULL
  
  ggplot(local.extinction.all)+geom_boxplot(aes(x=continent, y=N, color=type))+
    facet_grid(NB~DA)
  
}
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
dispersal.all$type<-factor(dispersal.all$type, 
                           levels=c("Primary Invader",
                                    "Secondary Invader"),
                           labels=c("Primary invader",
                                    "Secondary invader"))
p.disp<-ggplot(dispersal.all)+
  geom_boxplot(aes(x=disp.type, y=N, color=type))+
  facet_grid(DA~NB, scale="free")+
  scale_color_manual(values=c("Primary invader"=color_low, "Secondary invader"=color_high))+
  labs(y="Number of species", color="Type of invader")+
  theme_bw()+
  theme(axis.title.x = element_blank(),
        legend.position = "bottom")
p.disp

dispersal.all.se<-dispersal.all[,.(mean=mean(N), sd=sd(N)),
                                by=list(type, disp.type, NB, DA)]
setorderv(dispersal.all.se, c("type", "disp.type", "NB", "DA"))
if (remove.outliers==F){
  to.doc(dispersal.all.se, "Number of dispersal events for null model", 
         "../Figures/N.Speciation.Extinction.Dispersal/NULL.MODEL/N.Dispersal.details.NULL.docx",
         digits=2)
  ggsave(p.disp, filename="../Figures/N.Speciation.Extinction.Dispersal/NULL.MODEL/N.Dispersal.details.NULL.pdf", width=10, height=5)
  ggsave(p.disp, filename="../Figures/N.Speciation.Extinction.Dispersal/NULL.MODEL/N.Dispersal.details.NULL.png", width=10, height=5, bg="white")
}else{
  to.doc(dispersal.all.se, "Number of dispersal events for null model", 
         "../Figures/N.Speciation.Extinction.Dispersal/NULL.MODEL.remove.outliers/N.Dispersal.details.NULL.no.outliers.docx",
         digits=2)
  ggsave(p.disp, filename="../Figures/N.Speciation.Extinction.Dispersal/NULL.MODEL.remove.outliers/N.Dispersal.details.NULL.no.outliers.pdf", width=10, height=5)
  ggsave(p.disp, filename="../Figures/N.Speciation.Extinction.Dispersal/NULL.MODEL.remove.outliers/N.Dispersal.details.NULL.no.outliers.png", width=10, height=5, bg="white")
}


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
all.df<-rbindlist(list(richness.all, speciation.all), use.names=T, fill=T)
table(all.df$type)
table(all.df$event)

all.df$type<-factor(all.df$type, levels=c("Native", "Immigrant"))
all.df$NB<-factor(all.df$NB, 
                  levels = c("BROAD", "BIG", "MODERATE", "NARROW"), 
                  labels = c("BROAD", "MODERATE", "NARROW", "TINY"))
all.df$event<-factor(all.df$event, levels=c("Speciation", "Extinction", "Local Extinction", "Richness"),
                     labels=c("Speciation", "Extinction", "Extirpation", "Richness"))
all.df$continent<-ifelse(all.df$continent=="North America", "N", "S")
p1<-ggplot(all.df[event %in% c("Speciation", "Extinction", "Extirpation")])+
  geom_boxplot(aes(x=continent, y=N, color=type))+
  facet_grid(event~NB+DA, scale="free")+
  labs(y="Number of events", color="Species type")+
  scale_color_manual(values=c("Native"=color_native, "Immigrant"=color_immigrant))+
  theme_bw()+
  theme(legend.position = "bottom",
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank())
p1

p2<-ggplot(all.df[event %in% c("Richness")])+
  geom_boxplot(aes(x=continent, y=N, color=type))+
  facet_grid(event~NB+DA, scale="free")+
  labs(y="Number of species", color="Species type")+
  scale_color_manual(values=c("Native"=color_native, "Immigrant"=color_immigrant))+
  theme_bw()+
  theme(legend.position = "bottom",
        axis.title.x = element_blank(),
        strip.text.x = element_blank(),
        strip.background.x = element_blank())
p2
p <- (p1 / p2) + 
  plot_layout(heights = c(1, 1), guides = "collect") & 
  theme(legend.position = "bottom")
p

all.df.nb.da<-all.df

all.df.se<-all.df[,.(mean=mean(N), sd=sd(N)),
                  by=list(event, continent, type, NB, DA)]
setorderv(all.df.se, c("event", "continent", "type", "NB", "DA"))
if (remove.outliers==F){
  to.doc(all.df.se, "Number of speciation and extinction events, and species richness", 
         "../Figures/N.Speciation.Extinction.Dispersal/NULL.MODEL/N.Speciation.Extinction.Richness.details.NULL.docx",
         digits=2)
  ggsave(p, filename="../Figures/N.Speciation.Extinction.Dispersal/NULL.MODEL/N.Speciation.Extinction.Richness.details.NULL.pdf", 
         width=12, height=6)
  ggsave(p, filename="../Figures/N.Speciation.Extinction.Dispersal/NULL.MODEL/N.Speciation.Extinction.Richness.details.NULL.png", 
         width=12, height=6, bg="white")
}else{
  
  
  to.doc(all.df.se, "Number of speciation and extinction events, and species richness", 
         "../Figures/N.Speciation.Extinction.Dispersal/NULL.MODEL.remove.outliers/N.Speciation.Extinction.Richness.details.NULL.no.outliers.docx",
         digits=2)
  ggsave(p, filename="../Figures/N.Speciation.Extinction.Dispersal/NULL.MODEL.remove.outliers/N.Speciation.Extinction.Richness.details.NULL.no.outliers.pdf", 
         width=12, height=6)
  ggsave(p, filename="../Figures/N.Speciation.Extinction.Dispersal/NULL.MODEL.remove.outliers/N.Speciation.Extinction.Richness.details.NULL.no.outliers.png", 
         width=12, height=6, bg="white")
}






##For Percentage

richness_merge<-all.df.all[event %in% c("Richness")]
richness_merge_N_Native<-richness_merge[type=="Native"]
richness_merge_N_Immigrant<-richness_merge[type=="Immigrant"]
colnames(richness_merge_N_Native)[3]<-"Native"
colnames(richness_merge_N_Immigrant)[3]<-"Immigrant"
richness_merge<-merge(richness_merge_N_Native, richness_merge_N_Immigrant, 
                      by=c("rep", "continent"), all=T)
richness_merge$Per_Immigrant<-richness_merge$Immigrant/(richness_merge$Native+richness_merge$Immigrant)
richness_merge$Per_Native<-richness_merge$Native/(richness_merge$Native+richness_merge$Immigrant)

richness_merge[, c("rep", "continent", "Per_Immigrant", "Per_Native")]

richness_se<-richness_merge[,.(N_Native=mean(Native), 
                               sd_Native=sd(Native),
                               N_Immigrant=mean(Immigrant), 
                               sd_Immigrant=sd(Immigrant),
                               N_Per_Immigrant=mean(Per_Immigrant), 
                               sd_Per_Immigrant=sd(Per_Immigrant)), 
                            by=list(continent)]
richness_se

richness_merge_Native<-richness_merge
richness_merge_Native<-data.table(rep=richness_merge_Native$rep,
                                  continent=richness_merge_Native$continent,
                                  type="Native",
                                  per=richness_merge_Native$Per_Native)
richness_merge_Immigrant<-richness_merge
richness_merge_Immigrant<-data.table(rep=richness_merge_Immigrant$rep,
                                     continent=richness_merge_Immigrant$continent,
                                     type="Immigrant",
                                     per=richness_merge_Immigrant$Per_Immigrant)

richness_merge_df<-rbindlist(list(richness_merge_Native, richness_merge_Immigrant))

richness_merge_df$type<-factor(richness_merge_df$type, levels=c("Native", "Immigrant"))
p2<-ggplot(richness_merge_df)+
  geom_boxplot(aes(x=continent, y=per, color=type))+
  labs(y="Percentage", color="Species type")+
  scale_color_manual(values=c("Native"=color_native, "Immigrant"=color_immigrant))+
  theme_bw()+
  theme(legend.position = "bottom",
        axis.title.x = element_blank(),
        #axis.title.y = element_blank()
  )
p2

richness_nb_da_merge<-all.df.nb.da[event %in% c("Richness")]
richness_nb_da_merge_N_Native<-richness_nb_da_merge[type=="Native"]
richness_nb_da_merge_N_Immigrant<-richness_nb_da_merge[type=="Immigrant"]
colnames(richness_nb_da_merge_N_Native)[3]<-"Native"
colnames(richness_nb_da_merge_N_Immigrant)[3]<-"Immigrant"
richness_nb_da_merge<-merge(richness_nb_da_merge_N_Native, richness_nb_da_merge_N_Immigrant, 
                            by=c("rep", "continent", "NB", "DA"), all=T)
richness_nb_da_merge$Per_Immigrant<-richness_nb_da_merge$Immigrant/(richness_nb_da_merge$Native+richness_nb_da_merge$Immigrant)
richness_nb_da_merge$Per_Native<-richness_nb_da_merge$Native/(richness_nb_da_merge$Native+richness_nb_da_merge$Immigrant)

richness_nb_da_merge[, c("rep", "continent", "Per_Immigrant", "Per_Native", "NB", "DA")]

richness_se_nb_da<-richness_nb_da_merge[,.(N_Native=mean(Native), 
                                           sd_Native=sd(Native),
                                           N_Immigrant=mean(Immigrant), 
                                           sd_Immigrant=sd(Immigrant),
                                           N_Per_Immigrant=mean(Per_Immigrant), 
                                           sd_Per_Immigrant=sd(Per_Immigrant)), 
                                        by=list(continent, NB, DA)]
richness_se_nb_da


richness_merge_Native_nb_da<-richness_nb_da_merge
richness_merge_Native_nb_da<-data.table(rep=richness_merge_Native_nb_da$rep,
                                        continent=richness_merge_Native_nb_da$continent,
                                        type="Native",
                                        per=richness_merge_Native_nb_da$Per_Native,
                                        NB=richness_nb_da_merge$NB,
                                        DA=richness_nb_da_merge$DA)
richness_merge_Immigrant_nb_da<-richness_nb_da_merge
richness_merge_Immigrant_nb_da<-data.table(rep=richness_merge_Immigrant_nb_da$rep,
                                           continent=richness_merge_Immigrant_nb_da$continent,
                                           type="Immigrant",
                                           per=richness_merge_Immigrant_nb_da$Per_Immigrant,
                                           NB=richness_nb_da_merge$NB,
                                           DA=richness_nb_da_merge$DA)

richness_merge_df_nb_da<-rbindlist(list(richness_merge_Native_nb_da, richness_merge_Immigrant_nb_da))

richness_merge_df_nb_da$type<-factor(richness_merge_df_nb_da$type, levels=c("Native", "Immigrant"))

richness_merge_df_nb_da$continent<-factor(richness_merge_df_nb_da$continent, levels=c("N", "S"),
                                          labels=c("NA", "SA"))
p3<-ggplot(richness_merge_df_nb_da)+
  geom_boxplot(aes(x=continent, y=per, color=type))+
  labs(y="Percentage", color="Species type")+
  scale_color_manual(values=c("Native"=color_native, "Immigrant"=color_immigrant))+
  facet_grid(DA~NB)+
  theme_bw()+
  theme(legend.position = "bottom",
        axis.title.x = element_blank()
        #axis.title.y = element_blank()
  )
p3

p<-p2/p3
p
if (remove.outliers==F){
  ggsave(p2, filename="../Figures/N.Speciation.Extinction.Dispersal/NULL.MODEL/N.Richness.Per.NULL.pdf", 
         width=4, height=3, bg="white")
  
  ggsave(p, filename="../Figures/N.Speciation.Extinction.Dispersal/NULL.MODEL/N.Richness.Per.ALL.NULL.pdf", 
         width=8, height=6, bg="white")
}else{
  ggsave(p2, filename="../Figures/N.Speciation.Extinction.Dispersal/NULL.MODEL.remove.outliers/N.Richness.Per.NULL.no.outliers.pdf", 
         width=4, height=3, bg="white")
  
  ggsave(p, filename="../Figures/N.Speciation.Extinction.Dispersal/NULL.MODEL.remove.outliers/N.Richness.Per.ALL.NULL.no.outliers.pdf", 
         width=8, height=6, bg="white")
}




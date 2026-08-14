library(data.table)
library(ggplot2)
library(ggrepel)
library(ggh4x)
library(sf)
library(dplyr)
library(patchwork)
setwd("/media/huijieqiao/Butterfly/GABI/GABI")
source("Figures/common.r")
df<-readRDS("../Data/Tables/N.Speciation.Extinction.Dispersal.rda")

df[,.(N=sum(N)), by=list(type)]
table(df$type)
richness.df<-readRDS("../Data/Tables/N.with.bridge.seed.continent.rda")
richness.df$label<-sprintf("%d.%s.%s", richness.df$seed_id, richness.df$NB, richness.df$DA)


table(df$NB)
df$label<-sprintf("%d.%s.%s", df$seed_id, df$NB, df$DA)
table(df$type)
xxxx<-sprintf("../Data/Tables/outliers/random.seeds.threshold.by.nb.distance.%d.rda", 50)

seeds.all<-readRDS(xxxx)
length(unique(seeds.all$seed_label))
#seeds.all<-seeds.all[!label %in% c("9078.BROAD.GOOD", "11119.BIG.GOOD", "10794.BIG.GOOD", "12104.BROAD.GOOD")]


#seeds.all<-readRDS("../Data/Tables/random.seeds.95.rda")



outliers<-readRDS("../Data/Tables/outliers.1801.rda")
outliers.sum<-outliers[,.(N_SPECIES=sum(N_SPECIES)), by=.(seed_id)]
setorderv(outliers.sum, "N_SPECIES", -1)
outliers.sum$Order<-c(1:nrow(outliers.sum))
outliers.sum$N_SPECIES<-NULL
seeds.all.n.sp<-merge(outliers.sum, seeds.all, by="seed_id", all.y=T)

seeds.all.N<-seeds.all.n.sp[, .(Used=.N),
                            by=list(seed_id, Order, continent, nb, da, N_SPECIES, min.dist, seed_label)]

seeds.all.N$contribution<-seeds.all.N$N_SPECIES * seeds.all.N$Used
contributions<-seeds.all.N[,.(contribution=sum(contribution)), 
                           by=.(seed_id, Order, continent)]
setorderv(contributions, "contribution", -1)

contributions.df<-contributions[!is.na(Order)]
contributions.df[, Order:=seq_len(.N), by = continent]
ggplot(contributions.df)+
  geom_line(aes(x=Order, y=contribution, color=continent))+
  geom_point(aes(x=Order, y=contribution, color=continent))

plist<-NULL
for (i in c(1:10)){
  seeds.all.removed<-seeds.all[!seed_id %in% 
                                 contributions.df[Order<=i & continent=="South America"]$seed_id]
  
  rep.list<-list()
  rep.list.all<-list()
  
  rep.richness.list<-list()
  rep.richness.list.all<-list()
  
  for (rrrr in c(1:100)){
    print(paste(i, rrrr))
    seeds<-seeds.all.removed[rep==rrrr]
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
  rep.richness.df.all_se<-rep.richness.df.all[]
  
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
  
  #Dispersal by species
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
  
  p<-p.disp.seed+p.disp+plot_layout(guides = "collect", widths = c(1, 2))
  p
  
  
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
  all.df<-rbindlist(list(richness.all, speciation.all, extinction.all, local.extinction.all), fill=T)
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
          axis.title.x = element_blank(),
          strip.background = element_blank())
  p1
  p2<-ggplot(all.df[event %in% c("Richness")])+
    geom_boxplot(aes(x=continent, y=N, color=type))+
    facet_wrap(~event, nrow=1, scale="free")+
    labs(y="Number of species", color="Species type", title=sprintf("Removed %d seed(s)", i))+
    scale_color_manual(values=c("Native"=color_native, "Immigrant"=color_immigrant))+
    theme_bw()+
    theme(legend.position = "bottom",
          axis.title.x = element_blank(),
          strip.background = element_blank()
    )
  p2
  all.df.all<-all.df
  
  p<-p1+p2  +plot_layout(guides = "collect", widths = c(3.5, 1)) & 
    theme(legend.position = "bottom")
  
  p
  if (is.null(plist)){
    plist<-p
  }else{
    plist<-plist/p
  }
  
  
}
plist

ggsave(plist, filename="../Figures/Outliers/outlier.contributions.png", width=6, height=20)
  
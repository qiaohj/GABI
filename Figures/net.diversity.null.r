library(data.table)
library(ggplot2)
library(sf)
library(lme4)
library(broom.mixed)
library(ranger)
setDTthreads(30)
setwd("/media/huijieqiao/Butterfly/GABI/GABI")
source("Figures/common.r")
if (F){
  sp.with.bridge<-readRDS("../Data/Tables/sp_full_continents.NULL.rda")
  sp.with.bridge[sp_id=="10050" & NB=="BIG" & DA=="GOOD"]
  
  sp.with.bridge.NA<-sp.with.bridge[current_continent %in% c("North America", "Two continents")]
  sp.with.bridge.NA$current_continent<-"North America"
  sp.with.bridge.SA<-sp.with.bridge[current_continent %in% c("South America", "Two continents")]
  sp.with.bridge.SA$current_continent<-"South America"
  sp.full<-rbindlist(list(sp.with.bridge.NA, sp.with.bridge.SA))
  year_window_size<-50
  sp.full$year_window<-floor(sp.full$year/year_window_size)*year_window_size
  
  N.species<-sp.full[,.(N_SP_FULL=length(unique(sp_id)),
                        N_SP = length(unique(sp_id[year_window == year]))), 
                     by=list(seed_id, year_window, 
                             NB, DA, current_continent, seed_continent)]
  ggplot(N.species)+
    geom_point(aes(x=N_SP_FULL, N_SP, color=current_continent))+
    geom_abline()+
    coord_equal()+
    facet_grid(DA~NB)
  
  N.species$label<-sprintf("%d.%s.%s", N.species$seed_id, N.species$NB, N.species$DA)
  N.species$type<-ifelse(N.species$current_continent==N.species$seed_continent, "Native", "Immigrant")
  table(N.species$current_continent)
  seeds.all<-readRDS("../Data/Tables/random.seeds.threshold.by.nb.distance.rda")
  outliers<-readRDS("../Data/Tables/outliers.null.rda")
  seeds.all<-seeds.all[!(label %in% outliers)]
  rep.list<-list()
  rep.all.list<-list()
  
  for (rrrr in c(1:100)){
    print(rrrr)
    seeds<-seeds.all[rep==rrrr]
    item<-N.species[label %in% seeds$label]
    item.N<-item[,.(N_SP=sum(N_SP), rep=rrrr), by=list(year_window, NB, DA, seed_continent, type)]
    rep.list[[rrrr]]<-item.N
    item.N<-item[,.(N_SP=sum(N_SP), rep=rrrr), by=list(year_window, seed_continent, type)]
    rep.all.list[[rrrr]]<-item.N
  }
  rep.df<-rbindlist(rep.list)
  rep.df.all<-rbindlist(rep.all.list)
  
  #saveRDS(rep.df, sprintf("../Data/Tables/N.net.diversity.rep.window.size.%d.NULL.rda", year_window_size))
  #saveRDS(rep.df.all, sprintf("../Data/Tables/N.net.diversity.all.window.size.%d.rep.NULL.rda", year_window_size))
  saveRDS(rep.df, sprintf("../Data/Tables/N.net.diversity.rep.window.size.%d.NULL.no.outliers.rda", year_window_size))
  saveRDS(rep.df.all, sprintf("../Data/Tables/N.net.diversity.all.window.size.%d.rep.NULL.no.outliers.rda", year_window_size))
  
  
  
}

if (F){
  df<-readRDS("../Data/Tables/N.Extinction.NULL.rda")
  df$label<-sprintf("%d.%s.%s", df$seed_id, df$NB, df$DA)
  df$species.type<-"Unknown"
  df[current_continent==seed_continent & type=="Local.Extinction", species.type:="Immigrant"]
  df[current_continent!=seed_continent & type=="Local.Extinction", species.type:="Native"]
  df[type=="Extinction" & previous_continent==seed_continent, species.type:="Native"]
  df[type=="Extinction" & previous_continent!=seed_continent, species.type:="Immigrant"]
  df[type=="Extinction" & previous_continent %in% c("bridge1"), species.type:="Isthmus"]
  df[type=="Extinction" & previous_continent %in% c("bridge2"), species.type:="Caribbean"]
  
  df[species.type=="Unknown"]
  table(df$species.type)
  year_window_size<-50
  df$year_window<-floor(df$year/year_window_size)*year_window_size
  
  seeds.all<-readRDS("../Data/Tables/random.seeds.threshold.by.nb.distance.rda")
  outliers<-readRDS("../Data/Tables/outliers.null.rda")
  seeds.all<-seeds.all[!(label %in% outliers)]
  rep.list<-list()
  rep.all.list<-list()
  rrrr=1
  for (rrrr in c(1:100)){
    print(rrrr)
    seeds<-seeds.all[rep==rrrr]
    item<-df[label %in% seeds$label]
    item.N<-item[,.(N=sum(N), rep=rrrr), by=list(year_window, NB, DA, seed_continent, type, species.type)]
    rep.list[[rrrr]]<-item.N
    item.N<-item[,.(N=sum(N), rep=rrrr), by=list(year_window, seed_continent, type, species.type)]
    rep.all.list[[rrrr]]<-item.N
  }
  rep.df<-rbindlist(rep.list)
  rep.df.all<-rbindlist(rep.all.list)
  
  #saveRDS(rep.df, sprintf("../Data/Tables/N.Extinction.rep.window.size.%d.NULL.rda", year_window_size))
  #saveRDS(rep.df.all, sprintf("../Data/Tables/N.Extinction.all.window.size.%d.rep.NULL.rda", year_window_size))
  saveRDS(rep.df, sprintf("../Data/Tables/N.Extinction.rep.window.size.%d.NULL.no.outliers.rda", year_window_size))
  saveRDS(rep.df.all, sprintf("../Data/Tables/N.Extinction.all.window.size.%d.rep.NULL.no.outliers.rda", year_window_size))
  
  
}

if (F){
  df<-readRDS("../Data/Tables/N.Speciation.NULL.rda")
  df$label<-sprintf("%d.%s.%s", df$seed_id, df$NB, df$DA)
  df$species.type<-"Unknown"
  df[current_continent==seed_continent, species.type:="Immigrant"]
  df[current_continent!=seed_continent, species.type:="Native"]
  table(df$species.type)
  df[species.type=="Unknown"]
  table(df$species.type)
  year_window_size<-50
  df$year_window<-floor(df$year/year_window_size)*year_window_size
  
  seeds.all<-readRDS("../Data/Tables/random.seeds.threshold.by.nb.distance.rda")
  outliers<-readRDS("../Data/Tables/outliers.null.rda")
  seeds.all<-seeds.all[!(label %in% outliers)]
  rep.list<-list()
  rep.all.list<-list()
  rrrr=1
  for (rrrr in c(1:100)){
    print(rrrr)
    seeds<-seeds.all[rep==rrrr]
    item<-df[label %in% seeds$label]
    item.N<-item[,.(N=sum(N), rep=rrrr), by=list(year_window, NB, DA, seed_continent, type, species.type)]
    rep.list[[rrrr]]<-item.N
    item.N<-item[,.(N=sum(N), rep=rrrr), by=list(year_window, seed_continent, type, species.type)]
    rep.all.list[[rrrr]]<-item.N
  }
  rep.df<-rbindlist(rep.list)
  rep.df.all<-rbindlist(rep.all.list)
  
  #saveRDS(rep.df, sprintf("../Data/Tables/N.Speciation.rep.window.size.%d.NULL.rda", year_window_size))
  #saveRDS(rep.df.all, sprintf("../Data/Tables/N.Speciation.all.window.size.%d.rep.NULL.rda", year_window_size))
  saveRDS(rep.df, sprintf("../Data/Tables/N.Speciation.rep.window.size.%d.NULL.no.outliers.rda", year_window_size))
  saveRDS(rep.df.all, sprintf("../Data/Tables/N.Speciation.all.window.size.%d.rep.NULL.no.outliers.rda", year_window_size))
  
  
}


if (F){
  df<-readRDS("../Data/Tables/N.Dispersal.NULL.rda")
  df$label<-sprintf("%d.%s.%s", df$seed_id, df$NB, df$DA)
  year_window_size<-50
  df$year_window<-floor(df$year/year_window_size)*year_window_size
  
  seeds.all<-readRDS("../Data/Tables/random.seeds.threshold.by.nb.distance.rda")
  outliers<-readRDS("../Data/Tables/outliers.null.rda")
  seeds.all<-seeds.all[!(label %in% outliers)]
  rep.list<-list()
  rep.all.list<-list()
  rrrr=1
  for (rrrr in c(1:100)){
    print(rrrr)
    seeds<-seeds.all[rep==rrrr]
    item<-df[label %in% seeds$label]
    item.N<-item[,.(N=sum(N), rep=rrrr), by=list(year_window, NB, DA, seed_continent, type)]
    rep.list[[rrrr]]<-item.N
    item.N<-item[,.(N=sum(N), rep=rrrr), by=list(year_window, seed_continent, type)]
    rep.all.list[[rrrr]]<-item.N
  }
  rep.df<-rbindlist(rep.list)
  rep.df.all<-rbindlist(rep.all.list)
  
  #saveRDS(rep.df, sprintf("../Data/Tables/N.Dispersal.rep.window.size.%d.NULL.rda", year_window_size))
  #saveRDS(rep.df.all, sprintf("../Data/Tables/N.Dispersal.all.window.size.%d.rep.NULL.rda", year_window_size))
  saveRDS(rep.df, sprintf("../Data/Tables/N.Dispersal.rep.window.size.%d.NULL.no.outliers.rda", year_window_size))
  saveRDS(rep.df.all, sprintf("../Data/Tables/N.Dispersal.all.window.size.%d.rep.NULL.no.outliers.rda", year_window_size))
  
  
}
#Net diversification rate
year_window_size<-50
rep.df.sp<-readRDS(sprintf("../Data/Tables/N.net.diversity.rep.window.size.%d.NULL.rda", year_window_size))
rep.df.all.sp<-readRDS(sprintf("../Data/Tables/N.net.diversity.all.window.size.%d.rep.NULL.rda", year_window_size))
setorder(rep.df.all.sp, seed_continent, type, rep, year_window)
rep.df.all.sp[, net_div_rate := (N_SP-shift(N_SP)) / shift(N_SP), by = .(seed_continent, type, rep)]

rep.df.all.sp$continent<-rep.df.all.sp$seed_continent
rep.df.all.sp$other_continent<-ifelse(rep.df.all.sp$continent=="North America", "South America", "North America")
rep.df.all.sp[continent==other_continent]
rep.df.all.sp[type=="Immigrant", continent:=other_continent]
rep.df.all.sp$seed_continent<-factor(rep.df.all.sp$seed_continent, levels=c("North America", "South America"),
                                     labels=c("North American Origin", "South American Origin"))
rep.df.all.sp$type<-factor(rep.df.all.sp$type, levels=c("Native", "Immigrant"),
                           labels=c("Native", "Immigrant"))

p1<-ggplot(rep.df.all.sp[between(year_window, -1800, -50)])+
  geom_smooth(aes(x=year_window, y=net_div_rate, color=type), 
              method = "loess", span = 0.1, se = T, linewidth = 1) + 
  geom_point(aes(x=year_window, y=net_div_rate, color=type)) + 
  scale_color_manual(values = c("Native" = color_native, "Immigrant" = color_immigrant)) +
  labs(x = "Year", y = "Net Diversification Rate", 
       color = "Species Type") +
  theme_classic() +
  theme(
    legend.position = "bottom",
    axis.title = element_text(face = "bold")
  )+facet_wrap(~seed_continent, nrow=2)
p1
p2<-ggplot(rep.df.all.sp[between(year_window, -1800, -50)])+
  geom_smooth(aes(x=year_window, y=net_div_rate, color=type), 
              method = "loess", span = 0.1, se = T, linewidth = 1) + 
  #geom_point(aes(x=year_window, y=net_div_rate, color=type)) + 
  scale_color_manual(values = c("Native" = color_native, "Immigrant" = color_immigrant)) +
  labs(x = "Year", y = "Net Diversification Rate", 
       color = "Species Type") +
  theme_classic() +
  theme(
    legend.position = "bottom",
    axis.title = element_text(face = "bold")
  )+facet_wrap(~continent, nrow=2)
p2
p3<-ggplot(rep.df.all.sp[between(year_window, -1800, -50)])+
  geom_smooth(aes(x=year_window, y=N_SP, color=type), 
              method = "loess", span = 0.1, se = T, linewidth = 1) + 
  #geom_point(aes(x=year_window, y=N_SP, color=type)) + 
  scale_color_manual(values = c("Native" = color_native, "Immigrant" = color_immigrant)) +
  labs(x = "Year", y = "Species Richness", 
       color = "Species Type") +
  theme_classic() +
  theme(
    legend.position = "bottom",
    axis.title = element_text(face = "bold")
  )+facet_wrap(~continent, nrow=2)
p3

ggsave(p1, filename="../Figures/NET/Net.Diversification.Rate.origin.NULL.pdf", width=8, height=5)
ggsave(p1, filename="../Figures/NET/Net.Diversification.Rate.origin.NULL.png", width=8, height=5, bg="white")

ggsave(p2, filename="../Figures/NET/Net.Diversification.Rate.continent.NULL.pdf", width=8, height=5)
ggsave(p2, filename="../Figures/NET/Net.Diversification.Rate.continent.NULL.png", width=8, height=5, bg="white")

ggsave(p3, filename="../Figures/NET/N.Species.continent.NULL.pdf", width=8, height=5)
ggsave(p3, filename="../Figures/NET/N.Species.continent.NULL.png", width=8, height=5, bg="white")


#Extinction
year_window_size<-50
rep.df<-readRDS(sprintf("../Data/Tables/N.Extinction.rep.window.size.%d.NULL.rda", year_window_size))
rep.df.all<-readRDS(sprintf("../Data/Tables/N.Extinction.all.window.size.%d.rep.NULL.rda", year_window_size))
colnames(rep.df.all.sp)[3]<-"species.type"


setorder(rep.df.all, seed_continent, type, species.type, rep, year_window)
table(rep.df.all$species.type)

rep.df.all[year_window==-500]
rep.df.all$species.type<-factor(rep.df.all$species.type, 
                                levels=c("Native", "Immigrant", "Isthmus", "Caribbean"))
rep.df.all$continent<-rep.df.all$seed_continent
rep.df.all$other_continent<-ifelse(rep.df.all$continent=="North America", "South America", "North America")
rep.df.all[species.type=="Immigrant", continent:=other_continent]
rep.df.all[species.type=="Native", continent:=seed_continent]
rep.df.all[species.type=="Isthmus", continent:="Isthmus"]
rep.df.all[species.type=="Caribbean", continent:="Caribbean"]

rep.df.all$seed_continent<-factor(rep.df.all$seed_continent, levels=c("North America", "South America"),
                                  labels=c("North American Origin", "South American Origin"))
table(rep.df.all$type)
all.comb<-data.table(expand.grid(year_window=unique(rep.df.all$year_window),
                                 seed_continent=unique(rep.df.all$seed_continent),
                                 species.type=unique(rep.df.all$species.type),
                                 rep=unique(rep.df.all$rep),
                                 continent=unique(rep.df.all$continent),
                                 other_continent=unique(rep.df.all$other_continent),
                                 type=unique(rep.df.all$type), stringsAsFactors = F))
rep.df.all<-merge(rep.df.all, all.comb, by=c("year_window", "seed_continent", "species.type", 
                                             "rep", "continent", "other_continent", "type"), all=T)
rep.df.all[is.na(type)]
rep.df.all<-merge(rep.df.all, rep.df.all.sp, by=c("year_window", "seed_continent", "species.type", 
                                                  "rep", "continent", "other_continent"), all=T)
rep.df.all[is.na(N), N:=0]
rep.df.all<-rep.df.all[!is.na(N_SP)]

rep.df.all$net_extinction<-rep.df.all$N/rep.df.all$N_SP
rep.df.all[species.type %in% c("Native", "Immigrant") & continent %in% c("North America", "South America")]
rep.df.all.extinction<-rep.df.all[species.type %in% c("Native", "Immigrant") & 
                                    continent %in% c("North America", "South America") &
                                    !is.na(type)]

p1<-ggplot(rep.df.all[between(year_window, -1800, -50) & type!="Local.Extinction"])+
  geom_smooth(aes(x=year_window, y=N, color=species.type), 
              method = "loess", span = 0.1, se = FALSE, linewidth = 1) + 
  #geom_point(aes(x=year_window, y=N, color=species.type)) + 
  scale_color_manual(values = c("Native" = color_native, 
                                "Immigrant" = color_immigrant, 
                                "Isthmus" = color_mid2,
                                "Caribbean" = color_2)) +
  labs(x = "Year", y = "Number of Extinction", 
       color = "Species Type") +
  scale_y_sqrt()+
  theme_bw() +
  theme(
    legend.position = "bottom",
    axis.title = element_text(face = "bold")
  )+facet_wrap(~seed_continent, nrow=2, scale="free")

p1

rep.df.all.extinction$type<-factor(rep.df.all.extinction$type, levels=c("Extinction", "Local.Extinction"),
                                   labels=c("Extinction", "Local Extinction"))
p2<-ggplot(rep.df.all.extinction[between(year_window, -1800, -50) & 
                                   continent %in% c("North America", "South America")])+
  geom_smooth(aes(x=year_window, y=net_extinction, color=species.type), 
              method = "loess", span = 0.1, se = FALSE, linewidth = 1) + 
  #geom_point(aes(x=year_window, y=N, color=species.type)) + 
  scale_color_manual(values = c("Native" = color_native, 
                                "Immigrant" = color_immigrant, 
                                "Isthmus" = color_mid2,
                                "Caribbean" = color_2)) +
  labs(x = "Year", y = "Extinction / Species", 
       color = "Species Type") +
  scale_y_sqrt()+
  theme_bw() +
  theme(
    legend.position = "bottom",
    axis.title = element_text(face = "bold")
  )+facet_grid(type~continent, scale="free")
p2

ggsave(p1, filename="../Figures/NET/Extinction.origin.NULL.pdf", width=8, height=5)
ggsave(p1, filename="../Figures/NET/Extinction.origin.NULL.png", width=8, height=5, bg="white")

ggsave(p2, filename="../Figures/NET/Net.Extinction.continent.NULL.pdf", width=8, height=6)
ggsave(p2, filename="../Figures/NET/Net.Extinction.continent.NULL.png", width=8, height=6, bg="white")


#Speciation
year_window_size<-50
rep.df<-readRDS(sprintf("../Data/Tables/N.Speciation.rep.window.size.%d.NULL.rda", year_window_size))
rep.df.all<-readRDS(sprintf("../Data/Tables/N.Speciation.all.window.size.%d.rep.NULL.rda", year_window_size))

setorder(rep.df.all, seed_continent, type, species.type, rep, year_window)
table(rep.df.all$species.type)

rep.df.all[year_window==-500]
rep.df.all$species.type<-factor(rep.df.all$species.type, levels=c("Native", "Immigrant", "Isthmus", "Caribbean"))
rep.df.all$continent<-rep.df.all$seed_continent
rep.df.all$other_continent<-ifelse(rep.df.all$continent=="North America", "South America", "North America")
rep.df.all[species.type=="Immigrant", continent:=other_continent]
rep.df.all[species.type=="Native", continent:=seed_continent]
rep.df.all[species.type=="Isthmus", continent:="Isthmus"]
rep.df.all[species.type=="Caribbean", continent:="Caribbean"]
rep.df.all$seed_continent<-factor(rep.df.all$seed_continent, levels=c("North America", "South America"),
                                  labels=c("North American Origin", "South American Origin"))

all.comb<-data.table(expand.grid(year_window=unique(rep.df.all$year_window),
                                 seed_continent=unique(rep.df.all$seed_continent),
                                 species.type=unique(rep.df.all$species.type),
                                 rep=unique(rep.df.all$rep),
                                 continent=unique(rep.df.all$continent),
                                 other_continent=unique(rep.df.all$other_continent),
                                 type=unique(rep.df.all$type)))

rep.df.all<-merge(rep.df.all, all.comb, by=c("year_window", "seed_continent", "species.type", 
                                             "rep", "continent", "other_continent", "type"), all=T)

rep.df.all<-merge(rep.df.all, rep.df.all.sp, by=c("year_window", "seed_continent", "species.type", "rep", "continent", "other_continent"), all.x=T)
rep.df.all.sp[year_window==-1850]
rep.df.all[is.na(N), N:=0]
rep.df.all<-rep.df.all[!is.na(N_SP)]


rep.df.all$net_speciation<-rep.df.all$N/rep.df.all$N_SP
rep.df.all.speciation<-rep.df.all
p1<-ggplot(rep.df.all[between(year_window, -1800, -50)])+
  geom_smooth(aes(x=year_window, y=N, color=species.type), 
              method = "loess", span = 0.1, se = FALSE, linewidth = 1) + 
  #geom_point(aes(x=year_window, y=N, color=species.type)) + 
  scale_color_manual(values = c("Native" = color_native, 
                                "Immigrant" = color_immigrant, 
                                "Isthmus" = color_mid2,
                                "Caribbean" = color_2)) +
  labs(x = "Year", y = "Number of Speciation", 
       color = "Species Type") +
  scale_y_sqrt()+
  theme_bw() +
  theme(
    legend.position = "bottom",
    axis.title = element_text(face = "bold")
  )+facet_wrap(~seed_continent, nrow=2, scale="free")


p2<-ggplot(rep.df.all[between(year_window, -1800, -50)])+
  geom_smooth(aes(x=year_window, y=net_speciation, color=species.type), 
              method = "loess", span = 0.1, se = FALSE, linewidth = 1) + 
  #geom_point(aes(x=year_window, y=N, color=species.type)) + 
  scale_color_manual(values = c("Native" = color_native, 
                                "Immigrant" = color_immigrant, 
                                "Isthmus" = color_mid2,
                                "Caribbean" = color_2)) +
  labs(x = "Year", y = "Speciation / Species", 
       color = "Species Type") +
  scale_y_sqrt()+
  theme_bw() +
  theme(
    legend.position = "bottom",
    axis.title = element_text(face = "bold")
  )+facet_wrap(~continent, nrow=2, scale="free")
p2


ggsave(p1, filename="../Figures/NET/Speciation.origin.NULL.pdf", width=8, height=5)
ggsave(p1, filename="../Figures/NET/Speciation.origin.NULL.png", width=8, height=5, bg="white")

ggsave(p2, filename="../Figures/NET/Net.Speciation.continent.NULL.pdf", width=8, height=5)
ggsave(p2, filename="../Figures/NET/Net.Speciation.continent.NULL.png", width=8, height=5, bg="white")

#Dispersal

year_window_size<-50
rep.df<-readRDS(sprintf("../Data/Tables/N.Dispersal.rep.window.size.%d.NULL.rda", year_window_size))
rep.df.all<-readRDS(sprintf("../Data/Tables/N.Dispersal.all.window.size.%d.rep.NULL.rda", year_window_size))
setorder(rep.df.all, seed_continent, type, rep, year_window)

rep.df.all[year_window==-500]
rep.df.all$continent<-rep.df.all$seed_continent
rep.df.all$other_continent<-ifelse(rep.df.all$continent=="North America", "South America", "North America")
rep.df.all$seed_continent<-factor(rep.df.all$seed_continent, levels=c("North America", "South America"),
                                  labels=c("North American Origin", "South American Origin"))
rep.df.all[,.(N=.N), by=list(seed_continent, type)]

all.comb<-data.table(expand.grid(year_window=unique(rep.df.all$year_window),
                                 seed_continent=unique(rep.df.all$seed_continent),
                                 #species.type=unique(rep.df.all$species.type),
                                 rep=unique(rep.df.all$rep),
                                 continent=unique(rep.df.all$continent),
                                 other_continent=unique(rep.df.all$other_continent),
                                 type=unique(rep.df.all$type)))

rep.df.all<-merge(rep.df.all, all.comb, by=c("year_window", "seed_continent", 
                                             "rep", "continent", "other_continent", "type"), all=T)

rep.df.all<-merge(rep.df.all, rep.df.all.sp, by=c("year_window", "seed_continent", 
                                                  "rep", "continent", "other_continent"), all.x=T)

rep.df.all.sp[year_window==-1850]
rep.df.all[is.na(N), N:=0]
rep.df.all<-rep.df.all[!is.na(N_SP)]


rep.df.all$net_dispersal<-rep.df.all$N/rep.df.all$N_SP

#rep.df.all$disp.type<-"N to S"
rep.df.all[,.(N=.N), by=list(seed_continent, type)]
#rep.df.all[seed_continent=="North American Origin" & type=="Primary Invader", disp.type:="N to S"]
#rep.df.all[seed_continent=="North American Origin" & type=="Secondary Invader", disp.type:="S to N"]
#rep.df.all[seed_continent=="South American Origin" & type=="Primary Invader", disp.type:="S to N"]
#rep.df.all[seed_continent=="South American Origin" & type=="Secondary Invader", disp.type:="N to S"]
table(rep.df.all$disp.type)
custom_colors <- c(
  "Primary Invader" = color_n2s,
  "Secondary Invader" = color_s2n
)
rep.df.all.dispersal<-rep.df.all
p1<-ggplot(rep.df.all[between(year_window, -1800, -50)])+
  geom_smooth(aes(x=year_window, y=N, color=type), 
              method = "loess", span = 0.1, se = FALSE, linewidth = 1) + 
  #geom_point(aes(x=year_window, y=N, color=species.type)) + 
  scale_color_manual(values=custom_colors) +
  labs(x = "Year", y = "Number of Dispersal", 
       color = "Species Type") +
  scale_y_sqrt()+
  theme_bw() +
  theme(
    legend.position = "bottom",
    axis.title = element_text(face = "bold")
  )+facet_wrap(~seed_continent, nrow=2, scale="free")

p1

p2<-ggplot(rep.df.all[between(year_window, -1800, -50)])+
  geom_smooth(aes(x=year_window, y=net_dispersal, color=type), 
              method = "loess", span = 0.1, se = FALSE, linewidth = 1) + 
  #geom_point(aes(x=year_window, y=N, color=species.type)) + 
  scale_color_manual(values=custom_colors) +
  labs(x = "Year", y = "Immigrant persentage", 
       color = "Species Type") +
  scale_y_sqrt()+
  theme_bw() +
  theme(
    legend.position = "bottom",
    axis.title = element_text(face = "bold")
  )+facet_wrap(~continent, nrow=2, scale="free")
p2


ggsave(p1, filename="../Figures/NET/Dispersal.origin.NULL.pdf", width=8, height=5)
ggsave(p1, filename="../Figures/NET/Dispersal.origin.NULL.png", width=8, height=5, bg="white")

ggsave(p2, filename="../Figures/NET/Net.Dispersal.continent.NULL.pdf", width=8, height=5)
ggsave(p2, filename="../Figures/NET/Net.Dispersal.continent.NULL.png", width=8, height=5, bg="white")



###no outliers
#Net diversification rate
year_window_size<-50
rep.df.sp<-readRDS(sprintf("../Data/Tables/N.net.diversity.rep.window.size.%d.NULL.no.outliers.rda", year_window_size))
rep.df.all.sp<-readRDS(sprintf("../Data/Tables/N.net.diversity.all.window.size.%d.rep.NULL.no.outliers.rda", year_window_size))
setorder(rep.df.all.sp, seed_continent, type, rep, year_window)
rep.df.all.sp[, net_div_rate := (N_SP-shift(N_SP)) / shift(N_SP), by = .(seed_continent, type, rep)]

rep.df.all.sp$continent<-rep.df.all.sp$seed_continent
rep.df.all.sp$other_continent<-ifelse(rep.df.all.sp$continent=="North America", "South America", "North America")
rep.df.all.sp[continent==other_continent]
rep.df.all.sp[type=="Immigrant", continent:=other_continent]
rep.df.all.sp$seed_continent<-factor(rep.df.all.sp$seed_continent, levels=c("North America", "South America"),
                                     labels=c("North American Origin", "South American Origin"))
rep.df.all.sp$type<-factor(rep.df.all.sp$type, levels=c("Native", "Immigrant"),
                           labels=c("Native", "Immigrant"))

p1<-ggplot(rep.df.all.sp[between(year_window, -1800, -50)])+
  geom_smooth(aes(x=year_window, y=net_div_rate, color=type), 
              method = "loess", span = 0.1, se = T, linewidth = 1) + 
  geom_point(aes(x=year_window, y=net_div_rate, color=type)) + 
  scale_color_manual(values = c("Native" = color_native, "Immigrant" = color_immigrant)) +
  labs(x = "Year", y = "Net Diversification Rate", 
       color = "Species Type") +
  theme_classic() +
  theme(
    legend.position = "bottom",
    axis.title = element_text(face = "bold")
  )+facet_wrap(~seed_continent, nrow=2)
p1
p2<-ggplot(rep.df.all.sp[between(year_window, -1800, -50)])+
  geom_smooth(aes(x=year_window, y=net_div_rate, color=type), 
              method = "loess", span = 0.1, se = T, linewidth = 1) + 
  #geom_point(aes(x=year_window, y=net_div_rate, color=type)) + 
  scale_color_manual(values = c("Native" = color_native, "Immigrant" = color_immigrant)) +
  labs(x = "Year", y = "Net Diversification Rate", 
       color = "Species Type") +
  theme_classic() +
  theme(
    legend.position = "bottom",
    axis.title = element_text(face = "bold")
  )+facet_wrap(~continent, nrow=2)
p2
p3<-ggplot(rep.df.all.sp[between(year_window, -1800, -50)])+
  geom_smooth(aes(x=year_window, y=N_SP, color=type), 
              method = "loess", span = 0.1, se = T, linewidth = 1) + 
  #geom_point(aes(x=year_window, y=N_SP, color=type)) + 
  scale_color_manual(values = c("Native" = color_native, "Immigrant" = color_immigrant)) +
  labs(x = "Year", y = "Species Richness", 
       color = "Species Type") +
  theme_classic() +
  theme(
    legend.position = "bottom",
    axis.title = element_text(face = "bold")
  )+facet_wrap(~continent, nrow=2)
p3

ggsave(p1, filename="../Figures/NET/Net.Diversification.Rate.origin.NULL.no.outliers.pdf", width=8, height=5)
ggsave(p1, filename="../Figures/NET/Net.Diversification.Rate.origin.NULL.no.outliers.png", width=8, height=5, bg="white")

ggsave(p2, filename="../Figures/NET/Net.Diversification.Rate.continent.NULL.no.outliers.pdf", width=8, height=5)
ggsave(p2, filename="../Figures/NET/Net.Diversification.Rate.continent.NULL.no.outliers.png", width=8, height=5, bg="white")

ggsave(p3, filename="../Figures/NET/N.Species.continent.NULL.pdf", width=8, height=5)
ggsave(p3, filename="../Figures/NET/N.Species.continent.NULL.png", width=8, height=5, bg="white")


#Extinction
year_window_size<-50
rep.df<-readRDS(sprintf("../Data/Tables/N.Extinction.rep.window.size.%d.NULL.no.outliers.rda", year_window_size))
rep.df.all<-readRDS(sprintf("../Data/Tables/N.Extinction.all.window.size.%d.rep.NULL.no.outliers.rda", year_window_size))
colnames(rep.df.all.sp)[3]<-"species.type"


setorder(rep.df.all, seed_continent, type, species.type, rep, year_window)
table(rep.df.all$species.type)

rep.df.all[year_window==-500]
rep.df.all$species.type<-factor(rep.df.all$species.type, 
                                levels=c("Native", "Immigrant", "Isthmus", "Caribbean"))
rep.df.all$continent<-rep.df.all$seed_continent
rep.df.all$other_continent<-ifelse(rep.df.all$continent=="North America", "South America", "North America")
rep.df.all[species.type=="Immigrant", continent:=other_continent]
rep.df.all[species.type=="Native", continent:=seed_continent]
rep.df.all[species.type=="Isthmus", continent:="Isthmus"]
rep.df.all[species.type=="Caribbean", continent:="Caribbean"]

rep.df.all$seed_continent<-factor(rep.df.all$seed_continent, levels=c("North America", "South America"),
                                  labels=c("North American Origin", "South American Origin"))
table(rep.df.all$type)
all.comb<-data.table(expand.grid(year_window=unique(rep.df.all$year_window),
                                 seed_continent=unique(rep.df.all$seed_continent),
                                 species.type=unique(rep.df.all$species.type),
                                 rep=unique(rep.df.all$rep),
                                 continent=unique(rep.df.all$continent),
                                 other_continent=unique(rep.df.all$other_continent),
                                 type=unique(rep.df.all$type), stringsAsFactors = F))
rep.df.all<-merge(rep.df.all, all.comb, by=c("year_window", "seed_continent", "species.type", 
                                             "rep", "continent", "other_continent", "type"), all=T)
rep.df.all[is.na(type)]
rep.df.all<-merge(rep.df.all, rep.df.all.sp, by=c("year_window", "seed_continent", "species.type", 
                                                  "rep", "continent", "other_continent"), all=T)
rep.df.all[is.na(N), N:=0]
rep.df.all<-rep.df.all[!is.na(N_SP)]

rep.df.all$net_extinction<-rep.df.all$N/rep.df.all$N_SP
rep.df.all[species.type %in% c("Native", "Immigrant") & continent %in% c("North America", "South America")]
rep.df.all.extinction<-rep.df.all[species.type %in% c("Native", "Immigrant") & 
                                    continent %in% c("North America", "South America") &
                                    !is.na(type)]

p1<-ggplot(rep.df.all[between(year_window, -1800, -50) & type!="Local.Extinction"])+
  geom_smooth(aes(x=year_window, y=N, color=species.type), 
              method = "loess", span = 0.1, se = FALSE, linewidth = 1) + 
  #geom_point(aes(x=year_window, y=N, color=species.type)) + 
  scale_color_manual(values = c("Native" = color_native, 
                                "Immigrant" = color_immigrant, 
                                "Isthmus" = color_mid2,
                                "Caribbean" = color_2)) +
  labs(x = "Year", y = "Number of Extinction", 
       color = "Species Type") +
  scale_y_sqrt()+
  theme_bw() +
  theme(
    legend.position = "bottom",
    axis.title = element_text(face = "bold")
  )+facet_wrap(~seed_continent, nrow=2, scale="free")

p1

rep.df.all.extinction$type<-factor(rep.df.all.extinction$type, levels=c("Extinction", "Local.Extinction"),
                                   labels=c("Extinction", "Local Extinction"))
p2<-ggplot(rep.df.all.extinction[between(year_window, -1800, -50) & 
                                   continent %in% c("North America", "South America")])+
  geom_smooth(aes(x=year_window, y=net_extinction, color=species.type), 
              method = "loess", span = 0.1, se = FALSE, linewidth = 1) + 
  #geom_point(aes(x=year_window, y=N, color=species.type)) + 
  scale_color_manual(values = c("Native" = color_native, 
                                "Immigrant" = color_immigrant, 
                                "Isthmus" = color_mid2,
                                "Caribbean" = color_2)) +
  labs(x = "Year", y = "Extinction / Species", 
       color = "Species Type") +
  scale_y_sqrt()+
  theme_bw() +
  theme(
    legend.position = "bottom",
    axis.title = element_text(face = "bold")
  )+facet_grid(type~continent, scale="free")
p2

ggsave(p1, filename="../Figures/NET/Extinction.origin.NULL.no.outliers.pdf", width=8, height=5)
ggsave(p1, filename="../Figures/NET/Extinction.origin.NULL.no.outliers.png", width=8, height=5, bg="white")

ggsave(p2, filename="../Figures/NET/Net.Extinction.continent.NULL.no.outliers.pdf", width=8, height=6)
ggsave(p2, filename="../Figures/NET/Net.Extinction.continent.NULL.no.outliers.png", width=8, height=6, bg="white")


#Speciation
year_window_size<-50
rep.df<-readRDS(sprintf("../Data/Tables/N.Speciation.rep.window.size.%d.NULL.no.outliers.rda", year_window_size))
rep.df.all<-readRDS(sprintf("../Data/Tables/N.Speciation.all.window.size.%d.rep.NULL.no.outliers.rda", year_window_size))

setorder(rep.df.all, seed_continent, type, species.type, rep, year_window)
table(rep.df.all$species.type)

rep.df.all[year_window==-500]
rep.df.all$species.type<-factor(rep.df.all$species.type, levels=c("Native", "Immigrant", "Isthmus", "Caribbean"))
rep.df.all$continent<-rep.df.all$seed_continent
rep.df.all$other_continent<-ifelse(rep.df.all$continent=="North America", "South America", "North America")
rep.df.all[species.type=="Immigrant", continent:=other_continent]
rep.df.all[species.type=="Native", continent:=seed_continent]
rep.df.all[species.type=="Isthmus", continent:="Isthmus"]
rep.df.all[species.type=="Caribbean", continent:="Caribbean"]
rep.df.all$seed_continent<-factor(rep.df.all$seed_continent, levels=c("North America", "South America"),
                                  labels=c("North American Origin", "South American Origin"))

all.comb<-data.table(expand.grid(year_window=unique(rep.df.all$year_window),
                                 seed_continent=unique(rep.df.all$seed_continent),
                                 species.type=unique(rep.df.all$species.type),
                                 rep=unique(rep.df.all$rep),
                                 continent=unique(rep.df.all$continent),
                                 other_continent=unique(rep.df.all$other_continent),
                                 type=unique(rep.df.all$type)))

rep.df.all<-merge(rep.df.all, all.comb, by=c("year_window", "seed_continent", "species.type", 
                                             "rep", "continent", "other_continent", "type"), all=T)

rep.df.all<-merge(rep.df.all, rep.df.all.sp, by=c("year_window", "seed_continent", "species.type", "rep", "continent", "other_continent"), all.x=T)
rep.df.all.sp[year_window==-1850]
rep.df.all[is.na(N), N:=0]
rep.df.all<-rep.df.all[!is.na(N_SP)]


rep.df.all$net_speciation<-rep.df.all$N/rep.df.all$N_SP
rep.df.all.speciation<-rep.df.all
p1<-ggplot(rep.df.all[between(year_window, -1800, -50)])+
  geom_smooth(aes(x=year_window, y=N, color=species.type), 
              method = "loess", span = 0.1, se = FALSE, linewidth = 1) + 
  #geom_point(aes(x=year_window, y=N, color=species.type)) + 
  scale_color_manual(values = c("Native" = color_native, 
                                "Immigrant" = color_immigrant, 
                                "Isthmus" = color_mid2,
                                "Caribbean" = color_2)) +
  labs(x = "Year", y = "Number of Speciation", 
       color = "Species Type") +
  scale_y_sqrt()+
  theme_bw() +
  theme(
    legend.position = "bottom",
    axis.title = element_text(face = "bold")
  )+facet_wrap(~seed_continent, nrow=2, scale="free")


p2<-ggplot(rep.df.all[between(year_window, -1800, -50)])+
  geom_smooth(aes(x=year_window, y=net_speciation, color=species.type), 
              method = "loess", span = 0.1, se = FALSE, linewidth = 1) + 
  #geom_point(aes(x=year_window, y=N, color=species.type)) + 
  scale_color_manual(values = c("Native" = color_native, 
                                "Immigrant" = color_immigrant, 
                                "Isthmus" = color_mid2,
                                "Caribbean" = color_2)) +
  labs(x = "Year", y = "Speciation / Species", 
       color = "Species Type") +
  scale_y_sqrt()+
  theme_bw() +
  theme(
    legend.position = "bottom",
    axis.title = element_text(face = "bold")
  )+facet_wrap(~continent, nrow=2, scale="free")
p2


ggsave(p1, filename="../Figures/NET/Speciation.origin.NULL.no.outliers.pdf", width=8, height=5)
ggsave(p1, filename="../Figures/NET/Speciation.origin.NULL.no.outliers.png", width=8, height=5, bg="white")

ggsave(p2, filename="../Figures/NET/Net.Speciation.continent.NULL.no.outliers.pdf", width=8, height=5)
ggsave(p2, filename="../Figures/NET/Net.Speciation.continent.NULL.no.outliers.png", width=8, height=5, bg="white")

#Dispersal

year_window_size<-50
rep.df<-readRDS(sprintf("../Data/Tables/N.Dispersal.rep.window.size.%d.NULL.no.outliers.rda", year_window_size))
rep.df.all<-readRDS(sprintf("../Data/Tables/N.Dispersal.all.window.size.%d.rep.NULL.no.outliers.rda", year_window_size))
setorder(rep.df.all, seed_continent, type, rep, year_window)

rep.df.all[year_window==-500]
rep.df.all$continent<-rep.df.all$seed_continent
rep.df.all$other_continent<-ifelse(rep.df.all$continent=="North America", "South America", "North America")
rep.df.all$seed_continent<-factor(rep.df.all$seed_continent, levels=c("North America", "South America"),
                                  labels=c("North American Origin", "South American Origin"))
rep.df.all[,.(N=.N), by=list(seed_continent, type)]

all.comb<-data.table(expand.grid(year_window=unique(rep.df.all$year_window),
                                 seed_continent=unique(rep.df.all$seed_continent),
                                 #species.type=unique(rep.df.all$species.type),
                                 rep=unique(rep.df.all$rep),
                                 continent=unique(rep.df.all$continent),
                                 other_continent=unique(rep.df.all$other_continent),
                                 type=unique(rep.df.all$type)))

rep.df.all<-merge(rep.df.all, all.comb, by=c("year_window", "seed_continent", 
                                             "rep", "continent", "other_continent", "type"), all=T)

rep.df.all<-merge(rep.df.all, rep.df.all.sp, by=c("year_window", "seed_continent", 
                                                  "rep", "continent", "other_continent"), all.x=T)

rep.df.all.sp[year_window==-1850]
rep.df.all[is.na(N), N:=0]
rep.df.all<-rep.df.all[!is.na(N_SP)]


rep.df.all$net_dispersal<-rep.df.all$N/rep.df.all$N_SP

#rep.df.all$disp.type<-"N to S"
rep.df.all[,.(N=.N), by=list(seed_continent, type)]
#rep.df.all[seed_continent=="North American Origin" & type=="Primary Invader", disp.type:="N to S"]
#rep.df.all[seed_continent=="North American Origin" & type=="Secondary Invader", disp.type:="S to N"]
#rep.df.all[seed_continent=="South American Origin" & type=="Primary Invader", disp.type:="S to N"]
#rep.df.all[seed_continent=="South American Origin" & type=="Secondary Invader", disp.type:="N to S"]
table(rep.df.all$disp.type)
custom_colors <- c(
  "Primary Invader" = color_n2s,
  "Secondary Invader" = color_s2n
)
rep.df.all.dispersal<-rep.df.all
p1<-ggplot(rep.df.all[between(year_window, -1800, -50)])+
  geom_smooth(aes(x=year_window, y=N, color=type), 
              method = "loess", span = 0.1, se = FALSE, linewidth = 1) + 
  #geom_point(aes(x=year_window, y=N, color=species.type)) + 
  scale_color_manual(values=custom_colors) +
  labs(x = "Year", y = "Number of Dispersal", 
       color = "Species Type") +
  scale_y_sqrt()+
  theme_bw() +
  theme(
    legend.position = "bottom",
    axis.title = element_text(face = "bold")
  )+facet_wrap(~seed_continent, nrow=2, scale="free")

p1

p2<-ggplot(rep.df.all[between(year_window, -1800, -50)])+
  geom_smooth(aes(x=year_window, y=net_dispersal, color=type), 
              method = "loess", span = 0.1, se = FALSE, linewidth = 1) + 
  #geom_point(aes(x=year_window, y=N, color=species.type)) + 
  scale_color_manual(values=custom_colors) +
  labs(x = "Year", y = "Immigrant persentage", 
       color = "Species Type") +
  scale_y_sqrt()+
  theme_bw() +
  theme(
    legend.position = "bottom",
    axis.title = element_text(face = "bold")
  )+facet_wrap(~continent, nrow=2, scale="free")
p2


ggsave(p1, filename="../Figures/NET/Dispersal.origin.NULL.no.outliers.pdf", width=8, height=5)
ggsave(p1, filename="../Figures/NET/Dispersal.origin.NULL.no.outliers.png", width=8, height=5, bg="white")

ggsave(p2, filename="../Figures/NET/Net.Dispersal.continent.NULL.no.outliers.pdf", width=8, height=5)
ggsave(p2, filename="../Figures/NET/Net.Dispersal.continent.NULL.no.outliers.png", width=8, height=5, bg="white")

